#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::method_resolution::{
    lookup_builtin_method_sig, lookup_named_method_sig as shared_lookup_named_method_sig,
};

impl Checker {
    fn record_method_call_receiver_kind(&mut self, span: &Span, kind: MethodCallReceiverKind) {
        self.method_call_receiver_kinds
            .insert(SpanKey::from(span), kind);
    }

    pub(super) fn strip_module_prefix<'a>(&self, name: &'a str) -> Option<&'a str> {
        let dot = name.find('.')?;
        if self.modules.contains(&name[..dot]) {
            Some(&name[dot + 1..])
        } else {
            None
        }
    }

    /// Look up a type definition, handling module-qualified names like `json.Value`.
    pub(super) fn lookup_type_def(&self, name: &str) -> Option<TypeDef> {
        self.type_defs
            .get(name)
            .or_else(|| {
                self.strip_module_prefix(name)
                    .and_then(|u| self.type_defs.get(u))
            })
            .cloned()
    }

    /// Look up a type definition mutably, handling module-qualified names.
    pub(super) fn lookup_type_def_mut(&mut self, name: &str) -> Option<&mut TypeDef> {
        if self.type_defs.contains_key(name) {
            return self.type_defs.get_mut(name);
        }
        let unqualified = self.strip_module_prefix(name)?;
        self.type_defs.get_mut(unqualified)
    }

    /// Look up a non-builtin named method via `type_defs` first, then `fn_sigs`.
    pub(super) fn lookup_named_method_sig(
        &self,
        type_name: &str,
        type_args: &[Ty],
        method: &str,
    ) -> Option<FnSig> {
        shared_lookup_named_method_sig(&self.type_defs, &self.fn_sigs, type_name, type_args, method)
    }

    /// Try to resolve a method call on a named type via `type_defs` and `fn_sigs`.
    ///
    /// Used as a fallback from hardcoded handle-type dispatch tables so that
    /// methods added via `.hew` impl blocks work without updating the tables.
    pub(super) fn try_resolve_named_method(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        args: &[CallArg],
        _span: &Span,
    ) -> Option<Ty> {
        let Ty::Named {
            name,
            args: type_args,
        } = receiver_ty
        else {
            return None;
        };
        let sig = self.lookup_named_method_sig(name, type_args, method)?;
        for (i, arg) in args.iter().enumerate() {
            if let Some(param_ty) = sig.params.get(i) {
                let (expr, sp) = arg.expr();
                self.check_against(expr, sp, param_ty);
            }
        }
        Some(sig.return_type)
    }

    pub(super) fn check_named_method_fallback(
        &mut self,
        receiver_ty: &Ty,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
        type_display_name: &str,
    ) -> Ty {
        if let Some(ty) = self.try_resolve_named_method(receiver_ty, method_name, args, span) {
            if let Ty::Named { name, .. } = receiver_ty {
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::NamedTypeInstance {
                        type_name: name.clone(),
                    },
                );
            }
            return ty;
        }

        self.report_error(
            TypeErrorKind::UndefinedMethod,
            span,
            format!("no method `{method_name}` on {type_display_name}"),
        );
        Ty::Error
    }

    pub(super) fn check_stream_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let Some(inner) = self.validate_stream_sink_element_type(
            type_args,
            BuiltinNamedType::Stream.canonical_name(),
            method,
            span,
        ) else {
            return Ty::Error;
        };
        if method == "decode" {
            return self.report_unlowerable_stream_codec_boundary(
                BuiltinNamedType::Stream.canonical_name(),
                &inner,
                method,
                span,
            );
        }
        let receiver_ty = Ty::stream(inner.clone());
        let Some(sig) = lookup_builtin_method_sig(&receiver_ty, method) else {
            self.report_error(
                TypeErrorKind::UndefinedMethod,
                span,
                format!("no method `{method}` on `Stream<{}>`", inner.user_facing()),
            );
            return Ty::Error;
        };
        match method {
            "next" | "close" => sig.return_type,
            "lines" => {
                if inner != Ty::String {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`lines()` is only supported on `Stream<String>`, \
                             not `Stream<{}>`",
                            inner.user_facing()
                        ),
                    );
                }
                sig.return_type
            }
            "collect" => {
                if inner != Ty::String {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`collect()` is only supported on `Stream<String>`, \
                             not `Stream<{}>`",
                            inner.user_facing()
                        ),
                    );
                }
                sig.return_type
            }
            "chunks" | "take" | "map" | "filter" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    if let Some(param_ty) = sig.params.first() {
                        self.check_against(expr, sp, param_ty);
                    }
                }
                sig.return_type
            }
            _ => {
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Stream<{}>`", inner.user_facing()),
                );
                Ty::Error
            }
        }
    }

    pub(super) fn validate_stream_sink_element_type(
        &mut self,
        type_args: &[Ty],
        type_name: &str,
        method_name: &str,
        span: &Span,
    ) -> Option<Ty> {
        let _ = method_name;
        let inner = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        // Reject concrete element types that lack runtime
        // implementations. Only String and bytes are supported;
        // type variables and Ty::Error pass through (Error
        // preserves the original diagnostic instead of masking it).
        let is_supported = matches!(&inner, Ty::String | Ty::Bytes | Ty::Var(_) | Ty::Error);
        if !is_supported {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`{type_name}<{}>` is not supported; \
                     {type_name}<T> is currently only implemented for String and bytes",
                    inner.user_facing()
                ),
            );
            return None;
        }
        Some(inner)
    }

    fn report_unlowerable_stream_codec_boundary(
        &mut self,
        type_name: &str,
        inner: &Ty,
        method: &str,
        span: &Span,
    ) -> Ty {
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "`{method}()` is not available on `{type_name}<{}>` yet; lowering/runtime support is not implemented",
                inner.user_facing()
            ),
        );
        Ty::Error
    }

    pub(super) fn check_string_method(
        &mut self,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        match method {
            "len" => Ty::I64,
            "contains" | "starts_with" | "ends_with" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                Ty::Bool
            }
            "is_digit" | "is_alpha" | "is_alphanumeric" | "is_empty" => {
                self.check_arity(args, 0, &format!("`String::{method}`"), span);
                Ty::Bool
            }
            "to_uppercase" | "to_lowercase" | "to_upper" | "to_lower" | "trim" | "clone" => {
                Ty::String
            }
            "replace" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                Ty::String
            }
            "split" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                self.make_vec_type(Ty::String, span)
            }
            "lines" => {
                self.check_arity(args, 0, "`String::lines`", span);
                self.make_vec_type(Ty::String, span)
            }
            "find" | "index_of" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                Ty::I64
            }
            "slice" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                Ty::String
            }
            "repeat" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                Ty::String
            }
            "char_at" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                Ty::I64
            }
            "chars" => {
                self.check_arity(args, 0, "`String::chars`", span);
                self.make_vec_type(Ty::Char, span)
            }
            _ => {
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on string"),
                );
                Ty::Error
            }
        }
    }

    /// Reject `Rc<T>` (or types transitively containing `Rc<T>`) as a
    /// collection element type.  The runtime does not drop owned-type
    /// collection elements, so storing `Rc<T>` causes refcount leaks.
    pub(super) fn reject_rc_collection_element(
        &mut self,
        container: &str,
        elem_ty: &Ty,
        span: &Span,
    ) {
        let resolved = self.subst.resolve(elem_ty);
        let mut visiting = HashSet::new();
        if ty_contains_rc_deep(&resolved, &self.type_defs, &mut visiting) {
            self.report_error(
                TypeErrorKind::UnsafeCollectionElement,
                span,
                format!(
                    "`{container}` cannot hold `{}`; Rc<T> in collections is not yet \
                     supported (runtime does not track Rc ownership for collection elements)",
                    resolved.user_facing()
                ),
            );
        }
    }

    pub(super) fn validate_hashset_element_type(&mut self, elem_ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(elem_ty);
        if matches!(
            resolved,
            Ty::Var(_) | Ty::Error | Ty::String | Ty::I64 | Ty::U64
        ) {
            return true;
        }

        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "HashSet<{}> is not supported; only HashSet<String> and 64-bit integer element types are currently supported",
                resolved.user_facing()
            ),
        );
        false
    }

    fn instantiate_type_def_member(&self, ty: &Ty, type_params: &[String], type_args: &[Ty]) -> Ty {
        type_params
            .iter()
            .zip(type_args.iter())
            .fold(ty.clone(), |instantiated, (param, arg)| {
                self.substitute_named_param(&instantiated, param, arg)
            })
    }

    /// Detect arrays that survive inside the stored shape of a Vec element.
    /// Nested collection validation runs separately so this walk stays focused
    /// on structural payloads like tuples, builtin wrappers, ranges, and
    /// user-defined types.
    fn vec_element_contains_structural_array(
        &self,
        ty: &Ty,
        visiting: &mut HashSet<String>,
    ) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Array(_, _) => true,
            Ty::Tuple(elems) => elems
                .iter()
                .any(|elem| self.vec_element_contains_structural_array(elem, visiting)),
            // Builtin wrappers inline their payloads but do not live in
            // `type_defs`, so recurse through their type arguments directly.
            Ty::Named { name, args } if matches!(name.as_str(), "Range" | "Option" | "Result") => {
                args.iter()
                    .any(|arg| self.vec_element_contains_structural_array(arg, visiting))
            }
            Ty::Named { name, args } => {
                let Some(type_def) = self.lookup_type_def(name) else {
                    return false;
                };
                if visiting.contains(type_def.name.as_str()) {
                    return false;
                }

                visiting.insert(type_def.name.clone());
                let result = type_def.fields.values().any(|field_ty| {
                    let field_ty =
                        self.instantiate_type_def_member(field_ty, &type_def.type_params, args);
                    self.vec_element_contains_structural_array(&field_ty, visiting)
                }) || type_def.variants.values().any(|variant| match variant {
                    VariantDef::Unit => false,
                    VariantDef::Tuple(tys) => tys.iter().any(|ty| {
                        let ty = self.instantiate_type_def_member(ty, &type_def.type_params, args);
                        self.vec_element_contains_structural_array(&ty, visiting)
                    }),
                    VariantDef::Struct(fields) => fields.iter().any(|(_, ty)| {
                        let ty = self.instantiate_type_def_member(ty, &type_def.type_params, args);
                        self.vec_element_contains_structural_array(&ty, visiting)
                    }),
                });
                visiting.remove(type_def.name.as_str());
                result
            }
            _ => false,
        }
    }

    pub(super) fn validate_vec_element_type(&mut self, elem_ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(elem_ty);
        if matches!(resolved, Ty::Var(_) | Ty::Error) {
            return true;
        }

        if !self.validate_concrete_collection_types(&resolved, span) {
            return false;
        }

        let mut visiting = HashSet::new();
        if self.vec_element_contains_structural_array(&resolved, &mut visiting) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "Vec<{}> is not supported; vec lowering does not support array element types yet",
                    resolved.user_facing()
                ),
            );
            return false;
        }

        true
    }

    pub(super) fn validate_concrete_vec_type(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Named { name, args } => {
                if name == "Vec" && args.len() == 1 {
                    return self.validate_vec_element_type(&args[0], span);
                }
                args.iter()
                    .all(|arg| self.validate_concrete_vec_type(arg, span))
            }
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.validate_concrete_vec_type(elem, span)),
            Ty::Array(elem, _) | Ty::Slice(elem) => self.validate_concrete_vec_type(elem, span),
            Ty::Function { params, ret } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_vec_type(param, span))
                    && self.validate_concrete_vec_type(ret, span)
            }
            Ty::Closure {
                params,
                ret,
                captures,
            } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_vec_type(param, span))
                    && self.validate_concrete_vec_type(ret, span)
                    && captures
                        .iter()
                        .all(|capture| self.validate_concrete_vec_type(capture, span))
            }
            Ty::Pointer { pointee, .. } => self.validate_concrete_vec_type(pointee, span),
            Ty::TraitObject { traits } => traits.iter().all(|bound| {
                bound
                    .args
                    .iter()
                    .all(|arg| self.validate_concrete_vec_type(arg, span))
            }),
            _ => true,
        }
    }

    pub(super) fn validate_concrete_hashset_type(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Named { name, args } => {
                if name == "HashSet" && args.len() == 1 {
                    return self.validate_hashset_element_type(&args[0], span);
                }
                args.iter()
                    .all(|arg| self.validate_concrete_hashset_type(arg, span))
            }
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.validate_concrete_hashset_type(elem, span)),
            Ty::Array(elem, _) | Ty::Slice(elem) => self.validate_concrete_hashset_type(elem, span),
            Ty::Function { params, ret } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_hashset_type(param, span))
                    && self.validate_concrete_hashset_type(ret, span)
            }
            Ty::Closure {
                params,
                ret,
                captures,
            } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_hashset_type(param, span))
                    && self.validate_concrete_hashset_type(ret, span)
                    && captures
                        .iter()
                        .all(|capture| self.validate_concrete_hashset_type(capture, span))
            }
            Ty::Pointer { pointee, .. } => self.validate_concrete_hashset_type(pointee, span),
            Ty::TraitObject { traits } => traits.iter().all(|bound| {
                bound
                    .args
                    .iter()
                    .all(|arg| self.validate_concrete_hashset_type(arg, span))
            }),
            _ => true,
        }
    }

    pub(super) fn validate_concrete_collection_types(&mut self, ty: &Ty, span: &Span) -> bool {
        let hashset_ok = self.validate_concrete_hashset_type(ty, span);
        let vec_ok = self.validate_concrete_vec_type(ty, span);
        hashset_ok && vec_ok
    }

    pub(super) fn make_vec_type(&mut self, elem_ty: Ty, span: &Span) -> Ty {
        let ty = Ty::Named {
            name: "Vec".to_string(),
            args: vec![elem_ty],
        };
        self.validate_concrete_vec_type(&ty, span);
        ty
    }

    pub(super) fn check_hashmap_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let key_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let val_ty = type_args
            .get(1)
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        match method {
            "insert" => {
                self.check_arity(args, 2, "`HashMap::insert`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &key_ty);
                }
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &val_ty);
                }
                self.reject_rc_collection_element("HashMap", &key_ty, span);
                self.reject_rc_collection_element("HashMap", &val_ty, span);
                Ty::Unit
            }
            "get" => {
                self.check_arity(args, 1, "`HashMap::get`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &key_ty);
                }
                Ty::option(val_ty)
            }
            "remove" => {
                self.check_arity(args, 1, "`HashMap::remove`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &key_ty);
                }
                Ty::Bool
            }
            "contains_key" => {
                self.check_arity(args, 1, "`HashMap::contains_key`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &key_ty);
                }
                Ty::Bool
            }
            "keys" => {
                self.check_arity(args, 0, "`HashMap::keys`", span);
                self.make_vec_type(key_ty, span)
            }
            "values" => {
                self.check_arity(args, 0, "`HashMap::values`", span);
                self.make_vec_type(val_ty, span)
            }
            "clone" => {
                self.check_arity(args, 0, "`HashMap::clone`", span);
                Ty::Named {
                    name: "HashMap".to_string(),
                    args: vec![key_ty.clone(), val_ty.clone()],
                }
            }
            "len" => Ty::I64,
            "is_empty" => Ty::Bool,
            _ => {
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on HashMap"),
                );
                Ty::Error
            }
        }
    }

    pub(super) fn check_hashset_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let elem_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        match method {
            "insert" => {
                self.check_arity(args, 1, "`HashSet::insert`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                self.reject_rc_collection_element("HashSet", &elem_ty, span);
                let resolved = self.subst.resolve(&elem_ty);
                let mut visiting = HashSet::new();
                if ty_contains_rc_deep(&resolved, &self.type_defs, &mut visiting) {
                    return Ty::Error;
                }
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                Ty::Bool
            }
            "contains" | "remove" => {
                self.check_arity(args, 1, &format!("`HashSet::{method}`"), span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                Ty::Bool
            }
            "clone" => {
                self.check_arity(args, 0, "`HashSet::clone`", span);
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                Ty::Named {
                    name: "HashSet".to_string(),
                    args: vec![elem_ty.clone()],
                }
            }
            "len" => {
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                Ty::I64
            }
            "is_empty" => {
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                Ty::Bool
            }
            "clear" => {
                if !self.validate_hashset_element_type(&elem_ty, span) {
                    return Ty::Error;
                }
                Ty::Unit
            }
            _ => {
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on HashSet"),
                );
                Ty::Error
            }
        }
    }

    pub(super) fn check_rc_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let inner_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        match method {
            // rc.clone() increments the reference count and returns a new Rc<T>
            "clone" => {
                self.check_arity(args, 0, "`Rc::clone`", span);
                Ty::rc(inner_ty)
            }
            // rc.get() copies the inner value out of the Rc.
            // `LoadOp` performs a bitwise copy, which is only sound for `Copy`
            // types (no ownership to duplicate).  For non-Copy `T`, callers
            // share access via `rc.clone()` instead.
            "get" => {
                self.check_arity(args, 0, "`Rc::get`", span);
                if !self
                    .registry
                    .implements_marker(&inner_ty, MarkerTrait::Copy)
                {
                    self.report_error(
                        TypeErrorKind::BoundsNotSatisfied,
                        span,
                        format!(
                            "`Rc::get` requires `T: Copy`; `{}` is not `Copy` — \
                             use `rc.clone()` to share the reference instead",
                            inner_ty.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                inner_ty
            }
            // rc.strong_count() returns the current reference count as i64
            "strong_count" => {
                self.check_arity(args, 0, "`Rc::strong_count`", span);
                Ty::I64
            }
            _ => {
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Rc<{}>`", inner_ty.user_facing()),
                );
                Ty::Error
            }
        }
    }

    #[allow(clippy::too_many_lines, reason = "Vec has many methods to type-check")]
    pub(super) fn check_vec_method(
        &mut self,
        type_args: &[Ty],
        receiver_ty: &Ty,
        resolved: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let elem_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let elem_ty_before = self.subst.resolve(&elem_ty);
        let mut elem_ty_before_visiting = HashSet::new();
        let elem_ty_before_has_structural_array = self
            .vec_element_contains_structural_array(&elem_ty_before, &mut elem_ty_before_visiting);
        let result = match method {
            "push" => {
                self.check_arity(args, 1, "`Vec::push`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                Ty::Unit
            }
            "pop" => {
                self.check_arity(args, 0, "`Vec::pop`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                elem_ty.clone()
            }
            "len" => Ty::I64,
            "get" | "remove" => {
                self.check_arity(args, 1, &format!("`Vec::{method}`"), span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                elem_ty.clone()
            }
            "contains" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                Ty::Bool
            }
            "is_empty" => Ty::Bool,
            "clear" => {
                self.check_arity(args, 0, "`Vec::clear`", span);
                Ty::Unit
            }
            "clone" => {
                self.check_arity(args, 0, "`Vec::clone`", span);
                resolved.clone()
            }
            "set" => {
                if let Some(idx) = args.first() {
                    let (expr, sp) = idx.expr();
                    self.check_against(expr, sp, &Ty::I64);
                }
                if let Some(val) = args.get(1) {
                    let (expr, sp) = val.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                Ty::Unit
            }
            "append" | "extend" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, receiver_ty);
                }
                self.reject_rc_collection_element("Vec", &elem_ty, span);
                Ty::Unit
            }
            "join" => {
                self.check_arity(args, 1, "`Vec::join`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                if elem_ty != Ty::String {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`Vec::join` is only available on Vec<String>, not Vec<{}>",
                            elem_ty.user_facing()
                        ),
                    );
                }
                Ty::String
            }
            "map" => {
                self.check_arity(args, 1, "`Vec::map`", span);
                let ret_ty = Ty::Var(TypeVar::fresh());
                let expected_fn = Ty::Function {
                    params: vec![elem_ty.clone()],
                    ret: Box::new(ret_ty.clone()),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_ret = self.subst.resolve(&ret_ty);
                self.make_vec_type(resolved_ret, span)
            }
            "filter" => {
                self.check_arity(args, 1, "`Vec::filter`", span);
                let expected_fn = Ty::Function {
                    params: vec![elem_ty.clone()],
                    ret: Box::new(Ty::Bool),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                resolved.clone()
            }
            "fold" => {
                self.check_arity(args, 2, "`Vec::fold`", span);
                let acc_ty = if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp)
                } else {
                    Ty::Var(TypeVar::fresh())
                };
                let expected_fn = Ty::Function {
                    params: vec![acc_ty.clone(), elem_ty.clone()],
                    ret: Box::new(acc_ty.clone()),
                };
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                self.subst.resolve(&acc_ty)
            }
            _ => {
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on Vec"),
                );
                Ty::Error
            }
        };
        let elem_ty_after = self.subst.resolve(&elem_ty);
        let mut elem_ty_after_visiting = HashSet::new();
        let elem_ty_after_has_structural_array =
            self.vec_element_contains_structural_array(&elem_ty_after, &mut elem_ty_after_visiting);
        if elem_ty_after_has_structural_array && !elem_ty_before_has_structural_array {
            let _ = self.validate_vec_element_type(&elem_ty_after, span);
            return Ty::Error;
        }
        result
    }

    #[expect(
        clippy::too_many_lines,
        reason = "pattern matching type checker with many variants"
    )]
    pub(super) fn check_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Module-qualified calls: e.g. http.listen(addr) → lookup "http.listen" in fn_sigs
        if let Expr::Identifier(name) = &receiver.0 {
            let receiver_is_binding = self.env.lookup_ref(name).is_some();
            let receiver_is_known_type = self.type_defs.contains_key(name);
            let key = format!("{name}.{method}");
            let looks_like_module_call = !receiver_is_binding
                && !receiver_is_known_type
                && (self.modules.contains(name)
                    || self.module_fn_exports.contains(&key)
                    || self.fn_sigs.contains_key(&key));
            if looks_like_module_call {
                if self.modules.contains(name) {
                    self.used_modules
                        .borrow_mut()
                        .insert(ImportKey::new(self.current_module.clone(), name.clone()));
                }
                if !self.module_fn_exports.contains(&key) {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no function `{method}` in module `{name}`"),
                    );
                    return Ty::Error;
                }
                self.require_unsafe(&key, span);
                if let Some(sig) = self.fn_sigs.get(&key).cloned() {
                    if let Some(caller) = &self.current_function {
                        self.call_graph
                            .entry(caller.clone())
                            .or_default()
                            .insert(key.clone());
                    }
                    let (freshened_params, freshened_ret, resolved_type_args) =
                        self.instantiate_fn_sig_for_call(&sig, None, span);
                    // Separate positional and named args
                    let positional_count = args.iter().take_while(|a| a.name().is_none()).count();
                    let positional_args = &args[..positional_count];
                    let named_args = &args[positional_count..];

                    // Arity check
                    if !sig.accepts_kwargs && args.len() != freshened_params.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "expected {} arguments, found {}",
                                freshened_params.len(),
                                args.len()
                            ),
                        );
                    } else if sig.accepts_kwargs && positional_count < freshened_params.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "expected at least {} positional arguments, found {}",
                                freshened_params.len(),
                                positional_count
                            ),
                        );
                    }

                    // Check positional args by index
                    for (i, arg) in positional_args.iter().enumerate() {
                        if let Some(param_ty) = freshened_params.get(i) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    }

                    // Check named args by name lookup
                    for arg in named_args {
                        if let Some(arg_name) = arg.name() {
                            if let Some(idx) = sig.param_names.iter().position(|n| n == arg_name) {
                                if let Some(param_ty) = freshened_params.get(idx) {
                                    let (expr, sp) = arg.expr();
                                    self.check_against(expr, sp, param_ty);
                                }
                            } else if !sig.accepts_kwargs {
                                let (_, sp) = arg.expr();
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    sp,
                                    format!("unknown named argument `{arg_name}`"),
                                );
                            } else {
                                // For kwargs functions, synthesize the expression type
                                let (expr, sp) = arg.expr();
                                self.synthesize(expr, sp);
                            }
                        }
                    }
                    self.enforce_type_param_bounds(&sig, &resolved_type_args, span);

                    if !sig.type_params.is_empty() {
                        self.record_concrete_call_type_args(span, &resolved_type_args);
                    }
                    // Channel constructor: inject a shared type variable so
                    // Sender<T> and Receiver<T> from the same `new` call are
                    // linked through unification.
                    if key == "channel.new" {
                        let t = Ty::Var(TypeVar::fresh());
                        return Ty::Tuple(vec![Ty::sender(t.clone()), Ty::receiver(t)]);
                    }
                    return freshened_ret;
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no function `{method}` in module `{name}`"),
                );
                return Ty::Error;
            }

            // Static method calls on type names: e.g. Point.from_json(json)
            // Look up "TypeName.method" in fn_sigs (registered by wire types etc.)
            let static_key = format!("{name}.{method}");
            if let Some(sig) = self.fn_sigs.get(&static_key).cloned() {
                self.check_arity(args, sig.params.len(), &format!("`{static_key}`"), span);
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param_ty) = sig.params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param_ty);
                    }
                }
                return sig.return_type;
            }
        }

        let receiver_ty = self.synthesize(&receiver.0, &receiver.1);
        let resolved = self.subst.resolve(&receiver_ty);

        match (&resolved, method) {
            // Vec methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "Vec" => {
                self.check_vec_method(type_args, &receiver_ty, &resolved, method, args, span)
            }
            // HashMap methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "HashMap" => self.check_hashmap_method(type_args, method, args, span),
            // HashSet methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "HashSet" => self.check_hashset_method(type_args, method, args, span),
            // Rc<T> methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if name == "Rc" => self.check_rc_method(type_args, method, args, span),
            // bytes methods (ref-counted byte buffer)
            (Ty::Bytes, _) => match method {
                "push" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::Unit
                }
                "pop" => Ty::I32,
                "len" => Ty::I64,
                "get" | "remove" => {
                    if let Some(idx) = args.first() {
                        let (expr, sp) = idx.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    Ty::I32
                }
                "set" => {
                    if let Some(idx) = args.first() {
                        let (expr, sp) = idx.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    if let Some(val) = args.get(1) {
                        let (expr, sp) = val.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::Unit
                }
                "is_empty" => Ty::Bool,
                "clear" => Ty::Unit,
                "contains" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::Bool
                }
                "to_string" => Ty::String,
                "append" | "extend" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::Bytes);
                    }
                    Ty::Unit
                }
                _ => {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `bytes`"),
                    );
                    Ty::Error
                }
            },
            // Duration methods
            (Ty::Duration, _) => match method {
                "nanos" | "micros" | "millis" | "secs" | "mins" | "hours" => {
                    self.check_arity(args, 0, &format!("`duration::{method}`"), span);
                    Ty::I64
                }
                "abs" => {
                    self.check_arity(args, 0, "`duration::abs`", span);
                    Ty::Duration
                }
                "is_zero" => {
                    self.check_arity(args, 0, "`duration::is_zero`", span);
                    Ty::Bool
                }
                _ => {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `duration`"),
                    );
                    Ty::Error
                }
            },
            // Numeric type conversion methods (§10.1 intrinsics)
            // .to_i8(), .to_i16(), .to_i32(), .to_i64(), .to_u8(), .to_u16(),
            // .to_u32(), .to_u64(), .to_f32(), .to_f64(), .to_isize(), .to_usize()
            (resolved, method) if resolved.is_numeric() && method.starts_with("to_") => {
                match method {
                    "to_i8" => Ty::I8,
                    "to_i16" => Ty::I16,
                    "to_i32" => Ty::I32,
                    // to_isize maps to I64 (platform-dependent, default 64-bit)
                    "to_i64" | "to_isize" => Ty::I64,
                    "to_u8" => Ty::U8,
                    "to_u16" => Ty::U16,
                    "to_u32" => Ty::U32,
                    // to_usize maps to U64 (platform-dependent, default 64-bit)
                    "to_u64" | "to_usize" => Ty::U64,
                    "to_f32" => Ty::F32,
                    "to_f64" => Ty::F64,
                    _ => {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "no conversion method `{method}` on `{}`",
                                resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                }
            }
            // ActorRef methods
            (resolved, _) if resolved.as_actor_ref().is_some() => {
                let inner = resolved.as_actor_ref().unwrap();
                if method == "send" {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        let ty = self.synthesize(expr, sp);
                        self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                    }
                    Ty::Unit
                } else {
                    // Try to dispatch to the actor's receive methods
                    if let Ty::Named {
                        name: actor_name, ..
                    } = inner
                    {
                        let method_key = format!("{actor_name}::{method}");
                        if let Some(sig) = self.fn_sigs.get(&method_key).cloned() {
                            for (i, arg) in args.iter().enumerate() {
                                let (expr, sp) = arg.expr();
                                if let Some(param_ty) = sig.params.get(i) {
                                    self.check_against(expr, sp, param_ty);
                                }
                                let ty_raw = self.synthesize(expr, sp);
                                self.enforce_actor_boundary_send(expr, sp, sp, &ty_raw);
                            }
                            return sig.return_type;
                        }
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `{}`", resolved.user_facing()),
                    );
                    Ty::Error
                }
            }
            // Named types that have built-in methods (Actor<T> from lambda actors)
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                "send",
            ) if name == "Actor" => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    let ty = if let Some(param_ty) = type_args.first() {
                        self.check_against(expr, sp, param_ty)
                    } else {
                        self.synthesize(expr, sp)
                    };
                    self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                }
                Ty::Unit
            }
            // String methods
            (Ty::String, _) => self.check_string_method(method, args, span),
            // http.Server methods
            (Ty::Named { name, .. }, _) if name == "http.Server" => match method {
                "accept" => {
                    self.warn_if_blocking_in_receive_fn("http.Server::accept", span);
                    Ty::Named {
                        name: "http.Request".to_string(),
                        args: vec![],
                    }
                }
                "close" => Ty::Unit,
                _ => self.check_named_method_fallback(&resolved, method, args, span, "http.Server"),
            },
            // http.Request methods/properties
            (Ty::Named { name, .. }, _) if name == "http.Request" => match method {
                "path" | "method" | "body" => Ty::String,
                "header" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::String
                }
                "respond" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    if let Some(arg) = args.get(1) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    if let Some(arg) = args.get(2) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::I32
                }
                "respond_text" | "respond_json" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    if let Some(arg) = args.get(1) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::I32
                }
                "free" => Ty::Unit,
                _ => {
                    self.check_named_method_fallback(&resolved, method, args, span, "http.Request")
                }
            },
            // net.Listener methods
            (Ty::Named { name, .. }, _) if name == "net.Listener" => match method {
                "accept" => {
                    self.warn_if_blocking_in_receive_fn("net.Listener::accept", span);
                    Ty::Named {
                        name: "net.Connection".to_string(),
                        args: vec![],
                    }
                }
                "close" => Ty::Unit,
                _ => {
                    self.check_named_method_fallback(&resolved, method, args, span, "net.Listener")
                }
            },
            // net.Connection methods
            (Ty::Named { name, .. }, _) if name == "net.Connection" => match method {
                "read" => {
                    self.warn_if_blocking_in_receive_fn("net.Connection::read", span);
                    Ty::Bytes
                }
                "write" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::Bytes);
                    }
                    Ty::I32
                }
                "close" => Ty::I32,
                "set_read_timeout" | "set_write_timeout" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I32);
                    }
                    Ty::I32
                }
                _ => self.check_named_method_fallback(
                    &resolved,
                    method,
                    args,
                    span,
                    "net.Connection",
                ),
            },
            // regex.Pattern methods
            (Ty::Named { name, .. }, _) if name == "regex.Pattern" => match method {
                "is_match" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::Bool
                }
                "find" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::String
                }
                "replace" => {
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    if let Some(arg) = args.get(1) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::String);
                    }
                    Ty::String
                }
                "free" => Ty::Unit,
                _ => {
                    self.check_named_method_fallback(&resolved, method, args, span, "regex.Pattern")
                }
            },
            // process.Child methods
            (Ty::Named { name, .. }, _) if name == "process.Child" => match method {
                "wait" | "kill" => Ty::I32,
                "free" => Ty::Unit,
                _ => {
                    self.check_named_method_fallback(&resolved, method, args, span, "process.Child")
                }
            },
            // Generator methods: .next() returns the yielded type
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                "next",
            ) if name == "Generator" || name == "AsyncGenerator" => type_args
                .first()
                .cloned()
                .unwrap_or(Ty::Var(TypeVar::fresh())),
            // Stream<T> methods
            //
            // LIMITATION: Stream element-type validation only triggers here (on
            // method resolution).  A function parameter typed `Stream<MyStruct>`
            // passes typecheck if no stream methods are called on it.  Ideally
            // we would reject unsupported element types in resolve_type_expr when
            // the Stream<T> type is first formed, but that requires propagating
            // the span and restructuring the named-type resolution path.  For
            // now codegen will fail if the type is actually used.
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if builtin_named_type(name) == Some(BuiltinNamedType::Stream) => {
                self.check_stream_method(type_args, method, args, span)
            }
            // Sink<T> methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if builtin_named_type(name) == Some(BuiltinNamedType::Sink) => {
                let Some(inner) = self.validate_stream_sink_element_type(
                    type_args,
                    BuiltinNamedType::Sink.canonical_name(),
                    method,
                    span,
                ) else {
                    return Ty::Error;
                };
                if method == "encode" {
                    return self.report_unlowerable_stream_codec_boundary(
                        BuiltinNamedType::Sink.canonical_name(),
                        &inner,
                        method,
                        span,
                    );
                }
                let receiver_ty = Ty::sink(inner.clone());
                match method {
                    "write" => {
                        let sig =
                            lookup_builtin_method_sig(&receiver_ty, method).unwrap_or_else(|| {
                                unreachable!("builtin Sink::write signature missing")
                            });
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            if let Some(param_ty) = sig.params.first() {
                                self.check_against(expr, sp, param_ty);
                            }
                        }
                        sig.return_type
                    }
                    "close" | "flush" => {
                        lookup_builtin_method_sig(&receiver_ty, method)
                            .unwrap_or_else(|| {
                                unreachable!("builtin Sink::{method} signature missing")
                            })
                            .return_type
                    }
                    _ => {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on `{}`", resolved.user_facing()),
                        );
                        Ty::Error
                    }
                }
            }
            // Sender<T> methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if builtin_named_type(name) == Some(BuiltinNamedType::Sender) => {
                let inner = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                let receiver_ty = Ty::sender(inner.clone());
                match method {
                    "send" => {
                        let sig =
                            lookup_builtin_method_sig(&receiver_ty, method).unwrap_or_else(|| {
                                unreachable!("builtin Sender::send signature missing")
                            });
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            if let Some(param_ty) = sig.params.first() {
                                self.check_against(expr, sp, param_ty);
                            }
                        }
                        // Validate after unification so the concrete type is known.
                        let resolved_inner = self.subst.resolve(&inner);
                        if !matches!(resolved_inner, Ty::Var(_) | Ty::String)
                            && !resolved_inner.is_integer()
                        {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "Channel<{resolved_inner}> is not supported; \
                                     only Channel<String> and Channel<int> are currently supported"
                                ),
                            );
                            return Ty::Error;
                        }
                        sig.return_type
                    }
                    "clone" | "close" => {
                        lookup_builtin_method_sig(&receiver_ty, method)
                            .unwrap_or_else(|| {
                                unreachable!("builtin Sender::{method} signature missing")
                            })
                            .return_type
                    }
                    _ => {
                        self.check_named_method_fallback(&resolved, method, args, span, "Sender<T>")
                    }
                }
            }
            // Receiver<T> methods
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) if builtin_named_type(name) == Some(BuiltinNamedType::Receiver) => {
                let inner = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                let receiver_ty = Ty::receiver(inner.clone());
                let resolved_inner = self.subst.resolve(&inner);
                if !matches!(resolved_inner, Ty::Var(_) | Ty::String)
                    && !resolved_inner.is_integer()
                {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "Channel<{resolved_inner}> is not supported; \
                             only Channel<String> and Channel<int> are currently supported"
                        ),
                    );
                    return Ty::Error;
                }
                match method {
                    "recv" => {
                        let sig =
                            lookup_builtin_method_sig(&receiver_ty, method).unwrap_or_else(|| {
                                unreachable!("builtin Receiver::recv signature missing")
                            });
                        self.warn_if_blocking_in_receive_fn("Receiver::recv", span);
                        sig.return_type
                    }
                    "try_recv" | "close" => {
                        lookup_builtin_method_sig(&receiver_ty, method)
                            .unwrap_or_else(|| {
                                unreachable!("builtin Receiver::{method} signature missing")
                            })
                            .return_type
                    }
                    _ => self.check_named_method_fallback(
                        &resolved,
                        method,
                        args,
                        span,
                        "Receiver<T>",
                    ),
                }
            }
            // Machine methods: step(), state_name()
            (Ty::Machine { name }, _) => {
                if let Some(td) = self.lookup_type_def(name) {
                    if let Some(sig) = td.methods.get(method) {
                        self.check_arity(
                            args,
                            sig.params.len(),
                            &format!("method '{method}'"),
                            span,
                        );
                        for (i, arg) in args.iter().enumerate() {
                            if let Some(param_ty) = sig.params.get(i) {
                                let (expr, sp) = arg.expr();
                                self.check_against(expr, sp, param_ty);
                            }
                        }
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::NamedTypeInstance {
                                type_name: name.clone(),
                            },
                        );
                        return sig.return_type.clone();
                    }
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{name}`"),
                );
                Ty::Error
            }
            // User-defined struct/actor methods from type_defs
            (
                Ty::Named {
                    name,
                    args: type_args,
                },
                _,
            ) => {
                if let Some(sig) = self.lookup_named_method_sig(name, type_args, method) {
                    self.check_arity(args, sig.params.len(), &format!("method '{method}'"), span);
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(param_ty) = sig.params.get(i) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    }
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::NamedTypeInstance {
                            type_name: name.clone(),
                        },
                    );
                    return sig.return_type;
                }
                // Type-parameter method dispatch: resolve from trait bounds.
                // When the receiver is a generic type parameter (e.g. `T` in
                // `fn report<T: Measurable>(item: T)`), look up the method
                // from the traits that bound that parameter.
                let bounds_for_type_param = self.current_function.as_ref().and_then(|fn_name| {
                    self.fn_sigs.get(fn_name).and_then(|sig| {
                        if sig.type_params.contains(name) {
                            sig.type_param_bounds.get(name).cloned()
                        } else {
                            None
                        }
                    })
                });
                if let Some(bounds) = bounds_for_type_param {
                    for bound_trait in &bounds {
                        if let Some(mut trait_sig) = self.lookup_trait_method(bound_trait, method) {
                            // Replace `Self` references with the type parameter type.
                            let self_ty = resolved.clone();
                            for param_ty in &mut trait_sig.params {
                                *param_ty = self.substitute_named_param(param_ty, "Self", &self_ty);
                            }
                            trait_sig.return_type = self.substitute_named_param(
                                &trait_sig.return_type,
                                "Self",
                                &self_ty,
                            );

                            self.check_arity(
                                args,
                                trait_sig.params.len(),
                                &format!("method '{method}'"),
                                span,
                            );
                            for (i, arg) in args.iter().enumerate() {
                                if let Some(param_ty) = trait_sig.params.get(i) {
                                    let (expr, sp) = arg.expr();
                                    self.check_against(expr, sp, param_ty);
                                }
                            }
                            self.record_method_call_receiver_kind(
                                span,
                                MethodCallReceiverKind::NamedTypeInstance {
                                    type_name: name.clone(),
                                },
                            );
                            return trait_sig.return_type;
                        }
                    }
                }
                // Synthesize args even if method unknown (for error recovery)
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                );
                Ty::Error
            }
            // Trait object method dispatch: look up methods from all trait bounds
            (Ty::TraitObject { traits }, _) => {
                // Try to find the method in any of the traits
                let mut found_sig = None;
                let mut found_bound = None;
                for bound in traits {
                    if let Some(sig) = self.lookup_trait_method(&bound.trait_name, method) {
                        found_sig = Some(sig);
                        found_bound = Some(bound);
                        break;
                    }
                }

                if let Some(mut sig) = found_sig {
                    if let Some(bound) = found_bound {
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::TraitObject {
                                trait_name: bound.trait_name.clone(),
                            },
                        );
                    }
                    // Apply type substitutions from bound's type arguments
                    if let Some(bound) = found_bound {
                        if let Some(trait_info) = self.trait_defs.get(&bound.trait_name) {
                            let type_params = &trait_info.type_params;
                            if type_params.len() == bound.args.len() {
                                // Apply substitutions
                                for (param_name, replacement) in
                                    type_params.iter().zip(bound.args.iter())
                                {
                                    // Substitute in parameter types
                                    for param_ty in &mut sig.params {
                                        *param_ty = self.substitute_named_param(
                                            param_ty,
                                            param_name,
                                            replacement,
                                        );
                                    }
                                    // Substitute in return type
                                    sig.return_type = self.substitute_named_param(
                                        &sig.return_type,
                                        param_name,
                                        replacement,
                                    );
                                }
                            }
                        }
                    }

                    self.check_arity(args, sig.params.len(), &format!("method '{method}'"), span);
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(param_ty) = sig.params.get(i) {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, param_ty);
                        }
                    }
                    sig.return_type
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `{}`", resolved.user_facing()),
                    );
                    Ty::Error
                }
            }
            // For error types, don't report additional errors
            (Ty::Error, _) => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                Ty::Error
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                );
                Ty::Error
            }
        }
    }
}
