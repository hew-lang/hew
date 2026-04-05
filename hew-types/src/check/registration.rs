#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

impl Checker {
    #[expect(
        clippy::too_many_lines,
        reason = "all builtins registered in one place"
    )]
    pub(super) fn register_builtins(&mut self) {
        // Print functions
        self.register_builtin_fn("println_int", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("println_str", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("print_int", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("print_str", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("println_float", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("println_bool", vec![Ty::Bool], Ty::Unit);
        self.register_builtin_fn("print_float", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("print_bool", vec![Ty::Bool], Ty::Unit);
        // Generic print/println that accept any type
        self.register_builtin_fn("println", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        self.register_builtin_fn("print", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);

        // Math functions
        self.register_builtin_fn("abs", vec![Ty::I64], Ty::I64);
        self.register_builtin_fn("sqrt", vec![Ty::F64], Ty::F64);
        self.register_builtin_fn("min", vec![Ty::I64, Ty::I64], Ty::I64);
        self.register_builtin_fn("max", vec![Ty::I64, Ty::I64], Ty::I64);
        self.register_builtin_fn("to_float", vec![Ty::I64], Ty::F64);

        // String operations
        self.register_builtin_fn("string_concat", vec![Ty::String, Ty::String], Ty::String);
        self.register_builtin_fn("string_length", vec![Ty::String], Ty::I64);
        self.register_builtin_fn("to_string", vec![Ty::Var(TypeVar::fresh())], Ty::String);
        self.register_builtin_fn("len", vec![Ty::Var(TypeVar::fresh())], Ty::I64);

        // I/O and system
        self.register_builtin_fn("read_file", vec![Ty::String], Ty::String);
        self.register_builtin_fn("write_file", vec![Ty::String, Ty::String], Ty::Unit);
        self.register_builtin_fn("sleep_ms", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("sleep", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("stop", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        let close_t = TypeVar::fresh();
        self.register_builtin_fn(
            "close",
            vec![Ty::actor_ref(Ty::Var(close_t))],
            Ty::actor_ref(Ty::Var(close_t)),
        );
        self.register_builtin_fn("exit", vec![Ty::I64], Ty::Never);
        self.register_builtin_fn("panic", vec![Ty::String], Ty::Never);

        // Actor link/monitor (Erlang-style fault propagation)
        let link_t = TypeVar::fresh();
        self.register_builtin_fn("link", vec![Ty::actor_ref(Ty::Var(link_t))], Ty::Unit);
        let unlink_t = TypeVar::fresh();
        self.register_builtin_fn("unlink", vec![Ty::actor_ref(Ty::Var(unlink_t))], Ty::Unit);
        let monitor_t = TypeVar::fresh();
        self.register_builtin_fn("monitor", vec![Ty::actor_ref(Ty::Var(monitor_t))], Ty::I64);
        self.register_builtin_fn("demonitor", vec![Ty::I64], Ty::Unit);

        // Supervisor child access
        let sup_child_t = TypeVar::fresh();
        let sup_child_ret = TypeVar::fresh();
        self.register_builtin_fn(
            "supervisor_child",
            vec![Ty::actor_ref(Ty::Var(sup_child_t)), Ty::I64],
            Ty::actor_ref(Ty::Var(sup_child_ret)),
        );
        let sup_stop_t = TypeVar::fresh();
        self.register_builtin_fn(
            "supervisor_stop",
            vec![Ty::actor_ref(Ty::Var(sup_stop_t))],
            Ty::Unit,
        );

        // Assertions (test support)
        self.register_builtin_fn("assert", vec![Ty::Bool], Ty::Unit);
        let t_eq = TypeVar::fresh();
        self.register_builtin_fn("assert_eq", vec![Ty::Var(t_eq), Ty::Var(t_eq)], Ty::Unit);
        let t_ne = TypeVar::fresh();
        self.register_builtin_fn("assert_ne", vec![Ty::Var(t_ne), Ty::Var(t_ne)], Ty::Unit);

        // Option/Result constructors
        // Option/Result constructors are handled specially in check_call
        // (they need fresh linked type vars per invocation)

        // Collection constructors (path-style calls: Vec::new(), HashMap::new())
        // These also need fresh vars per call but are less critical
        self.register_builtin_fn(
            "Vec::new",
            vec![],
            Ty::Named {
                name: "Vec".to_string(),
                args: vec![Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "Vec::with_capacity",
            vec![Ty::I64],
            Ty::Named {
                name: "Vec".to_string(),
                args: vec![Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "HashMap::new",
            vec![],
            Ty::Named {
                name: "HashMap".to_string(),
                args: vec![Ty::Var(TypeVar::fresh()), Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn(
            "HashSet::new",
            vec![],
            Ty::Named {
                name: "HashSet".to_string(),
                args: vec![Ty::Var(TypeVar::fresh())],
            },
        );
        self.register_builtin_fn("bytes::new", vec![], Ty::Bytes);
        self.register_builtin_fn("duration::from_nanos", vec![Ty::I64], Ty::Duration);

        // Rc<T> constructor — Rc::new(value: T) -> Rc<T>
        // Each call site gets a fresh type variable; the actual Rc<T> type is
        // inferred from the argument type.
        {
            let t = TypeVar::fresh();
            self.register_builtin_fn("Rc::new", vec![Ty::Var(t)], Ty::rc(Ty::Var(t)));
        }

        // More print variants
        self.register_builtin_fn("println_f64", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("print_f64", vec![Ty::F64], Ty::Unit);
        self.register_builtin_fn("println_i64", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("println_char", vec![Ty::Char], Ty::Unit);

        // String utilities
        self.register_builtin_fn("string_char_at", vec![Ty::String, Ty::I64], Ty::Char);
        self.register_builtin_fn("string_equals", vec![Ty::String, Ty::String], Ty::Bool);
        self.register_builtin_fn("string_from_int", vec![Ty::I64], Ty::String);
        self.register_builtin_fn("string_contains", vec![Ty::String, Ty::String], Ty::Bool);
        self.register_builtin_fn(
            "string_split",
            vec![Ty::String, Ty::String],
            Ty::Named {
                name: "Vec".to_string(),
                args: vec![Ty::String],
            },
        );
        self.register_builtin_fn("string_starts_with", vec![Ty::String, Ty::String], Ty::Bool);
        self.register_builtin_fn("substring", vec![Ty::String, Ty::I64, Ty::I64], Ty::String);
        self.register_builtin_fn(
            "string_slice",
            vec![Ty::String, Ty::I64, Ty::I64],
            Ty::String,
        );
        self.register_builtin_fn("string_trim", vec![Ty::String], Ty::String);
        self.register_builtin_fn("string_to_int", vec![Ty::String], Ty::I64);
        self.register_builtin_fn("string_find", vec![Ty::String, Ty::String], Ty::I64);
        self.register_builtin_fn(
            "string_replace",
            vec![Ty::String, Ty::String, Ty::String],
            Ty::String,
        );
        self.register_builtin_fn("string_to_upper", vec![Ty::String], Ty::String);
        self.register_builtin_fn("string_to_lower", vec![Ty::String], Ty::String);
        self.register_builtin_fn("string_ends_with", vec![Ty::String, Ty::String], Ty::Bool);
        self.register_builtin_fn("int_to_string", vec![Ty::I64], Ty::String);
        self.register_builtin_fn("float_to_string", vec![Ty::F64], Ty::String);
        self.register_builtin_fn("char_to_string", vec![Ty::Char], Ty::String);
        self.register_builtin_fn("bool_to_string", vec![Ty::Bool], Ty::String);

        // Node/distributed builtins
        self.register_builtin_fn("Node::start", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("Node::shutdown", vec![], Ty::Unit);
        self.register_builtin_fn("Node::connect", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn("Node::set_transport", vec![Ty::String], Ty::Unit);
        self.register_builtin_fn(
            "Node::register",
            vec![Ty::String, Ty::Var(TypeVar::fresh())],
            Ty::Unit,
        );
        self.register_builtin_fn("Node::lookup", vec![Ty::String], Ty::Var(TypeVar::fresh()));

        // std::math module — always available, no import needed
        self.modules.insert("math".to_string());
        // Single-argument math functions: f64 → f64
        for name in &[
            "exp", "log", "sqrt", "sin", "cos", "floor", "ceil", "abs", "tanh", "log2", "log10",
            "exp2",
        ] {
            self.register_builtin_fn(&format!("math.{name}"), vec![Ty::F64], Ty::F64);
        }
        // Two-argument math functions: (f64, f64) → f64
        for name in &["pow", "max", "min"] {
            self.register_builtin_fn(&format!("math.{name}"), vec![Ty::F64, Ty::F64], Ty::F64);
        }
        // Constants (zero-argument): () → f64
        for name in &["pi", "e"] {
            self.register_builtin_fn(&format!("math.{name}"), vec![], Ty::F64);
        }

        // std::random module — always available, no import needed
        self.modules.insert("random".to_string());
        self.register_builtin_fn("random.seed", vec![Ty::I64], Ty::Unit);
        self.register_builtin_fn("random.random", vec![], Ty::F64);
        self.register_builtin_fn("random.gauss", vec![Ty::F64, Ty::F64], Ty::F64);
        self.register_builtin_fn("random.randint", vec![Ty::I64, Ty::I64], Ty::I64);
        self.register_builtin_fn("random.shuffle", vec![Ty::Var(TypeVar::fresh())], Ty::Unit);
        self.register_builtin_fn(
            "random.choices",
            vec![Ty::Var(TypeVar::fresh()), Ty::F64, Ty::I64],
            Ty::I64,
        );
    }

    pub(super) fn register_builtin_fn(&mut self, name: &str, params: Vec<Ty>, return_type: Ty) {
        self.fn_sigs.insert(
            name.to_string(),
            FnSig {
                params,
                return_type,
                ..FnSig::default()
            },
        );
    }

    /// Pass 1: Collect type definitions
    pub(super) fn collect_types(&mut self, program: &Program) {
        for (item, span) in &program.items {
            match item {
                Item::TypeDecl(td) => {
                    if !self.register_type_namespace_name(&td.name, span) {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.local_type_defs.insert(td.name.clone());
                }
                Item::Actor(ad) => {
                    if !self.register_type_namespace_name(&ad.name, span) {
                        continue;
                    }
                    self.register_actor_decl(ad);
                }
                Item::Wire(wd) => {
                    if !self.register_type_namespace_name(&wd.name, span) {
                        continue;
                    }
                    self.register_wire_decl(wd);
                }
                Item::TypeAlias(ta) => {
                    if !self.register_type_namespace_name(&ta.name, span) {
                        continue;
                    }
                    let mut hole_vars = Vec::new();
                    let resolved = self.resolve_type_expr_tracking_holes(&ta.ty.0, &mut hole_vars);
                    self.type_aliases.insert(ta.name.clone(), resolved);
                    self.record_type_def_inference_holes(&ta.name, hole_vars);
                }
                Item::Trait(td) => {
                    if !self.register_type_namespace_name(&td.name, span) {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(td);
                    self.trait_defs.insert(td.name.clone(), info);
                    self.local_trait_defs.insert(td.name.clone());
                    // Record super-trait relationships
                    if let Some(supers) = &td.super_traits {
                        let super_names: Vec<String> =
                            supers.iter().map(|s| s.name.clone()).collect();
                        self.trait_super.insert(td.name.clone(), super_names);
                    }
                }
                Item::Supervisor(sd) => {
                    self.warn_wasm_limitation(span, WasmUnsupportedFeature::SupervisionTrees);
                    let children: Vec<(String, String)> = sd
                        .children
                        .iter()
                        .map(|c| (c.name.clone(), c.actor_type.clone()))
                        .collect();
                    self.supervisor_children.insert(sd.name.clone(), children);
                }
                Item::Machine(md) => {
                    if !self.register_machine_type_namespace_names(&md.name, span) {
                        continue;
                    }
                    self.register_machine_decl(md);
                    self.local_type_defs.insert(md.name.clone());
                }
                _ => {}
            }
        }
    }

    pub(super) fn register_type_namespace_name(&mut self, name: &str, span: &Span) -> bool {
        if let Some(prev_span) = self.type_def_spans.get(name).cloned() {
            self.report_duplicate_type_namespace_name(name, span, prev_span);
            return false;
        }

        self.type_def_spans.insert(name.to_string(), span.clone());
        true
    }

    pub(super) fn report_duplicate_type_namespace_name(
        &mut self,
        name: &str,
        span: &Span,
        prev_span: Span,
    ) {
        self.errors.push(TypeError::duplicate_definition(
            span.clone(),
            name,
            prev_span,
        ));
    }

    pub(super) fn register_machine_type_namespace_names(
        &mut self,
        machine_name: &str,
        span: &Span,
    ) -> bool {
        if let Some(prev_span) = self.type_def_spans.get(machine_name).cloned() {
            self.report_duplicate_type_namespace_name(machine_name, span, prev_span);
            return false;
        }

        let event_type_name = format!("{machine_name}Event");
        if let Some(prev_span) = self.type_def_spans.get(&event_type_name).cloned() {
            self.report_duplicate_type_namespace_name(&event_type_name, span, prev_span);
            return false;
        }

        self.type_def_spans
            .insert(machine_name.to_string(), span.clone());
        self.type_def_spans.insert(event_type_name, span.clone());
        true
    }

    #[expect(clippy::too_many_lines, reason = "type resolution requires many cases")]
    pub(super) fn register_type_decl(&mut self, td: &TypeDecl) {
        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };

        let mut fields = HashMap::new();
        let mut variants = HashMap::new();
        let mut hole_vars = Vec::new();
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(td.type_params.as_ref(), td.where_clause.as_ref());
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                name: name.clone(),
                args: vec![],
            })
            .collect();

        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let field_ty = self.resolve_type_expr_tracking_holes(&ty.0, &mut hole_vars);
                    fields.insert(name.clone(), field_ty);
                }
                TypeBodyItem::Variant(variant) => {
                    let return_type = Ty::Named {
                        name: td.name.clone(),
                        args: enum_return_args.clone(),
                    };
                    match &variant.kind {
                        VariantKind::Unit => {
                            variants.insert(variant.name.clone(), VariantDef::Unit);
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    return_type,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Tuple(fields) => {
                            let variant_tys: Vec<Ty> = fields
                                .iter()
                                .map(|(te, _)| {
                                    self.resolve_type_expr_tracking_holes(te, &mut hole_vars)
                                })
                                .collect();
                            variants.insert(variant.name.clone(), VariantDef::Tuple(variant_tys));

                            // Register variant constructor as function
                            let constructor_params: Vec<Ty> = fields
                                .iter()
                                .map(|(te, _)| {
                                    self.resolve_type_expr_tracking_holes(te, &mut hole_vars)
                                })
                                .collect();
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    params: constructor_params,
                                    return_type,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Struct(fields) => {
                            let variant_fields: Vec<(String, Ty)> = fields
                                .iter()
                                .map(|(name, (te, _))| {
                                    (
                                        name.clone(),
                                        self.resolve_type_expr_tracking_holes(te, &mut hole_vars),
                                    )
                                })
                                .collect();
                            variants
                                .insert(variant.name.clone(), VariantDef::Struct(variant_fields));
                        }
                    }
                }
                TypeBodyItem::Method(_) => {
                    // Methods are handled in pass 2
                }
            }
        }

        let type_def = TypeDef {
            kind,
            name: td.name.clone(),
            type_params: type_param_names.clone(),
            fields,
            variants,
            methods: HashMap::new(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };

        // Register with trait registry for Send/Frozen derivation
        let field_types: Vec<_> = type_def.fields.values().cloned().collect();
        let all_fields_encodable = td.wire.is_none()
            && kind == TypeDefKind::Struct
            && field_types
                .iter()
                .all(|f| self.registry.implements_marker(f, MarkerTrait::Encode));

        self.registry.register_type(td.name.clone(), field_types);

        self.type_defs.insert(td.name.clone(), type_def);
        self.record_type_def_inference_holes(&td.name, hole_vars);

        // If this is a wire type, register encode/decode/to_json/from_json/to_yaml/from_yaml methods
        if let Some(ref wire) = td.wire {
            self.register_wire_methods(&td.name);
            self.validate_wire_version_constraints(&td.name, wire);
        }

        // For non-wire struct types: if all fields are Encode, register
        // serialization methods (to_json, from_json, to_yaml, from_yaml, to_toml, from_toml)
        if all_fields_encodable {
            self.register_encode_methods(&td.name);
        }
    }

    /// Register codec methods for a wire type.
    ///
    /// - Wire structs expose binary + JSON/YAML helpers.
    /// - Unit-only wire enums expose JSON/YAML helpers.
    /// - Payload-bearing wire enums do not currently expose helper methods.
    pub(super) fn register_wire_methods(&mut self, type_name: &str) {
        let self_ty = Ty::Named {
            name: type_name.to_string(),
            args: vec![],
        };
        let bytes_ty = Ty::Bytes;

        let Some(type_def) = self.type_defs.get(type_name) else {
            return;
        };
        let is_wire_struct = type_def.kind == TypeDefKind::Struct;
        let is_unit_wire_enum = type_def.kind == TypeDefKind::Enum
            && type_def
                .variants
                .values()
                .all(|variant| matches!(variant, VariantDef::Unit));

        let instance_methods = if is_wire_struct {
            vec![
                ("encode", vec![], bytes_ty.clone()),
                ("to_json", vec![], Ty::String),
                ("to_yaml", vec![], Ty::String),
            ]
        } else if is_unit_wire_enum {
            vec![
                ("to_json", vec![], Ty::String),
                ("to_yaml", vec![], Ty::String),
            ]
        } else {
            vec![]
        };

        if let Some(type_def) = self.type_defs.get_mut(type_name) {
            for (method_name, params, return_type) in instance_methods {
                type_def.methods.insert(
                    method_name.to_string(),
                    FnSig {
                        params,
                        return_type,
                        is_pure: true,
                        ..FnSig::default()
                    },
                );
            }
        }

        let static_methods = if is_wire_struct {
            vec![
                ("decode", vec![bytes_ty], self_ty.clone()),
                ("from_json", vec![Ty::String], self_ty.clone()),
                ("from_yaml", vec![Ty::String], self_ty),
            ]
        } else if is_unit_wire_enum {
            vec![
                ("from_json", vec![Ty::String], self_ty.clone()),
                ("from_yaml", vec![Ty::String], self_ty),
            ]
        } else {
            vec![]
        };

        for (method_name, params, return_type) in static_methods {
            let qualified_name = format!("{type_name}.{method_name}");
            self.fn_sigs.insert(
                qualified_name,
                FnSig {
                    params,
                    return_type,
                    is_pure: true,
                    ..FnSig::default()
                },
            );
        }
    }

    /// Register serialization methods for a struct type that implements `Encode`.
    ///
    /// Adds `to_json`, `to_yaml`, `to_toml` instance methods and
    /// `from_json`, `from_yaml`, `from_toml` static methods.
    ///
    /// The `from_*` static methods return `Result<Self, String>`.  Errors are
    /// returned for: (1) top-level parse failure, (2) missing required field,
    /// (3) a string field that is not a string at runtime, and (4) a scalar
    /// (int/bool/float) field whose runtime type code does not match the
    /// expected kind for the format.
    pub(super) fn register_encode_methods(&mut self, type_name: &str) {
        let self_ty = Ty::Named {
            name: type_name.to_string(),
            args: vec![],
        };

        // Instance methods: to_json(self) -> String, to_yaml(self) -> String, to_toml(self) -> String
        let instance_methods = [
            ("to_json", Ty::String),
            ("to_yaml", Ty::String),
            ("to_toml", Ty::String),
        ];

        if let Some(type_def) = self.type_defs.get_mut(type_name) {
            for (method_name, return_type) in instance_methods {
                type_def.methods.insert(
                    method_name.to_string(),
                    FnSig {
                        return_type,
                        is_pure: true,
                        ..FnSig::default()
                    },
                );
            }
        }

        // Static methods: TypeName.from_json(String) -> Result<Self, String>, etc.
        // Returns Result so callers can distinguish valid input from a malformed document
        // without a runtime panic.
        let result_ty = Ty::result(self_ty.clone(), Ty::String);
        let static_methods = [
            ("from_json", vec![Ty::String], result_ty.clone()),
            ("from_yaml", vec![Ty::String], result_ty.clone()),
            ("from_toml", vec![Ty::String], result_ty),
        ];

        for (method_name, params, return_type) in static_methods {
            let qualified_name = format!("{type_name}.{method_name}");
            self.fn_sigs.insert(
                qualified_name,
                FnSig {
                    params,
                    return_type,
                    is_pure: true,
                    ..FnSig::default()
                },
            );
        }
    }

    /// Validate version constraints on a wire type.
    pub(super) fn validate_wire_version_constraints(
        &mut self,
        type_name: &str,
        wire: &hew_parser::ast::WireMetadata,
    ) {
        use crate::error::Severity;

        let version = wire.version;
        let min_version = wire.min_version;

        // min_version cannot exceed version
        if let (Some(min_v), Some(v)) = (min_version, version) {
            if min_v > v {
                self.errors.push(TypeError {
                    severity: Severity::Error,
                    kind: TypeErrorKind::InvalidOperation,
                    span: 0..0,
                    message: format!(
                        "wire `{type_name}`: min_version ({min_v}) cannot exceed version ({v})"
                    ),
                    notes: vec![],
                    suggestions: vec![],
                });
            }
        }

        // Per-field `since` constraints
        for fm in &wire.field_meta {
            if let Some(since) = fm.since {
                if version.is_none() {
                    // since has no effect without a schema version
                    self.warnings.push(TypeError {
                        severity: Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: 0..0,
                        message: format!(
                            "wire `{type_name}.{}`: field has `since {since}` but struct \
                             has no #[wire(version = N)] attribute",
                            fm.field_name
                        ),
                        notes: vec![],
                        suggestions: vec![],
                    });
                }

                // since cannot exceed version
                if let Some(v) = version {
                    if since > v {
                        self.errors.push(TypeError {
                            severity: Severity::Error,
                            kind: TypeErrorKind::InvalidOperation,
                            span: 0..0,
                            message: format!(
                                "wire `{type_name}.{}`: since ({since}) cannot exceed \
                                 schema version ({v})",
                                fm.field_name
                            ),
                            notes: vec![],
                            suggestions: vec![],
                        });
                    }
                }
            }

            // Warn if version > 1 and a non-optional field lacks `since`
            if let Some(v) = version {
                if v > 1 && fm.since.is_none() && !fm.is_optional {
                    self.warnings.push(TypeError {
                        severity: Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: 0..0,
                        message: format!(
                            "wire `{type_name}.{}`: non-optional field has no `since` annotation \
                             (schema version is {v})",
                            fm.field_name
                        ),
                        notes: vec![],
                        suggestions: vec![],
                    });
                }
            }
        }
    }

    /// Register a machine declaration as a type definition with variants and methods.
    #[expect(
        clippy::too_many_lines,
        reason = "machine registration covers states, events, and generated methods"
    )]
    pub(super) fn register_machine_decl(&mut self, md: &MachineDecl) {
        let machine_ty = Ty::Machine {
            name: md.name.clone(),
        };
        let event_type_name = format!("{}Event", md.name);
        let event_ty = Ty::Named {
            name: event_type_name.clone(),
            args: vec![],
        };

        // Build state variants
        let mut variants = HashMap::new();
        let mut machine_hole_vars = Vec::new();
        for state in &md.states {
            if state.fields.is_empty() {
                variants.insert(state.name.clone(), VariantDef::Unit);
                // Register unit state constructor as a function
                self.fn_sigs.insert(
                    state.name.clone(),
                    FnSig {
                        return_type: machine_ty.clone(),
                        is_pure: true,
                        ..FnSig::default()
                    },
                );
            } else {
                let variant_fields: Vec<(String, Ty)> = state
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.clone(),
                            self.resolve_type_expr_tracking_holes(
                                &spanned_te.0,
                                &mut machine_hole_vars,
                            ),
                        )
                    })
                    .collect();
                variants.insert(state.name.clone(), VariantDef::Struct(variant_fields));
            }
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Machine,
            name: md.name.clone(),
            type_params: vec![],
            fields: HashMap::new(),
            variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };

        // Register field types for Send/Frozen derivation
        let mut all_field_types = Vec::new();
        for state in &md.states {
            for (_, spanned_te) in &state.fields {
                all_field_types.push(self.resolve_type_expr(&spanned_te.0));
            }
        }
        self.registry
            .register_type(md.name.clone(), all_field_types);

        self.type_defs.insert(md.name.clone(), type_def);
        self.record_type_def_inference_holes(&md.name, machine_hole_vars);
        self.known_types.insert(md.name.clone());

        // Register the generated event companion enum
        let mut event_variants = HashMap::new();
        let mut event_hole_vars = Vec::new();
        for event in &md.events {
            if event.fields.is_empty() {
                event_variants.insert(event.name.clone(), VariantDef::Unit);
            } else {
                let variant_fields: Vec<(String, Ty)> = event
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.clone(),
                            self.resolve_type_expr_tracking_holes(
                                &spanned_te.0,
                                &mut event_hole_vars,
                            ),
                        )
                    })
                    .collect();
                event_variants.insert(event.name.clone(), VariantDef::Struct(variant_fields));
            }
        }
        let event_type_def = TypeDef {
            kind: TypeDefKind::Enum,
            name: event_type_name.clone(),
            type_params: vec![],
            fields: HashMap::new(),
            variants: event_variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };
        self.type_defs
            .insert(event_type_name.clone(), event_type_def);
        self.record_type_def_inference_holes(&event_type_name, event_hole_vars);
        self.known_types.insert(event_type_name);

        // Register the step() method on the machine type
        if let Some(td) = self.type_defs.get_mut(&md.name) {
            td.methods.insert(
                "step".to_string(),
                FnSig {
                    param_names: vec!["event".to_string()],
                    params: vec![event_ty],
                    ..FnSig::default()
                },
            );
            // Register state_name() method
            td.methods.insert(
                "state_name".to_string(),
                FnSig {
                    return_type: Ty::String,
                    is_pure: true,
                    ..FnSig::default()
                },
            );
        }
    }

    /// Check that the machine's state × event matrix is fully covered.
    #[expect(
        clippy::too_many_lines,
        reason = "exhaustiveness checking requires many validation steps"
    )]
    pub(super) fn check_machine_exhaustiveness(&mut self, md: &MachineDecl, span: &Span) {
        let state_names: Vec<&str> = md.states.iter().map(|s| s.name.as_str()).collect();
        let event_names: Vec<&str> = md.events.iter().map(|e| e.name.as_str()).collect();

        // Fix 4: Enforce minimum cardinality
        if md.states.len() < 2 {
            self.errors.push(TypeError::new(
                TypeErrorKind::MachineExhaustivenessError,
                span.clone(),
                format!("machine '{}' must declare at least 2 states", md.name),
            ));
        }
        if md.events.is_empty() {
            self.errors.push(TypeError::new(
                TypeErrorKind::MachineExhaustivenessError,
                span.clone(),
                format!("machine '{}' must declare at least 1 event", md.name),
            ));
        }

        // Build coverage: track explicit (state, event) pairs and wildcard events
        let mut covered: HashSet<(String, String)> = HashSet::new();
        let mut wildcard_events: HashSet<String> = HashSet::new();

        for transition in &md.transitions {
            // Fix 2: Reject unknown event names
            if !event_names.contains(&transition.event_name.as_str()) {
                self.errors.push(TypeError::new(
                    TypeErrorKind::MachineExhaustivenessError,
                    span.clone(),
                    format!(
                        "machine '{}': transition references unknown event '{}'",
                        md.name, transition.event_name
                    ),
                ));
            }

            // Fix 1: Reject unknown source/target state names
            if transition.source_state != "_"
                && !state_names.contains(&transition.source_state.as_str())
            {
                self.errors.push(TypeError::new(
                    TypeErrorKind::MachineExhaustivenessError,
                    span.clone(),
                    format!(
                        "machine '{}': transition references unknown state '{}'",
                        md.name, transition.source_state
                    ),
                ));
            }
            if transition.target_state != "_"
                && !state_names.contains(&transition.target_state.as_str())
            {
                self.errors.push(TypeError::new(
                    TypeErrorKind::MachineExhaustivenessError,
                    span.clone(),
                    format!(
                        "machine '{}': transition references unknown state '{}'",
                        md.name, transition.target_state
                    ),
                ));
            }

            if transition.source_state == "_" {
                // Fix 3: Reject duplicate wildcard transitions for same event
                if wildcard_events.contains(&transition.event_name) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::MachineExhaustivenessError,
                        span.clone(),
                        format!(
                            "machine '{}': duplicate wildcard transition for event '{}'",
                            md.name, transition.event_name
                        ),
                    ));
                }
                wildcard_events.insert(transition.event_name.clone());
            } else {
                let key = (
                    transition.source_state.clone(),
                    transition.event_name.clone(),
                );
                // Fix 5: Reject duplicate explicit transitions (unless guarded)
                if covered.contains(&key) && transition.guard.is_none() {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::MachineExhaustivenessError,
                        span.clone(),
                        format!(
                            "machine '{}': duplicate transition for event '{}' in state '{}'",
                            md.name, transition.event_name, transition.source_state
                        ),
                    ));
                }
                covered.insert(key);
            }

            // Fix 6: Transition body validation with source-state field scoping.
            // Bind `state` as the machine type, and track the source state so
            // that `state.field` access resolves correctly for payload states.
            // (`state` rather than `self` to avoid confusion with actor self)
            self.env.push_scope();
            self.env.define(
                "state".to_string(),
                Ty::Machine {
                    name: md.name.clone(),
                },
                false,
            );
            // Bind `event` as the event companion enum type so that
            // `event.field` resolves for events with payload fields.
            let event_type_name = format!("{}Event", md.name);
            self.env.define(
                "event".to_string(),
                Ty::Named {
                    name: event_type_name,
                    args: vec![],
                },
                false,
            );
            if transition.source_state == "_" {
                self.current_machine_transition = Some((
                    md.name.clone(),
                    "_".to_string(),
                    transition.event_name.clone(),
                ));
            } else {
                self.current_machine_transition = Some((
                    md.name.clone(),
                    transition.source_state.clone(),
                    transition.event_name.clone(),
                ));
            }
            // Type-check guard expression if present
            if let Some((guard_expr, guard_span)) = &transition.guard {
                self.check_against(guard_expr, guard_span, &Ty::Bool);
            }
            self.synthesize(&transition.body.0, &transition.body.1);
            self.current_machine_transition = None;
            self.env.pop_scope();
        }

        // Check that every (state, event) pair is covered
        // If has_default is true, unhandled pairs default to self-transition
        for state in &state_names {
            for event in &event_names {
                let key = (state.to_string(), event.to_string());
                if !covered.contains(&key) && !wildcard_events.contains(*event) && !md.has_default {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::MachineExhaustivenessError,
                        span.clone(),
                        format!(
                            "machine '{}': state '{}' does not handle event '{}'",
                            md.name, state, event
                        ),
                    ));
                }
            }
        }
    }

    pub(super) fn register_actor_decl(&mut self, ad: &ActorDecl) {
        let mut fields = HashMap::new();
        let mut hole_vars = Vec::new();
        for field in &ad.fields {
            let field_ty = self.resolve_type_expr_tracking_holes(&field.ty.0, &mut hole_vars);
            fields.insert(field.name.clone(), field_ty);
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Actor,
            name: ad.name.clone(),
            type_params: vec![],
            fields,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: ad.doc_comment.clone(),
            is_indirect: false,
        };

        // Actors are always Send
        self.registry.register_actor(ad.name.clone());

        self.type_defs.insert(ad.name.clone(), type_def);
        self.record_type_def_inference_holes(&ad.name, hole_vars);
    }

    pub(super) fn register_wire_decl(&mut self, wd: &WireDecl) {
        // Wire types are similar to regular types but use string field types
        let mut fields = HashMap::new();
        for field in &wd.fields {
            let ty = Ty::from_name(&field.ty).unwrap_or_else(|| Ty::Named {
                name: field.ty.clone(),
                args: vec![],
            });
            fields.insert(field.name.clone(), ty);
        }

        let mut variants = HashMap::new();
        let mut hole_vars = Vec::new();
        for variant in &wd.variants {
            match &variant.kind {
                VariantKind::Unit => {
                    variants.insert(variant.name.clone(), VariantDef::Unit);
                }
                VariantKind::Tuple(fields) => {
                    let variant_tys = fields
                        .iter()
                        .map(|(te, _)| self.resolve_type_expr_tracking_holes(te, &mut hole_vars))
                        .collect();
                    variants.insert(variant.name.clone(), VariantDef::Tuple(variant_tys));
                }
                VariantKind::Struct(fields) => {
                    let variant_fields: Vec<(String, Ty)> = fields
                        .iter()
                        .map(|(name, (te, _))| {
                            (
                                name.clone(),
                                self.resolve_type_expr_tracking_holes(te, &mut hole_vars),
                            )
                        })
                        .collect();
                    variants.insert(variant.name.clone(), VariantDef::Struct(variant_fields));
                }
            }
        }

        let type_def = TypeDef {
            kind: match wd.kind {
                WireDeclKind::Struct => TypeDefKind::Struct,
                WireDeclKind::Enum => TypeDefKind::Enum,
            },
            name: wd.name.clone(),
            type_params: vec![],
            fields,
            variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };

        let field_types: Vec<_> = type_def.fields.values().cloned().collect();
        self.registry.register_type(wd.name.clone(), field_types);

        self.type_defs.insert(wd.name.clone(), type_def);
        self.record_type_def_inference_holes(&wd.name, hole_vars);
        self.register_wire_methods(&wd.name);
    }

    pub(super) fn trait_info_from_decl(tr: &TraitDecl) -> TraitInfo {
        let mut methods = Vec::new();
        let mut associated_types = Vec::new();
        for item in &tr.items {
            match item {
                TraitItem::Method(m) => methods.push(m.clone()),
                TraitItem::AssociatedType { name, default, .. } => {
                    associated_types.push(TraitAssociatedTypeInfo {
                        name: name.clone(),
                        default: default.clone(),
                    });
                }
            }
        }
        let type_params = tr
            .type_params
            .as_ref()
            .map(|params| params.iter().map(|p| p.name.clone()).collect())
            .unwrap_or_default();
        TraitInfo {
            methods,
            associated_types,
            type_params,
        }
    }

    pub(super) fn build_impl_alias_entries(
        &mut self,
        id: &ImplDecl,
    ) -> HashMap<String, ImplAliasEntry> {
        let mut entries = HashMap::new();
        let mut seen_spans: HashMap<String, Span> = HashMap::new();
        for alias in &id.type_aliases {
            if let Some(prev_span) = seen_spans.insert(alias.name.clone(), alias.ty.1.clone()) {
                self.errors.push(TypeError::duplicate_definition(
                    alias.ty.1.clone(),
                    &alias.name,
                    prev_span,
                ));
                continue;
            }
            entries.insert(
                alias.name.clone(),
                ImplAliasEntry {
                    expr: alias.ty.clone(),
                    resolved: None,
                    resolving: false,
                },
            );
        }
        if let Some(tb) = &id.trait_bound {
            if let Some(trait_info) = self.trait_defs.get(&tb.name) {
                for assoc in &trait_info.associated_types {
                    if entries.contains_key(&assoc.name) {
                        continue;
                    }
                    if let Some(default) = &assoc.default {
                        entries.insert(
                            assoc.name.clone(),
                            ImplAliasEntry {
                                expr: default.clone(),
                                resolved: None,
                                resolving: false,
                            },
                        );
                    }
                }
            }
        }
        entries
    }

    pub(super) fn enter_impl_scope(
        &mut self,
        id: &ImplDecl,
        span: &Span,
        type_name: Option<&str>,
        enforce: bool,
    ) -> bool {
        let Some(target_name) = type_name else {
            return false;
        };
        let entries = self.build_impl_alias_entries(id);
        if enforce {
            if let Some(tb) = &id.trait_bound {
                if let Some(info) = self.trait_defs.get(&tb.name) {
                    let missing: Vec<String> = info
                        .associated_types
                        .iter()
                        .filter(|assoc| !entries.contains_key(&assoc.name))
                        .map(|assoc| assoc.name.clone())
                        .collect();
                    let target_name = target_name.to_string();
                    let tb_name = tb.name.clone();
                    for name in missing {
                        self.report_error(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!(
                                "impl `{tb_name}` for `{target_name}` must define associated type `{name}`"
                            ),
                        );
                    }
                }
            }
        }
        self.impl_alias_scopes.push(ImplAliasScope {
            span: span.clone(),
            entries,
            missing_reported: HashSet::new(),
            report_missing: enforce,
        });
        true
    }

    pub(super) fn exit_impl_scope(&mut self) {
        self.impl_alias_scopes.pop();
    }

    pub(super) fn resolve_impl_associated_type(&mut self, alias: &str) -> Option<Ty> {
        let scope_index = self.impl_alias_scopes.len().checked_sub(1)?;
        let expr = {
            let scope = &mut self.impl_alias_scopes[scope_index];
            if let Some(entry) = scope.entries.get_mut(alias) {
                if let Some(resolved) = &entry.resolved {
                    return Some(resolved.clone());
                }
                if entry.resolving {
                    let should_report =
                        scope.report_missing && scope.missing_reported.insert(alias.to_string());
                    let err_span = entry.expr.1.clone();
                    if should_report {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &err_span,
                            format!(
                                "associated type `Self::{alias}` recursively references itself"
                            ),
                        );
                    }
                    return Some(Ty::Error);
                }
                entry.resolving = true;
                entry.expr.clone()
            } else {
                let should_report =
                    scope.report_missing && scope.missing_reported.insert(alias.to_string());
                let err_span = scope.span.clone();
                if should_report {
                    self.report_error(
                        TypeErrorKind::UndefinedType,
                        &err_span,
                        format!("type alias `Self::{alias}` is not defined in this impl"),
                    );
                }
                return Some(Ty::Error);
            }
        };
        let ty = self.resolve_type_expr(&expr.0);
        if let Some(scope) = self.impl_alias_scopes.get_mut(scope_index) {
            if let Some(entry) = scope.entries.get_mut(alias) {
                entry.resolving = false;
                entry.resolved = Some(ty.clone());
            }
        }
        Some(ty)
    }

    /// Pass 2: Collect function signatures
    pub(super) fn collect_functions(&mut self, program: &Program) {
        // Process module graph items first (if multi-module).
        // Skip the root module — its items are already in program.items and
        // will be processed below with current_module = None (bare names).
        if let Some(ref mg) = program.module_graph {
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let module_name = mod_id.path.join(".");
                    self.current_module = Some(module_name);
                    for (item, span) in &module.items {
                        self.collect_function_item(item, span);
                    }
                }
            }
        }

        // Process main module items.
        self.current_module = None;
        for (item, span) in &program.items {
            self.collect_function_item(item, span);
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression type checking requires many cases"
    )]
    pub(super) fn collect_function_item(&mut self, item: &Item, span: &Span) {
        match item {
            Item::Function(fd) => {
                let scoped_name = match &self.current_module {
                    Some(m) => format!("{m}.{}", fd.name),
                    None => fd.name.clone(),
                };
                if let Some(prev_span) = self.fn_def_spans.get(&scoped_name) {
                    self.errors.push(TypeError::duplicate_definition(
                        span.clone(),
                        &scoped_name,
                        prev_span.clone(),
                    ));
                } else {
                    self.fn_def_spans.insert(scoped_name, span.clone());
                }
                self.register_fn_sig(fd);
            }
            Item::Actor(ad) => {
                for rf in &ad.receive_fns {
                    self.register_receive_fn(&ad.name, rf);
                }
                for method in &ad.methods {
                    let method_name = format!("{}::{}", ad.name, method.name);
                    self.register_fn_sig_with_name(&method_name, method);
                }
            }
            Item::Impl(id) => {
                // Register impl methods with Type::method naming
                if let TypeExpr::Named {
                    name: type_name,
                    type_args,
                } = &id.target_type.0
                {
                    // Do NOT push generic_ctx here — type params like T should remain
                    // as Ty::Named so that substitute_named_param can replace them
                    // at method call sites with concrete type arguments.

                    // Set current_self_type for resolving `Self` in method parameters
                    let prev_self_type = self.current_self_type.take();
                    let self_type_args = type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|(te, _)| self.resolve_type_expr(te))
                                .collect()
                        })
                        .unwrap_or_default();
                    self.current_self_type = Some((type_name.clone(), self_type_args));
                    let scope_pushed =
                        self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                    for method in &id.methods {
                        self.register_impl_method(type_name, method, id.type_params.as_ref());
                    }

                    // Register default trait methods not overridden in this impl
                    if let Some(tb) = &id.trait_bound {
                        self.record_trait_impl(type_name, &tb.name);

                        let overridden: HashSet<&str> =
                            id.methods.iter().map(|m| m.name.as_str()).collect();
                        if let Some(trait_methods) = self.trait_defs.get(&tb.name) {
                            let defaults: Vec<_> = trait_methods
                                .methods
                                .iter()
                                .filter(|m| {
                                    m.body.is_some() && !overridden.contains(m.name.as_str())
                                })
                                .cloned()
                                .collect();
                            for m in defaults {
                                let method_key = format!("{type_name}::{}", m.name);
                                let skip = usize::from(
                                    m.params.first().is_some_and(|p| self.is_receiver_param(p)),
                                );
                                let param_names: Vec<String> =
                                    m.params.iter().skip(skip).map(|p| p.name.clone()).collect();
                                let params: Vec<Ty> = m
                                    .params
                                    .iter()
                                    .skip(skip)
                                    .map(|p| self.resolve_type_expr(&p.ty.0))
                                    .collect();
                                let return_type = m
                                    .return_type
                                    .as_ref()
                                    .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
                                let sig = FnSig {
                                    param_names: param_names.clone(),
                                    params: params.clone(),
                                    return_type: return_type.clone(),
                                    is_pure: m.is_pure,
                                    ..FnSig::default()
                                };
                                self.fn_sigs.insert(method_key, sig);
                                if let Some(td) = self.lookup_type_def_mut(type_name) {
                                    td.methods.insert(
                                        m.name.clone(),
                                        FnSig {
                                            param_names,
                                            params,
                                            return_type,
                                            is_pure: m.is_pure,
                                            ..FnSig::default()
                                        },
                                    );
                                }
                            }
                        }
                    }

                    // Restore previous self type
                    self.current_self_type = prev_self_type;
                    if scope_pushed {
                        self.exit_impl_scope();
                    }
                }
            }
            Item::TypeDecl(td) => {
                // Register methods defined inside struct/enum bodies
                for item in &td.body {
                    if let TypeBodyItem::Method(method) = item {
                        let method_key = format!("{}::{}", td.name, method.name);
                        self.register_fn_sig_with_name(&method_key, method);
                        let skip = usize::from(
                            method
                                .params
                                .first()
                                .is_some_and(|p| self.is_receiver_param(p)),
                        );
                        let param_names: Vec<String> = method
                            .params
                            .iter()
                            .skip(skip)
                            .map(|p| p.name.clone())
                            .collect();
                        let params: Vec<Ty> = method
                            .params
                            .iter()
                            .skip(skip)
                            .map(|p| self.resolve_type_expr(&p.ty.0))
                            .collect();
                        let return_type = method
                            .return_type
                            .as_ref()
                            .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
                        let is_async = method.is_async;
                        let method_name = method.name.clone();
                        let type_name = td.name.clone();
                        if let Some(type_def) = self.lookup_type_def_mut(&type_name) {
                            type_def.methods.insert(
                                method_name,
                                FnSig {
                                    param_names,
                                    params,
                                    return_type,
                                    is_async,
                                    is_pure: method.is_pure,
                                    ..FnSig::default()
                                },
                            );
                        }
                    }
                }
            }
            Item::ExternBlock(eb) => {
                self.register_extern_block(eb);
            }
            Item::Import(id) => {
                // Only track import spans for root-module items. Sub-module spans are
                // byte offsets into their own source file and cannot be attributed to
                // the root file's source text for diagnostics.
                let import_span = if self.current_module.is_none() {
                    Some(span)
                } else {
                    None
                };
                self.register_import(id, import_span);
            }
            _ => {}
        }
    }

    pub(super) fn register_fn_sig(&mut self, fd: &FnDecl) {
        self.register_fn_sig_with_name(&fd.name, fd);
    }

    #[expect(
        clippy::unused_self,
        reason = "method signature is part of the checker API"
    )]
    pub(super) fn collect_type_param_bounds(
        &self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
    ) -> HashMap<String, Vec<String>> {
        let mut bounds = HashMap::new();
        let mut declared = HashSet::new();
        if let Some(params) = type_params {
            for param in params {
                declared.insert(param.name.clone());
                if param.bounds.is_empty() {
                    continue;
                }
                let entry = bounds.entry(param.name.clone()).or_default();
                for bound in &param.bounds {
                    Self::push_unique_bound(entry, &bound.name);
                }
            }
        }
        if let Some(wc) = where_clause {
            for predicate in &wc.predicates {
                if let TypeExpr::Named { name, type_args } = &predicate.ty.0 {
                    if !declared.contains(name) {
                        continue;
                    }
                    if type_args.as_ref().is_some_and(|args| !args.is_empty()) {
                        continue;
                    }
                    let entry = bounds.entry(name.clone()).or_default();
                    for bound in &predicate.bounds {
                        Self::push_unique_bound(entry, &bound.name);
                    }
                }
            }
        }
        bounds
    }

    pub(super) fn push_unique_bound(entry: &mut Vec<String>, bound: &str) {
        if !entry.iter().any(|b| b == bound) {
            entry.push(bound.to_string());
        }
    }

    /// Check whether a parameter is the receiver (i.e. the implicit first
    /// parameter of an impl/trait method).  A parameter is a receiver if its
    /// declared type matches `Self` or the current impl target type.
    /// Note: name-based matching (`p.name == "self"`) has been intentionally
    /// removed — receivers are identified by type, not by name.
    pub(super) fn is_receiver_param(&mut self, p: &Param) -> bool {
        match &p.ty.0 {
            TypeExpr::Named { name, type_args } => {
                if name == "Self" {
                    return true;
                }
                // Clone to avoid borrowing self while we resolve type args.
                let impl_target = self.current_self_type.clone();
                if let Some((self_name, self_type_args)) = impl_target {
                    if name != &self_name {
                        return false;
                    }
                    // Name matches — also verify generic arguments match the
                    // impl target so that e.g. `impl Box<int>` rejects a
                    // parameter typed `Box<string>`.
                    let param_args: Vec<Ty> = type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|(te, _)| self.resolve_type_expr(te))
                                .collect()
                        })
                        .unwrap_or_default();
                    param_args == self_type_args
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub(super) fn register_fn_sig_with_name(&mut self, name: &str, fd: &FnDecl) {
        // Only filter out the receiver for methods (Type::method), not free
        // functions that happen to have a parameter named `self`.
        let is_method = name.contains("::");
        let skip = if is_method {
            usize::from(fd.params.first().is_some_and(|p| self.is_receiver_param(p)))
        } else {
            0
        };
        let mut hole_vars = Vec::new();
        let param_names = fd
            .params
            .iter()
            .skip(skip)
            .map(|p| p.name.clone())
            .collect();
        let params = fd
            .params
            .iter()
            .skip(skip)
            .map(|p| self.resolve_type_expr_tracking_holes(&p.ty.0, &mut hole_vars))
            .collect();
        let declared_return = fd.return_type.as_ref().map_or(Ty::Unit, |(te, _)| {
            self.resolve_type_expr_tracking_holes(te, &mut hole_vars)
        });
        // Wrap return type for generator functions
        let return_type = if fd.is_generator && fd.is_async {
            Ty::async_generator(declared_return)
        } else if fd.is_generator {
            Ty::generator(declared_return, Ty::Unit)
        } else {
            declared_return
        };

        let sig = FnSig {
            type_params: fd.type_params.as_ref().map_or(vec![], |params| {
                params.iter().map(|p| p.name.clone()).collect()
            }),
            type_param_bounds: self
                .collect_type_param_bounds(fd.type_params.as_ref(), fd.where_clause.as_ref()),
            param_names,
            params,
            return_type,
            is_async: fd.is_async,
            is_pure: fd.is_pure,
            doc_comment: fd.doc_comment.clone(),
            ..FnSig::default()
        };

        self.fn_sigs.insert(name.to_string(), sig);
        self.record_fn_sig_inference_holes(name, hole_vars);
    }

    /// Register an impl method on a type's method table and `fn_sigs`.
    ///
    /// `impl_type_params` carries the enclosing `impl<T, U, …>` type
    /// parameter names so they are included in the resulting `FnSig`.
    ///
    /// **Important**: the caller must have already pushed the impl-level type
    /// params into `self.generic_ctx` so that type resolution sees them.
    ///
    /// Returns the built `FnSig` for callers that need to insert it
    /// on additional type names (e.g., qualified aliases).
    pub(super) fn register_impl_method(
        &mut self,
        type_name: &str,
        method: &FnDecl,
        impl_type_params: Option<&Vec<TypeParam>>,
    ) -> FnSig {
        let method_key = format!("{type_name}::{}", method.name);
        self.register_fn_sig_with_name(&method_key, method);
        let skip = usize::from(
            method
                .params
                .first()
                .is_some_and(|p| self.is_receiver_param(p)),
        );
        let params: Vec<Ty> = method
            .params
            .iter()
            .skip(skip)
            .map(|p| self.resolve_type_expr(&p.ty.0))
            .collect();
        let return_type = method
            .return_type
            .as_ref()
            .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
        let param_names: Vec<String> = method
            .params
            .iter()
            .skip(skip)
            .map(|p| p.name.clone())
            .collect();

        // Collect type param names: impl-level + method-level.
        let mut all_type_params: Vec<String> = impl_type_params
            .map(|tps| tps.iter().map(|tp| tp.name.clone()).collect())
            .unwrap_or_default();
        if let Some(method_tps) = &method.type_params {
            all_type_params.extend(method_tps.iter().map(|tp| tp.name.clone()));
        }

        let type_param_bounds =
            self.collect_type_param_bounds(impl_type_params, method.where_clause.as_ref());

        let sig = FnSig {
            type_params: all_type_params,
            type_param_bounds,
            param_names,
            params,
            return_type,
            is_async: method.is_async,
            is_pure: method.is_pure,
            ..FnSig::default()
        };
        if let Some(td) = self.lookup_type_def_mut(type_name) {
            td.methods.insert(method.name.clone(), sig.clone());
        }
        sig
    }

    pub(super) fn record_trait_impl(&mut self, type_name: &str, trait_name: &str) {
        self.trait_impls_set
            .insert((type_name.to_string(), trait_name.to_string()));
    }

    pub(super) fn register_receive_fn(&mut self, actor_name: &str, rf: &ReceiveFnDecl) {
        let mut generic_bindings = std::collections::HashMap::new();
        if let Some(type_params) = &rf.type_params {
            for tp in type_params {
                generic_bindings.insert(
                    tp.name.clone(),
                    Ty::Named {
                        name: tp.name.clone(),
                        args: vec![],
                    },
                );
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        let mut hole_vars = Vec::new();
        let param_names = rf.params.iter().map(|p| p.name.clone()).collect();
        let params = rf
            .params
            .iter()
            .map(|p| self.resolve_type_expr_tracking_holes(&p.ty.0, &mut hole_vars))
            .collect();
        let declared_return_type = rf.return_type.as_ref().map_or(Ty::Unit, |(te, _)| {
            self.resolve_type_expr_tracking_holes(te, &mut hole_vars)
        });
        let return_type = if rf.is_generator {
            Ty::stream(declared_return_type)
        } else {
            declared_return_type
        };

        if rf.type_params.as_ref().is_some_and(|tp| !tp.is_empty()) {
            self.generic_ctx.pop();
        }

        let type_param_bounds =
            self.collect_type_param_bounds(rf.type_params.as_ref(), rf.where_clause.as_ref());
        let sig = FnSig {
            type_params: rf.type_params.as_ref().map_or(vec![], |params| {
                params.iter().map(|p| p.name.clone()).collect()
            }),
            type_param_bounds,
            param_names,
            params,
            return_type,
            is_pure: rf.is_pure,
            ..FnSig::default()
        };

        let method_name = format!("{}::{}", actor_name, rf.name);
        self.record_fn_sig_inference_holes(&method_name, hole_vars);
        self.fn_sigs.insert(method_name, sig);
    }

    pub(super) fn register_extern_block(&mut self, eb: &ExternBlock) {
        for f in &eb.functions {
            let mut hole_vars = Vec::new();
            let param_names = f.params.iter().map(|p| p.name.clone()).collect();
            let params = f
                .params
                .iter()
                .map(|p| self.resolve_type_expr_tracking_holes(&p.ty.0, &mut hole_vars))
                .collect();
            let return_type = f.return_type.as_ref().map_or(Ty::Unit, |(te, _)| {
                self.resolve_type_expr_tracking_holes(te, &mut hole_vars)
            });
            let sig = FnSig {
                param_names,
                params,
                return_type,
                ..FnSig::default()
            };
            self.record_fn_sig_inference_holes(&f.name, hole_vars);
            self.fn_sigs.insert(f.name.clone(), sig);
            self.unsafe_functions.insert(f.name.clone());
        }

        // Register codegen-intercepted channel functions that use
        // out-parameter ABI and cannot appear in extern blocks.
        // Without these entries standalone `hew check` on channel.hew
        // reports "undefined function" for recv/try_recv calls.
        self.register_channel_recv_builtins();
    }

    /// Registers synthetic `fn_sigs` entries for channel `recv`/`try_recv`
    /// functions whose calling convention is handled entirely by codegen.
    ///
    /// These functions use an out-parameter ABI for `Option<T>` and must
    /// NOT be declared in `extern "C"` blocks — the codegen intercepts
    /// them by name and emits custom MLIR.  We register them here so the
    /// type checker can resolve calls inside `unsafe` blocks.
    ///
    /// Only activates when we're actually in the channel module: the
    /// local module must define `Receiver` AND the extern block must
    /// have already registered `hew_channel_send`.
    pub(super) fn register_channel_recv_builtins(&mut self) {
        if !self.local_type_defs.contains("Receiver")
            || !self.fn_sigs.contains_key("hew_channel_send")
        {
            return;
        }

        let receiver_ty = Ty::Named {
            name: "Receiver".to_string(),
            args: vec![],
        };

        let builtins: &[(&str, Ty)] = &[
            ("hew_channel_recv", Ty::option(Ty::String)),
            ("hew_channel_recv_int", Ty::option(Ty::I64)),
            ("hew_channel_try_recv", Ty::option(Ty::String)),
            ("hew_channel_try_recv_int", Ty::option(Ty::I64)),
        ];

        for (name, ret_ty) in builtins {
            if self.fn_sigs.contains_key(*name) {
                continue;
            }
            let sig = FnSig {
                param_names: vec!["rx".to_string()],
                params: vec![receiver_ty.clone()],
                return_type: ret_ty.clone(),
                ..FnSig::default()
            };
            self.fn_sigs.insert((*name).to_string(), sig);
            self.unsafe_functions.insert((*name).to_string());
        }
    }

    pub(super) fn register_import(&mut self, decl: &ImportDecl, import_span: Option<&Span>) {
        let module_path = decl.path.join("::");

        // Try to load from the registry first, keeping any error detail owned so the
        // `self.module_registry` borrow ends before we mutate `self.errors`.
        let load_error_detail: Option<String> = match self.module_registry.load(&module_path) {
            Ok(info) => {
                // Clone all data from ModuleInfo before mutating self, because
                // info borrows from self.module_registry.
                let functions = info.functions.clone();
                let wrapper_fns = info.wrapper_fns.clone();
                let clean_names = info.clean_names.clone();
                let handle_types = info.handle_types.clone();
                let drop_types = info.drop_types.clone();

                let short = module_path
                    .rsplit("::")
                    .next()
                    .unwrap_or(&module_path)
                    .to_string();

                // Register extern C function signatures
                for (name, params, ret) in functions {
                    let accepts_kwargs = module_path == "std::misc::log"
                        && Self::LOG_KWARGS_FUNCTIONS.contains(&name.as_str());
                    let sig = FnSig {
                        params,
                        return_type: ret,
                        accepts_kwargs,
                        ..FnSig::default()
                    };
                    self.unsafe_functions.insert(name.clone());
                    self.fn_sigs.insert(name, sig);
                }

                // Register wrapper pub fn signatures
                for (name, params, ret) in wrapper_fns {
                    let accepts_kwargs = module_path == "std::misc::log"
                        && Self::LOG_KWARGS_FUNCTIONS.contains(&name.as_str());
                    let sig = FnSig {
                        params,
                        return_type: ret,
                        accepts_kwargs,
                        ..FnSig::default()
                    };
                    self.fn_sigs.insert(name, sig);
                }

                // Register module and clean names
                self.modules.insert(short.clone());
                if let Some(span) = import_span {
                    self.import_spans.insert(short.clone(), span.clone());
                }
                for (method, c_symbol) in &clean_names {
                    // Prefer the wrapper function's own signature (registered under
                    // the method name) over the extern C function's signature.
                    // E.g. `log.setup()` should have 0 params (the wrapper's sig),
                    // not 1 param (the extern `hew_log_set_level(level)` sig).
                    let wrapper_sig = self.fn_sigs.get(method.as_str()).cloned();
                    let sig = wrapper_sig
                        .clone()
                        .or_else(|| self.fn_sigs.get(c_symbol.as_str()).cloned());
                    if let Some(sig) = sig {
                        let key = format!("{short}.{method}");
                        self.fn_sigs.insert(key.clone(), sig);
                        if wrapper_sig.is_none() {
                            self.unsafe_functions.insert(key);
                        }
                    }
                }

                // Register handle type names so they can be used in type annotations
                for type_name in &handle_types {
                    self.known_types.insert(type_name.clone());
                }

                // Populate TraitRegistry with handle/drop types
                for ht in &handle_types {
                    self.registry.register_handle_type(ht.clone());
                }
                for dt in &drop_types {
                    self.registry.register_drop_type(dt.clone());
                }

                // Process resolved Hew source items from stdlib modules that ship
                // alongside their C/Rust bindings so trait methods stay visible.
                if let Some(ref resolved_items) = decl.resolved_items {
                    if !self.stdlib_hew_module_already_registered(&module_path) {
                        self.register_stdlib_hew_items(&short, resolved_items);
                    }
                }
                return;
            }
            Err(ModuleError::NotFound { .. }) => {
                Some("module not found in any search path".to_string())
            }
            Err(ModuleError::ParseError { ref file_path, .. }) => Some(format!(
                "module file `{}` has parse errors",
                file_path.display()
            )),
        };
        // `self.module_registry` borrow is released here.

        // --- User module path ---
        if let Some(ref resolved_items) = decl.resolved_items {
            if decl.path.is_empty() {
                if self.flat_file_import_already_registered(decl) {
                    return;
                }
                // File imports register top-level names without a module namespace.
                self.register_file_import_items(resolved_items);
            } else {
                let short = decl.path.last().expect("import path is non-empty").clone();
                self.modules.insert(short.clone());
                self.user_modules.insert(short.clone());
                if let Some(span) = import_span {
                    self.import_spans.insert(short.clone(), span.clone());
                }
                self.register_user_module(&short, resolved_items, &decl.spec);
            }
        } else if let Some(error) =
            Self::unresolved_import_error(decl, import_span, &module_path, load_error_detail)
        {
            self.errors.push(error);
        }
    }

    fn unresolved_import_error(
        decl: &ImportDecl,
        import_span: Option<&Span>,
        module_path: &str,
        load_error_detail: Option<String>,
    ) -> Option<TypeError> {
        let detail = if decl.path.is_empty() {
            Some("file import was not resolved before type checking".to_string())
        } else {
            load_error_detail
        }?;
        let span = import_span.cloned().unwrap_or(0..0);
        let import_target = if decl.path.is_empty() {
            decl.file_path.as_deref().unwrap_or("<file import>")
        } else {
            module_path
        };
        Some(TypeError::unresolved_import(span, import_target, &detail))
    }

    /// Determine whether a name should be imported unqualified based on the `ImportSpec`.
    #[expect(clippy::ref_option, reason = "avoids cloning the option contents")]
    pub(super) fn should_import_name(name: &str, spec: &Option<ImportSpec>) -> bool {
        match spec {
            None => false,                  // bare import → qualified only
            Some(ImportSpec::Glob) => true, // import foo::*; → everything
            Some(ImportSpec::Names(names)) => names
                .iter()
                .any(|n| n.name == name || n.alias.as_deref() == Some(name)),
        }
    }

    /// Resolve the binding name for an imported symbol, applying any alias.
    #[expect(clippy::ref_option, reason = "avoids cloning the option contents")]
    pub(super) fn resolve_import_name(spec: &Option<ImportSpec>, name: &str) -> Option<String> {
        match spec {
            Some(ImportSpec::Names(names)) => names
                .iter()
                .find(|n| n.name == name)
                .map(|n| n.alias.as_deref().unwrap_or(&n.name).to_string()),
            Some(ImportSpec::Glob) => Some(name.to_string()),
            None => None,
        }
    }

    /// Register type declarations, trait declarations, and impl blocks from
    /// stdlib modules that have Hew source files. This makes trait methods
    /// (e.g. bench.Suite.add) visible to the type checker.
    pub(super) fn register_stdlib_hew_items(
        &mut self,
        module_short: &str,
        items: &[Spanned<Item>],
    ) {
        // Pass 1: Register types, traits, and functions first
        for (item, span) in items {
            match item {
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(&td.name, span) {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.known_types.insert(td.name.clone());
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(&tr.name, span) {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);
                    self.trait_defs.insert(tr.name.clone(), info.clone());
                    let qualified = format!("{module_short}.{}", tr.name);
                    self.trait_defs.insert(qualified, info);
                }
                Item::Function(fd) => {
                    if !fd.visibility.is_pub() {
                        continue;
                    }
                    let qualified = format!("{module_short}.{}", fd.name);
                    if !self.fn_sigs.contains_key(&qualified) {
                        let sig = self.build_fn_sig_from_decl(fd);
                        self.fn_sigs.insert(qualified, sig);
                    }
                }
                Item::Actor(ad) => {
                    if !self.register_type_namespace_name(&ad.name, span) {
                        continue;
                    }
                    self.register_actor_base(ad);
                }
                _ => {}
            }
        }
        // Pass 2: Register impl methods (after types exist)
        for (item, span) in items {
            if let Item::Impl(id) = item {
                if let TypeExpr::Named {
                    name: type_name,
                    type_args,
                } = &id.target_type.0
                {
                    // Set current_self_type for resolving `Self` in method parameters
                    let prev_self_type = self.current_self_type.take();
                    let self_type_args = type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|(te, _)| self.resolve_type_expr(te))
                                .collect()
                        })
                        .unwrap_or_default();
                    self.current_self_type = Some((type_name.clone(), self_type_args));
                    let scope_pushed =
                        self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                    for method in &id.methods {
                        let sig =
                            self.register_impl_method(type_name, method, id.type_params.as_ref());
                        // Also register on qualified type name
                        let qualified_type = format!("{module_short}.{type_name}");
                        if let Some(td) = self.lookup_type_def_mut(&qualified_type) {
                            td.methods.insert(method.name.clone(), sig);
                        }
                    }
                    if let Some(tb) = &id.trait_bound {
                        self.record_trait_impl(type_name, &tb.name);
                    }

                    // Restore previous self type
                    self.current_self_type = prev_self_type;
                    if scope_pushed {
                        self.exit_impl_scope();
                    }
                }
            }
        }
        // Pass 3: Create qualified type aliases (after impls have been registered)
        for (item, _span) in items {
            match item {
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    self.register_qualified_type_alias(module_short, &td.name);
                }
                Item::Actor(ad) => {
                    self.register_qualified_type_alias(module_short, &ad.name);
                }
                _ => {}
            }
        }
    }

    /// Register items from a file-based import as top-level names (no module namespace).
    pub(super) fn register_file_import_items(&mut self, items: &[Spanned<Item>]) {
        let mut current_import_pub_spans = HashMap::new();
        let mut skipped_type_names = HashSet::new();

        for (item, span) in items {
            match item {
                Item::Function(fd) => {
                    if !fd.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_pub_name(
                        &mut current_import_pub_spans,
                        &fd.name,
                        span,
                    ) {
                        continue;
                    }
                    let sig = self.build_fn_sig_from_decl(fd);
                    self.fn_sigs.insert(fd.name.clone(), sig);
                }
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_pub_name(
                        &mut current_import_pub_spans,
                        &cd.name,
                        span,
                    ) {
                        continue;
                    }
                    let ty = self.resolve_type_expr(&cd.ty.0);
                    self.env.define(cd.name.clone(), ty, false);
                }
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &td.name,
                        span,
                    ) {
                        skipped_type_names.insert(td.name.clone());
                        continue;
                    }
                    self.register_type_decl(td);
                    self.known_types.insert(td.name.clone());
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &tr.name,
                        span,
                    ) {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);
                    self.trait_defs.insert(tr.name.clone(), info);
                }
                Item::Actor(ad) => {
                    if !ad.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_flat_file_import_type_name(
                        &mut current_import_pub_spans,
                        &ad.name,
                        span,
                    ) {
                        continue;
                    }
                    self.register_actor_base(ad);
                }
                Item::Impl(id) => {
                    if let TypeExpr::Named {
                        name: type_name, ..
                    } = &id.target_type.0
                    {
                        if skipped_type_names.contains(type_name) {
                            continue;
                        }
                        for method in &id.methods {
                            if !method.visibility.is_pub() {
                                continue;
                            }
                            self.register_impl_method(type_name, method, id.type_params.as_ref());
                        }
                        // Track trait implementations
                        if let Some(tb) = &id.trait_bound {
                            self.record_trait_impl(type_name, &tb.name);
                        }
                    }
                }
                _ => {}
            }
        }

        self.flat_file_import_pub_spans
            .extend(current_import_pub_spans);
    }

    pub(super) fn register_flat_file_import_pub_name(
        &mut self,
        current_import_pub_spans: &mut HashMap<String, Span>,
        name: &str,
        span: &Span,
    ) -> bool {
        if let Some(prev_span) = self.flat_file_import_pub_spans.get(name) {
            self.errors.push(TypeError::duplicate_definition(
                span.clone(),
                name,
                prev_span.clone(),
            ));
            return false;
        }

        match current_import_pub_spans.entry(name.to_string()) {
            Entry::Occupied(_) => {}
            Entry::Vacant(entry) => {
                entry.insert(span.clone());
            }
        }

        true
    }

    pub(super) fn register_flat_file_import_type_name(
        &mut self,
        current_import_pub_spans: &mut HashMap<String, Span>,
        name: &str,
        span: &Span,
    ) -> bool {
        self.register_flat_file_import_pub_name(current_import_pub_spans, name, span)
            && self.register_type_namespace_name(name, span)
    }

    fn flat_file_import_already_registered(&mut self, decl: &ImportDecl) -> bool {
        let import_source = decl
            .resolved_source_paths
            .first()
            .cloned()
            .or_else(|| decl.file_path.as_ref().map(std::path::PathBuf::from));
        let Some(import_source) = import_source else {
            return false;
        };
        !self
            .registered_flat_file_import_sources
            .insert(import_source)
    }

    fn stdlib_hew_module_already_registered(&mut self, module_path: &str) -> bool {
        !self
            .registered_stdlib_hew_modules
            .insert(module_path.to_string())
    }

    /// Register items from a user module under the module's namespace.
    #[expect(
        clippy::too_many_lines,
        clippy::ref_option,
        reason = "statement type checking requires many cases"
    )]
    pub(super) fn register_user_module(
        &mut self,
        module_short: &str,
        items: &[Spanned<Item>],
        spec: &Option<ImportSpec>,
    ) {
        for (item, span) in items {
            match item {
                Item::Function(fd) => {
                    // Skip non-pub functions (enforce visibility)
                    if !fd.visibility.is_pub() {
                        continue;
                    }

                    let sig = self.build_fn_sig_from_decl(fd);
                    let qualified = format!("{module_short}.{}", fd.name);
                    self.fn_sigs.insert(qualified, sig.clone());

                    // If named import or glob, also register unqualified (using alias if present)
                    if Self::should_import_name(&fd.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &fd.name)
                            .unwrap_or_else(|| fd.name.clone());
                        self.fn_sigs.insert(binding_name.clone(), sig);
                        self.unqualified_to_module
                            .insert(binding_name, module_short.to_string());
                    }
                }
                Item::TypeDecl(td) => {
                    if !td.visibility.is_pub() {
                        continue;
                    }
                    if !self.register_type_namespace_name(&td.name, span) {
                        continue;
                    }
                    self.register_type_decl(td);
                    self.register_qualified_type_alias(module_short, &td.name);
                    self.known_types.insert(td.name.clone());
                }
                Item::Trait(tr) => {
                    if !tr.visibility.is_pub() {
                        continue;
                    }
                    let info = Self::trait_info_from_decl(tr);
                    let import_binding = if Self::should_import_name(&tr.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &tr.name)
                            .unwrap_or_else(|| tr.name.clone());
                        if self.register_type_namespace_name(&binding_name, span) {
                            Some(binding_name)
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    // Register under qualified name (e.g. "mymod.Drawable")
                    let qualified = format!("{module_short}.{}", tr.name);
                    self.trait_defs.insert(qualified.clone(), info.clone());

                    // Record super-trait relationships for both qualified and unqualified
                    if let Some(supers) = &tr.super_traits {
                        let super_names: Vec<String> =
                            supers.iter().map(|s| s.name.clone()).collect();
                        self.trait_super.insert(qualified, super_names.clone());
                        if let Some(binding_name) = import_binding.as_ref() {
                            self.trait_super.insert(binding_name.clone(), super_names);
                        }
                    }

                    // If glob or named import, also register unqualified (using alias if present)
                    if let Some(binding_name) = import_binding {
                        self.trait_defs.insert(binding_name.clone(), info.clone());
                        self.unqualified_to_module
                            .insert(binding_name, module_short.to_string());
                    }
                }
                Item::Const(cd) => {
                    if !cd.visibility.is_pub() {
                        continue;
                    }
                    let ty = self.resolve_type_expr(&cd.ty.0);
                    let qualified = format!("{module_short}.{}", cd.name);
                    self.env.define(qualified, ty.clone(), false);
                    if Self::should_import_name(&cd.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &cd.name)
                            .unwrap_or_else(|| cd.name.clone());
                        self.env.define(binding_name, ty, false);
                    }
                }
                Item::Impl(id) => {
                    // Register impl methods for types defined in the module
                    if let TypeExpr::Named {
                        name: type_name,
                        type_args,
                    } = &id.target_type.0
                    {
                        // Set current_self_type for resolving `Self` in method parameters
                        let prev_self_type = self.current_self_type.take();
                        let self_type_args = type_args
                            .as_ref()
                            .map(|args| {
                                args.iter()
                                    .map(|(te, _)| self.resolve_type_expr(te))
                                    .collect()
                            })
                            .unwrap_or_default();
                        self.current_self_type = Some((type_name.clone(), self_type_args));
                        let scope_pushed =
                            self.enter_impl_scope(id, span, Some(type_name.as_str()), false);

                        for method in &id.methods {
                            if !method.visibility.is_pub() {
                                continue;
                            }
                            self.register_impl_method(type_name, method, id.type_params.as_ref());
                        }
                        if let Some(tb) = &id.trait_bound {
                            self.record_trait_impl(type_name, &tb.name);
                        }

                        // Restore previous self type
                        self.current_self_type = prev_self_type;
                        if scope_pushed {
                            self.exit_impl_scope();
                        }
                    }
                }
                Item::Actor(ad) => {
                    if !self.register_type_namespace_name(&ad.name, span) {
                        continue;
                    }
                    self.register_actor_base(ad);
                    self.register_qualified_type_alias(module_short, &ad.name);
                    // If named import or glob, also register unqualified
                    if Self::should_import_name(&ad.name, spec) {
                        let binding_name = Self::resolve_import_name(spec, &ad.name)
                            .unwrap_or_else(|| ad.name.clone());
                        self.unqualified_to_module
                            .insert(binding_name, module_short.to_string());
                    }
                }
                _ => {}
            }
        }
    }

    /// Build a `FnSig` from a function declaration (used for user module registration).
    pub(super) fn build_fn_sig_from_decl(&mut self, fd: &FnDecl) -> FnSig {
        let param_names = fd.params.iter().map(|p| p.name.clone()).collect();
        let params = fd
            .params
            .iter()
            .map(|p| self.resolve_type_expr(&p.ty.0))
            .collect();
        let declared_return = fd
            .return_type
            .as_ref()
            .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
        let type_params = fd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(fd.type_params.as_ref(), fd.where_clause.as_ref());
        let return_type = if fd.is_generator && fd.is_async {
            Ty::async_generator(declared_return)
        } else if fd.is_generator {
            Ty::generator(declared_return, Ty::Unit)
        } else {
            declared_return
        };
        FnSig {
            type_params,
            type_param_bounds,
            param_names,
            params,
            return_type,
            is_async: fd.is_async,
            is_pure: fd.is_pure,
            doc_comment: fd.doc_comment.clone(),
            ..FnSig::default()
        }
    }

    /// Register an actor's core items: the type declaration, receive functions,
    /// and inline methods.  This block is identical across all three import
    /// registration paths; only the qualified-alias and unqualified-binding
    /// steps differ and are therefore kept in each caller.
    pub(super) fn register_actor_base(&mut self, ad: &ActorDecl) {
        self.register_actor_decl(ad);
        self.known_types.insert(ad.name.clone());
        for rf in &ad.receive_fns {
            self.register_receive_fn(&ad.name, rf);
        }
        for method in &ad.methods {
            let method_name = format!("{}::{}", ad.name, method.name);
            self.register_fn_sig_with_name(&method_name, method);
        }
    }

    /// Insert a qualified alias (`module_short.Name`) for a type that has
    /// already been registered under its bare name.
    pub(super) fn register_qualified_type_alias(&mut self, module_short: &str, name: &str) {
        let qualified = format!("{module_short}.{name}");
        if let Some(def) = self.type_defs.get(name).cloned() {
            self.type_defs.insert(qualified, def);
        }
    }
}
