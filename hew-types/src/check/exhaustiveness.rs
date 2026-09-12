//! Match exhaustiveness: one column-wise coverage authority.
//!
//! A `match` is exhaustive when its arms leave no value of the scrutinee type
//! uncovered, and a non-exhaustive `match` is an error for every scrutinee
//! (D464): a run-time fallthrough reaches no arm at all.
//!
//! Coverage is decided on a pattern matrix — one row per unguarded arm leaf
//! (or-patterns flattened), one column per live slot. Column zero is split
//! over its type's constructor space: `bool`, enum variants, and the single
//! tuple/record constructor are enumerable, so when the rows already name
//! every constructor each one is recursed into with the columns it opens.
//! Integers, floats, strings, chars and opaque types have no enumerable
//! space, so only a wildcard or binding row covers them. When the rows leave
//! a constructor unnamed, coverage falls to the rows that admit any value —
//! the wildcard rows — and the unnamed constructors become the missing
//! shapes.
//!
//! The result is the missing shapes rendered as arm patterns, so both the
//! diagnostic text and its editor quick fix can name what to add.

use super::patterns::{nominal_path_leaf, substitute_pattern_field_ty, VariantPayloadShape};
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use hew_parser::ast::{NominalPatternPayload, PatternField};

/// The wildcard a specialised row borrows for a slot its pattern left open.
static WILDCARD: Pattern = Pattern::Wildcard;

/// One row of the pattern matrix: one pattern per live column.
type Row<'pattern> = Vec<&'pattern Pattern>;

/// A constructor of one column's value space.
#[derive(Clone, PartialEq, Eq)]
enum Ctor {
    Bool(bool),
    /// A named variant of an enum, `Option`, `Result` or machine state.
    Variant(String),
    /// The single constructor of a tuple or record type.
    Product,
}

/// A constructor together with the columns it opens and how it is spelled.
struct CtorInfo {
    ctor: Ctor,
    /// Slot types in match order.
    slots: Vec<Ty>,
    /// Field names when the payload is a record shape, in declaration order.
    fields: Option<Vec<String>>,
    /// Surface head of a record product (`Point`); a tuple product has none.
    head: Option<String>,
}

impl CtorInfo {
    fn variant(name: String, shape: &VariantPayloadShape) -> Self {
        let ctor = Ctor::Variant(name);
        match shape {
            VariantPayloadShape::Unit => Self {
                ctor,
                slots: Vec::new(),
                fields: None,
                head: None,
            },
            VariantPayloadShape::Tuple(types) => Self {
                ctor,
                slots: types.clone(),
                fields: None,
                head: None,
            },
            VariantPayloadShape::Struct(fields) => Self {
                ctor,
                slots: fields.iter().map(|(_, ty)| ty.clone()).collect(),
                fields: Some(fields.iter().map(|(name, _)| name.clone()).collect()),
                head: None,
            },
        }
    }

    /// Spell this constructor as an arm pattern over its slot witnesses.
    fn render(&self, slots: &[String]) -> String {
        let record = |head: &str| {
            let body = self.fields.as_ref().map_or_else(String::new, |fields| {
                fields
                    .iter()
                    .zip(slots)
                    .map(|(field, witness)| format!("{field}: {witness}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            });
            format!("{head} {{ {body} }}")
        };
        match &self.ctor {
            Ctor::Bool(value) => value.to_string(),
            Ctor::Variant(name) if self.slots.is_empty() => name.clone(),
            Ctor::Variant(name) if self.fields.is_some() => record(name),
            Ctor::Variant(name) => format!("{name}({})", slots.join(", ")),
            Ctor::Product => match &self.head {
                Some(head) => record(head),
                None => format!("({})", slots.join(", ")),
            },
        }
    }

    /// This constructor with every slot left open.
    fn open(&self) -> String {
        self.render(&vec!["_".to_string(); self.slots.len()])
    }
}

fn short_name(name: &str) -> &str {
    name.rsplit("::").next().unwrap_or(name)
}

impl Checker {
    /// The arm shapes `arms` leaves uncovered for `scrutinee_ty`, rendered as
    /// arm patterns. An empty result means the match is exhaustive.
    pub(super) fn missing_match_shapes(&self, arms: &[MatchArm], scrutinee_ty: &Ty) -> Vec<String> {
        let leaves = Self::unguarded_leaf_patterns(arms);
        let rows: Vec<Row<'_>> = leaves.iter().map(|pattern| vec![*pattern]).collect();
        let mut shapes = Vec::new();
        for witness in self.missing_rows(&rows, std::slice::from_ref(scrutinee_ty)) {
            let shape = witness
                .into_iter()
                .next()
                .unwrap_or_else(|| "_".to_string());
            if !shapes.contains(&shape) {
                shapes.push(shape);
            }
        }
        shapes
    }

    /// The witnesses `rows` leaves uncovered over the column types `tys`.
    /// Each witness carries one rendered pattern per column.
    fn missing_rows(&self, rows: &[Row<'_>], tys: &[Ty]) -> Vec<Vec<String>> {
        let Some((column_ty, rest_tys)) = tys.split_first() else {
            // No columns left: an empty matrix covers nothing, any row covers
            // everything.
            return if rows.is_empty() {
                vec![Vec::new()]
            } else {
                Vec::new()
            };
        };
        let space = self.ctor_space(column_ty);
        if space.as_ref().is_some_and(Vec::is_empty) {
            // An uninhabited column has no reachable value to cover.
            return Vec::new();
        }
        let space = space.unwrap_or_default();
        let named: Vec<bool> = space
            .iter()
            .map(|info| {
                rows.iter().any(|row| {
                    !self.head_admits_anything(row[0], column_ty)
                        && !self.specialize(row[0], column_ty, info).is_empty()
                })
            })
            .collect();
        if !space.is_empty() && named.iter().all(|named| *named) {
            return self.missing_by_constructor(rows, &space, column_ty, rest_tys);
        }
        // The rows leave constructors unnamed, so coverage rests on the rows
        // that admit any value; the unnamed constructors are the witnesses.
        let heads: Vec<String> = if space.is_empty() {
            vec!["_".to_string()]
        } else {
            space
                .iter()
                .zip(&named)
                .filter(|(_, named)| !**named)
                .map(|(info, _)| info.open())
                .collect()
        };
        let default_rows: Vec<Row<'_>> = rows
            .iter()
            .filter(|row| self.head_admits_anything(row[0], column_ty))
            .map(|row| row[1..].to_vec())
            .collect();
        // With no row admitting this column, every later column is open too,
        // so one witness per unnamed constructor says all there is to say.
        let tails = if default_rows.is_empty() {
            vec![vec!["_".to_string(); rest_tys.len()]]
        } else {
            self.missing_rows(&default_rows, rest_tys)
        };
        tails
            .into_iter()
            .flat_map(|tail| {
                heads.iter().map(move |head| {
                    let mut witness = vec![head.clone()];
                    witness.extend(tail.iter().cloned());
                    witness
                })
            })
            .collect()
    }

    /// Split column zero over a fully named constructor space.
    fn missing_by_constructor(
        &self,
        rows: &[Row<'_>],
        space: &[CtorInfo],
        column_ty: &Ty,
        rest_tys: &[Ty],
    ) -> Vec<Vec<String>> {
        let mut missing = Vec::new();
        for info in space {
            let mut next_rows = Vec::new();
            for row in rows {
                for mut prefix in self.specialize(row[0], column_ty, info) {
                    prefix.extend_from_slice(&row[1..]);
                    next_rows.push(prefix);
                }
            }
            let mut next_tys = info.slots.clone();
            next_tys.extend_from_slice(rest_tys);
            for witness in self.missing_rows(&next_rows, &next_tys) {
                let (slots, tail) = witness.split_at(info.slots.len());
                let mut row = vec![info.render(slots)];
                row.extend_from_slice(tail);
                missing.push(row);
            }
        }
        missing
    }

    /// The constructor space of `ty`, or `None` when the space is unbounded
    /// (integers, floats, strings, chars, opaque and handle types) and only a
    /// wildcard or binding covers it. `Some(empty)` means uninhabited.
    fn ctor_space(&self, ty: &Ty) -> Option<Vec<CtorInfo>> {
        let ty = self.project_assoc_types(&self.subst.resolve(ty));
        match &ty {
            Ty::Never => return Some(Vec::new()),
            // Unit has exactly one value, spelled `()`.
            Ty::Unit => {
                return Some(vec![CtorInfo {
                    ctor: Ctor::Product,
                    slots: Vec::new(),
                    fields: None,
                    head: None,
                }])
            }
            Ty::Bool => {
                return Some(
                    [true, false]
                        .into_iter()
                        .map(|value| CtorInfo {
                            ctor: Ctor::Bool(value),
                            slots: Vec::new(),
                            fields: None,
                            head: None,
                        })
                        .collect(),
                )
            }
            Ty::Tuple(items) => {
                return Some(vec![CtorInfo {
                    ctor: Ctor::Product,
                    slots: items.clone(),
                    fields: None,
                    head: None,
                }])
            }
            _ => {}
        }
        if let Some(variants) = self.enum_variant_payloads(&ty) {
            // `Option` and `Result` are listed in their declaration order;
            // every other enum comes out of a hash map, so name order keeps
            // the diagnostic stable.
            let mut variants = variants;
            if ty.as_option().is_none() && ty.as_result().is_none() {
                variants.sort_by(|(left, _), (right, _)| left.cmp(right));
            }
            return Some(
                variants
                    .into_iter()
                    .filter(|(_, shape)| !self.variant_is_unconstructable(shape))
                    .map(|(name, shape)| CtorInfo::variant(name, &shape))
                    .collect(),
            );
        }
        let td = self.lookup_type_def(ty.type_name()?)?;
        match td.kind {
            // An enum with no variants is uninhabited.
            TypeDefKind::Enum => Some(Vec::new()),
            TypeDefKind::Struct | TypeDefKind::Record if !td.field_order.is_empty() => {
                let args = match &ty {
                    Ty::Named { args, .. } => args.clone(),
                    _ => Vec::new(),
                };
                let slots = td
                    .field_order
                    .iter()
                    .map(|field| {
                        td.fields
                            .get(field)
                            .map(|ty| substitute_pattern_field_ty(ty, &td.type_params, &args))
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(vec![CtorInfo {
                    ctor: Ctor::Product,
                    slots,
                    fields: Some(td.field_order.clone()),
                    head: Some(td.name.clone()),
                }])
            }
            _ => None,
        }
    }

    /// True when `head` matches every value of its column: a wildcard, a
    /// binding, or an or-pattern with such a branch.
    fn head_admits_anything(&self, head: &Pattern, ty: &Ty) -> bool {
        match head {
            Pattern::Or(left, right) => {
                self.head_admits_anything(&left.0, ty) || self.head_admits_anything(&right.0, ty)
            }
            _ => self.is_catch_all_for_scrutinee(head, ty),
        }
    }

    /// The sub-rows `head` opens for `info`, one per or-pattern branch that
    /// matches. An empty result means the head cannot match this constructor.
    fn specialize<'pattern>(
        &self,
        head: &'pattern Pattern,
        ty: &Ty,
        info: &CtorInfo,
    ) -> Vec<Row<'pattern>> {
        let open = || vec![vec![&WILDCARD; info.slots.len()]];
        let variant_name = match &info.ctor {
            Ctor::Variant(name) => Some(name.as_str()),
            _ => None,
        };
        match head {
            Pattern::Wildcard => open(),
            Pattern::Or(left, right) => {
                let mut rows = self.specialize(&left.0, ty, info);
                rows.extend(self.specialize(&right.0, ty, info));
                rows
            }
            Pattern::Identifier(name) => {
                if self.is_catch_all_for_scrutinee(head, ty) {
                    return open();
                }
                let matched = variant_name == Some(short_name(name))
                    && info.slots.is_empty()
                    && self.variant_surface_owner_matches(name, ty);
                if matched {
                    vec![Vec::new()]
                } else {
                    Vec::new()
                }
            }
            Pattern::Literal(Literal::Bool(value)) if info.ctor == Ctor::Bool(*value) => {
                vec![Vec::new()]
            }
            Pattern::Tuple(patterns) if info.ctor == Ctor::Product => {
                Self::positional_row(patterns, info)
            }
            Pattern::Constructor { name, patterns } => {
                if variant_name == Some(short_name(name))
                    && self.variant_surface_owner_matches(name, ty)
                {
                    Self::positional_row(patterns, info)
                } else {
                    Vec::new()
                }
            }
            Pattern::Struct { name, fields, .. } => match &info.ctor {
                Ctor::Product => Self::named_row(fields, info),
                Ctor::Variant(_)
                    if variant_name == Some(short_name(name))
                        && self.variant_surface_owner_matches(name, ty) =>
                {
                    Self::named_row(fields, info)
                }
                _ => Vec::new(),
            },
            Pattern::RecordShorthand { fields, .. } if info.ctor == Ctor::Product => {
                Self::named_row(fields, info)
            }
            Pattern::ContextVariant(context) if variant_name == Some(context.name.as_str()) => {
                Self::payload_row(context.payload.as_ref(), info)
            }
            Pattern::NominalPath { path, payload }
                if variant_name == nominal_path_leaf(path)
                    && self.variant_path_owner_matches(path, ty) =>
            {
                Self::payload_row(payload.as_ref(), info)
            }
            _ => Vec::new(),
        }
    }

    fn payload_row<'pattern>(
        payload: Option<&'pattern NominalPatternPayload>,
        info: &CtorInfo,
    ) -> Vec<Row<'pattern>> {
        match payload {
            None if info.slots.is_empty() => vec![Vec::new()],
            None => Vec::new(),
            Some(NominalPatternPayload::Tuple(patterns)) => Self::positional_row(patterns, info),
            Some(NominalPatternPayload::Record { fields, .. }) => Self::named_row(fields, info),
        }
    }

    fn positional_row<'pattern>(
        patterns: &'pattern [Spanned<Pattern>],
        info: &CtorInfo,
    ) -> Vec<Row<'pattern>> {
        if patterns.len() == info.slots.len() {
            vec![patterns.iter().map(|(pattern, _)| pattern).collect()]
        } else {
            Vec::new()
        }
    }

    /// Map a record-shaped pattern onto its constructor's declared field
    /// order. An omitted field, a `..` rest and a shorthand binder all leave
    /// their slot open.
    fn named_row<'pattern>(
        fields: &'pattern [PatternField],
        info: &CtorInfo,
    ) -> Vec<Row<'pattern>> {
        let Some(declared) = info.fields.as_ref() else {
            return Vec::new();
        };
        vec![declared
            .iter()
            .map(|name| {
                fields
                    .iter()
                    .find(|field| field.name == *name)
                    .and_then(|field| field.pattern.as_ref())
                    .map_or(&WILDCARD, |(pattern, _)| pattern)
            })
            .collect()]
    }
}
