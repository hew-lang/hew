use std::collections::HashMap;

/// Configuration for mapping Rust enum variant names to C++ struct names.
pub struct TypeMap {
    /// `enum_name` → variant naming strategy
    variant_prefix: HashMap<String, VariantNaming>,
    /// (`enum_name`, `variant_name`) → C++ struct name overrides
    variant_overrides: HashMap<(String, String), String>,
    /// Types that need forward declarations (recursive types).
    pub forward_declared: Vec<String>,
    /// Types that are special-cased and should not be auto-generated.
    pub skip_types: Vec<String>,
}

enum VariantNaming {
    /// Prefix + variant name: e.g., "Expr" + "Binary" → "`ExprBinary`"
    Prefix(String),
    /// Use the inner newtype's type name: e.g., `Item::Import(ImportDecl)` → "`ImportDecl`"
    InnerTypeName,
}

impl TypeMap {
    pub fn new() -> Self {
        let mut m = Self {
            variant_prefix: HashMap::new(),
            variant_overrides: HashMap::new(),
            forward_declared: vec!["Expr", "Stmt", "Block", "TypeExpr", "Pattern", "FnDecl"]
                .into_iter()
                .map(String::from)
                .collect(),
            skip_types: vec![
                "Literal",  // custom serde, hard-coded parser
                "IntRadix", // only used by Literal internally, not in wire format
            ]
            .into_iter()
            .map(String::from)
            .collect(),
        };

        // Configure variant prefixes
        let prefixes = [
            ("Expr", "Expr"),
            ("Stmt", "Stmt"),
            ("TypeExpr", "Type"),
            ("Pattern", "Pat"),
            ("StringPart", "StringPart"),
            ("CallArg", "CallArg"),
            ("TraitItem", "TraitItem"),
            ("ImportSpec", "ImportSpec"),
            ("OverflowPolicy", "Overflow"),
        ];
        for (enum_name, prefix) in prefixes {
            m.variant_prefix.insert(
                enum_name.to_string(),
                VariantNaming::Prefix(prefix.to_string()),
            );
        }

        // Item uses inner type names
        m.variant_prefix
            .insert("Item".to_string(), VariantNaming::InnerTypeName);

        // TypeBodyItem has irregular naming
        m.variant_prefix.insert(
            "TypeBodyItem".to_string(),
            VariantNaming::Prefix("TypeBody".to_string()),
        );
        // Override for the "Field" variant
        m.variant_overrides.insert(
            ("TypeBodyItem".to_string(), "Field".to_string()),
            "TypeBodyItemField".to_string(),
        );

        m
    }

    /// Get the C++ struct name for a variant of an enum.
    ///
    /// For `VariantNaming::InnerTypeName`, extracts the type name from the variant's
    /// newtype data instead of hardcoding a mapping table.
    pub fn cpp_variant_struct(
        &self,
        enum_name: &str,
        variant: &crate::model::EnumVariant,
    ) -> String {
        let variant_name = variant.name();
        // Check overrides first
        if let Some(override_name) = self
            .variant_overrides
            .get(&(enum_name.to_string(), variant_name.to_string()))
        {
            return override_name.clone();
        }

        match self.variant_prefix.get(enum_name) {
            Some(VariantNaming::Prefix(prefix)) => format!("{prefix}{variant_name}"),
            Some(VariantNaming::InnerTypeName) => {
                // Extract the inner type name from the newtype variant data.
                // e.g., Item::Import(ImportDecl) → "ImportDecl"
                if let crate::model::EnumVariant::Newtype {
                    ty: crate::model::RustType::Named(name),
                    ..
                } = variant
                {
                    name.clone()
                } else {
                    // Fallback for non-newtype variants
                    variant_name.to_string()
                }
            }
            None => {
                // Default: just use the variant name (shouldn't happen for tagged enums)
                variant_name.to_string()
            }
        }
    }

    /// Get the C++ parser function name for a type.
    pub fn parse_fn_name(type_name: &str) -> String {
        format!("parse{type_name}")
    }

    /// Check if a type should be skipped (special-cased).
    pub fn should_skip(&self, type_name: &str) -> bool {
        self.skip_types.iter().any(|s| s == type_name)
    }

    /// Check if a type needs a forward declaration.
    pub fn needs_forward_decl(&self, type_name: &str) -> bool {
        self.forward_declared.iter().any(|s| s == type_name)
    }
}

/// Map a `RustType` to a C++ type string.
pub fn cpp_type(ty: &crate::model::RustType) -> String {
    use crate::model::RustType;
    match ty {
        RustType::Bool => "bool".to_string(),
        RustType::I64 => "int64_t".to_string(),
        RustType::U64 | RustType::Usize => "uint64_t".to_string(),
        RustType::U32 => "uint32_t".to_string(),
        RustType::F64 => "double".to_string(),
        RustType::Char => "char".to_string(),
        RustType::String | RustType::PathBuf => "std::string".to_string(),
        RustType::Vec(inner) => format!("std::vector<{}>", cpp_type(inner)),
        RustType::Option(inner) => {
            match inner.as_ref() {
                // Option<unique_ptr<T>> stays as unique_ptr (nullptr = none)
                RustType::Box(t) => format!("std::unique_ptr<{}>", cpp_type(t)),
                _ => format!("std::optional<{}>", cpp_type(inner)),
            }
        }
        RustType::Box(inner) => format!("std::unique_ptr<{}>", cpp_type(inner)),
        RustType::Spanned(inner) => format!("ast::Spanned<{}>", cpp_type(inner)),
        RustType::Named(name) => format!("ast::{name}"),
        RustType::Tuple(elems) => {
            if elems.len() == 2 {
                format!(
                    "std::pair<{}, {}>",
                    cpp_type(&elems[0]),
                    cpp_type(&elems[1])
                )
            } else {
                "/* unsupported tuple */".to_string()
            }
        }
        RustType::HashMap(k, v) => {
            format!("std::unordered_map<{}, {}>", cpp_type(k), cpp_type(v))
        }
        RustType::Range(_) => "ast::Span".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{EnumVariant, RustType};

    // ── cpp_type: primitive mappings ────────────────────────────────────────

    #[test]
    fn maps_bool_to_cpp_bool() {
        assert_eq!(cpp_type(&RustType::Bool), "bool");
    }

    #[test]
    fn maps_integer_types_to_stdint() {
        assert_eq!(cpp_type(&RustType::I64), "int64_t");
        assert_eq!(cpp_type(&RustType::U64), "uint64_t");
        assert_eq!(cpp_type(&RustType::U32), "uint32_t");
        assert_eq!(cpp_type(&RustType::Usize), "uint64_t");
    }

    #[test]
    fn maps_f64_to_double() {
        assert_eq!(cpp_type(&RustType::F64), "double");
    }

    #[test]
    fn maps_string_and_pathbuf_to_std_string() {
        assert_eq!(cpp_type(&RustType::String), "std::string");
        assert_eq!(cpp_type(&RustType::PathBuf), "std::string");
    }

    // ── cpp_type: generic wrappers ─────────────────────────────────────────

    #[test]
    fn maps_vec_to_std_vector() {
        let ty = RustType::Vec(Box::new(RustType::I64));
        assert_eq!(cpp_type(&ty), "std::vector<int64_t>");
    }

    #[test]
    fn maps_option_to_std_optional() {
        let ty = RustType::Option(Box::new(RustType::String));
        assert_eq!(cpp_type(&ty), "std::optional<std::string>");
    }

    #[test]
    fn maps_option_box_to_unique_ptr_without_optional() {
        // Option<Box<T>> collapses to unique_ptr (nullptr represents None)
        let ty = RustType::Option(Box::new(RustType::Box(Box::new(RustType::Named(
            "Expr".to_string(),
        )))));
        assert_eq!(cpp_type(&ty), "std::unique_ptr<ast::Expr>");
    }

    #[test]
    fn maps_box_to_unique_ptr() {
        let ty = RustType::Box(Box::new(RustType::Named("Block".to_string())));
        assert_eq!(cpp_type(&ty), "std::unique_ptr<ast::Block>");
    }

    #[test]
    fn maps_spanned_to_ast_spanned() {
        let ty = RustType::Spanned(Box::new(RustType::Named("TypeExpr".to_string())));
        assert_eq!(cpp_type(&ty), "ast::Spanned<ast::TypeExpr>");
    }

    #[test]
    fn maps_named_type_with_ast_prefix() {
        assert_eq!(
            cpp_type(&RustType::Named("FnDecl".to_string())),
            "ast::FnDecl"
        );
    }

    #[test]
    fn maps_pair_tuple_to_std_pair() {
        let ty = RustType::Tuple(vec![RustType::String, RustType::U64]);
        assert_eq!(cpp_type(&ty), "std::pair<std::string, uint64_t>");
    }

    #[test]
    fn maps_hashmap_to_unordered_map() {
        let ty = RustType::HashMap(
            Box::new(RustType::String),
            Box::new(RustType::Named("Module".to_string())),
        );
        assert_eq!(
            cpp_type(&ty),
            "std::unordered_map<std::string, ast::Module>"
        );
    }

    #[test]
    fn maps_range_to_ast_span() {
        let ty = RustType::Range(Box::new(RustType::Usize));
        assert_eq!(cpp_type(&ty), "ast::Span");
    }

    #[test]
    fn maps_nested_vec_option_correctly() {
        // Vec<Option<String>> → std::vector<std::optional<std::string>>
        let ty = RustType::Vec(Box::new(RustType::Option(Box::new(RustType::String))));
        assert_eq!(cpp_type(&ty), "std::vector<std::optional<std::string>>");
    }

    // ── TypeMap: variant naming ────────────────────────────────────────────

    #[test]
    fn prefix_naming_prepends_enum_name() {
        let tm = TypeMap::new();
        let variant = EnumVariant::Unit {
            name: "Binary".to_string(),
        };
        assert_eq!(tm.cpp_variant_struct("Expr", &variant), "ExprBinary");
    }

    #[test]
    fn type_expr_uses_type_prefix() {
        let tm = TypeMap::new();
        let variant = EnumVariant::Unit {
            name: "Array".to_string(),
        };
        assert_eq!(tm.cpp_variant_struct("TypeExpr", &variant), "TypeArray");
    }

    #[test]
    fn pattern_uses_pat_prefix() {
        let tm = TypeMap::new();
        let variant = EnumVariant::Unit {
            name: "Wildcard".to_string(),
        };
        assert_eq!(tm.cpp_variant_struct("Pattern", &variant), "PatWildcard");
    }

    #[test]
    fn item_uses_inner_type_name() {
        let tm = TypeMap::new();
        let variant = EnumVariant::Newtype {
            name: "Import".to_string(),
            ty: RustType::Named("ImportDecl".to_string()),
        };
        assert_eq!(tm.cpp_variant_struct("Item", &variant), "ImportDecl");
    }

    #[test]
    fn item_unit_variant_falls_back_to_variant_name() {
        let tm = TypeMap::new();
        let variant = EnumVariant::Unit {
            name: "Empty".to_string(),
        };
        // Unit variant under InnerTypeName falls back
        assert_eq!(tm.cpp_variant_struct("Item", &variant), "Empty");
    }

    #[test]
    fn type_body_item_field_override_takes_precedence() {
        let tm = TypeMap::new();
        let variant = EnumVariant::Unit {
            name: "Field".to_string(),
        };
        assert_eq!(
            tm.cpp_variant_struct("TypeBodyItem", &variant),
            "TypeBodyItemField"
        );
    }

    #[test]
    fn type_body_item_non_overridden_uses_prefix() {
        let tm = TypeMap::new();
        let variant = EnumVariant::Unit {
            name: "Method".to_string(),
        };
        assert_eq!(
            tm.cpp_variant_struct("TypeBodyItem", &variant),
            "TypeBodyMethod"
        );
    }

    #[test]
    fn unknown_enum_falls_back_to_variant_name() {
        let tm = TypeMap::new();
        let variant = EnumVariant::Unit {
            name: "Foo".to_string(),
        };
        assert_eq!(tm.cpp_variant_struct("UnknownEnum", &variant), "Foo");
    }

    // ── TypeMap: utility methods ────────────────────────────────────────────

    #[test]
    fn parse_fn_name_follows_convention() {
        assert_eq!(TypeMap::parse_fn_name("Expr"), "parseExpr");
        assert_eq!(TypeMap::parse_fn_name("FnDecl"), "parseFnDecl");
    }

    #[test]
    fn should_skip_returns_true_for_literal() {
        let tm = TypeMap::new();
        assert!(tm.should_skip("Literal"));
        assert!(tm.should_skip("IntRadix"));
        assert!(!tm.should_skip("Expr"));
    }

    #[test]
    fn needs_forward_decl_for_recursive_types() {
        let tm = TypeMap::new();
        assert!(tm.needs_forward_decl("Expr"));
        assert!(tm.needs_forward_decl("Stmt"));
        assert!(tm.needs_forward_decl("Block"));
        assert!(tm.needs_forward_decl("FnDecl"));
        assert!(!tm.needs_forward_decl("WhereClause"));
    }
}
