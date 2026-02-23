use std::collections::HashMap;

/// Configuration for mapping Rust enum variant names to C++ struct names.
pub struct TypeMap {
    /// enum_name → variant naming strategy
    variant_prefix: HashMap<String, VariantNaming>,
    /// (enum_name, variant_name) → C++ struct name overrides
    variant_overrides: HashMap<(String, String), String>,
    /// Types that need forward declarations (recursive types).
    pub forward_declared: Vec<String>,
    /// Types that are special-cased and should not be auto-generated.
    pub skip_types: Vec<String>,
}

enum VariantNaming {
    /// Prefix + variant name: e.g., "Expr" + "Binary" → "ExprBinary"
    Prefix(String),
    /// Use the inner newtype's type name: e.g., Item::Import(ImportDecl) → "ImportDecl"
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

/// Map a RustType to a C++ type string.
pub fn cpp_type(ty: &crate::model::RustType) -> String {
    use crate::model::RustType;
    match ty {
        RustType::String => "std::string".to_string(),
        RustType::Bool => "bool".to_string(),
        RustType::I64 => "int64_t".to_string(),
        RustType::U64 => "uint64_t".to_string(),
        RustType::U32 => "uint32_t".to_string(),
        RustType::F64 => "double".to_string(),
        RustType::Char => "char".to_string(),
        RustType::Usize => "uint64_t".to_string(),
        RustType::PathBuf => "std::string".to_string(),
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
