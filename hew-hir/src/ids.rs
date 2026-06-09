use std::fmt;

macro_rules! id_newtype {
    ($name:ident, $prefix:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub u32);

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!($prefix, "{}"), self.0)
            }
        }
    };
}

id_newtype!(BindingId, "b");
id_newtype!(SiteId, "s");
id_newtype!(ScopeId, "sc");
id_newtype!(HirNodeId, "h");
id_newtype!(ItemId, "i");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolvedRef {
    Binding(BindingId),
    Item(ItemId),
    /// Reference to a module-level `const`. The `ItemId` matches the
    /// `HirConst::id` of the declaration; MIR/codegen resolve it back to the
    /// folded constant descriptor.
    Const(ItemId),
    Unresolved,
}

#[derive(Debug, Default)]
pub struct IdGen {
    binding: u32,
    site: u32,
    scope: u32,
    node: u32,
    item: u32,
}

impl IdGen {
    #[must_use]
    pub fn binding(&mut self) -> BindingId {
        let id = BindingId(self.binding);
        self.binding += 1;
        id
    }

    #[must_use]
    pub fn site(&mut self) -> SiteId {
        let id = SiteId(self.site);
        self.site += 1;
        id
    }

    #[must_use]
    pub fn scope(&mut self) -> ScopeId {
        let id = ScopeId(self.scope);
        self.scope += 1;
        id
    }

    #[must_use]
    pub fn node(&mut self) -> HirNodeId {
        let id = HirNodeId(self.node);
        self.node += 1;
        id
    }

    #[must_use]
    pub fn item(&mut self) -> ItemId {
        let id = ItemId(self.item);
        self.item += 1;
        id
    }
}
