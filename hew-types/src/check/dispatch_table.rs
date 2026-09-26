//! The method dispatch table: which declaration a method name reaches on a
//! receiver, by the receiver's declaration and the method's owner.

use std::collections::HashMap;

use hew_parser::ast::Symbol;

use crate::{DefId, NominalId, NominalInstance};

/// Whose method a declaration is: the receiver type's own (`impl T`, a type
/// body method) or a trait's (`impl Trait for T`, a materialized default).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MethodOwner {
    Inherent,
    Trait(DefId),
}

/// The outcome of selecting a dot-call method (rule R1).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodSelection {
    /// One method: the inherent one, or the only trait method of the name.
    Unique(MethodOwner, DefId),
    /// No inherent method and two or more traits declare the name; each entry
    /// is `(trait, method)`, ordered by trait.
    Ambiguous(Vec<(DefId, DefId)>),
    Missing,
}

/// Every source-declared method, keyed by the receiver's declaration.
///
/// A generic impl (`impl<T> Show for Box<T>`) and a type's own methods serve
/// every instance of the declaration; a concrete specialisation
/// (`impl Show for Box<i64>`) serves exactly its instance and wins over the
/// generic method of the same owner.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DispatchTable {
    by_head: HashMap<(NominalId, Symbol), Vec<(MethodOwner, DefId)>>,
    specialized: HashMap<(NominalInstance, Symbol), Vec<(MethodOwner, DefId)>>,
}

fn file(entries: &mut Vec<(MethodOwner, DefId)>, owner: MethodOwner, method: DefId) {
    match entries.iter_mut().find(|(existing, _)| *existing == owner) {
        Some(entry) => entry.1 = method,
        None => entries.push((owner, method)),
    }
}

impl DispatchTable {
    /// File a method serving every instance of `head`.
    pub fn insert_generic(
        &mut self,
        head: NominalId,
        owner: MethodOwner,
        name: Symbol,
        method: DefId,
    ) {
        file(self.by_head.entry((head, name)).or_default(), owner, method);
    }

    /// File a method serving exactly `instance`.
    pub fn insert_specialized(
        &mut self,
        instance: NominalInstance,
        owner: MethodOwner,
        name: Symbol,
        method: DefId,
    ) {
        file(
            self.specialized.entry((instance, name)).or_default(),
            owner,
            method,
        );
    }

    /// The methods named `name` a receiver reaches, one per owner: a
    /// specialisation for the receiver's exact instance wins over the generic
    /// method of the same owner.
    #[must_use]
    pub fn candidates(
        &self,
        head: NominalId,
        instance: Option<&NominalInstance>,
        name: Symbol,
    ) -> Vec<(MethodOwner, DefId)> {
        let mut found: Vec<(MethodOwner, DefId)> = instance
            .and_then(|instance| self.specialized.get(&(instance.clone(), name)))
            .cloned()
            .unwrap_or_default();
        for &(owner, method) in self.by_head.get(&(head, name)).into_iter().flatten() {
            if !found.iter().any(|(existing, _)| *existing == owner) {
                found.push((owner, method));
            }
        }
        found
    }

    /// Select the method a dot call `recv.name()` reaches (rule R1): the
    /// inherent method wins; otherwise exactly one trait method of the name;
    /// two or more is ambiguous.
    #[must_use]
    pub fn select(
        &self,
        head: NominalId,
        instance: Option<&NominalInstance>,
        name: Symbol,
    ) -> MethodSelection {
        let candidates = self.candidates(head, instance, name);
        if let Some(&(owner, method)) = candidates
            .iter()
            .find(|(owner, _)| *owner == MethodOwner::Inherent)
        {
            return MethodSelection::Unique(owner, method);
        }
        match candidates.as_slice() {
            [] => MethodSelection::Missing,
            [(owner, method)] => MethodSelection::Unique(*owner, *method),
            _ => {
                let mut traits: Vec<(DefId, DefId)> = candidates
                    .iter()
                    .filter_map(|(owner, method)| match owner {
                        MethodOwner::Trait(declaring) => Some((*declaring, *method)),
                        MethodOwner::Inherent => None,
                    })
                    .collect();
                traits.sort();
                MethodSelection::Ambiguous(traits)
            }
        }
    }

    /// The method `name` of `owner` a receiver reaches.
    #[must_use]
    pub fn method_of(
        &self,
        head: NominalId,
        instance: Option<&NominalInstance>,
        owner: MethodOwner,
        name: Symbol,
    ) -> Option<DefId> {
        self.candidates(head, instance, name)
            .into_iter()
            .find_map(|(candidate, method)| (candidate == owner).then_some(method))
    }

    /// Every method filed for any instance of `head`, specialisations
    /// included.
    pub fn methods_of(
        &self,
        head: NominalId,
    ) -> impl Iterator<Item = (Symbol, MethodOwner, DefId)> + '_ {
        let generic = self
            .by_head
            .iter()
            .filter(move |((candidate, _), _)| *candidate == head)
            .flat_map(|((_, name), entries)| {
                entries
                    .iter()
                    .map(move |(owner, method)| (*name, *owner, *method))
            });
        let specialized = self
            .specialized
            .iter()
            .filter(move |((instance, _), _)| instance.nominal == head)
            .flat_map(|((_, name), entries)| {
                entries
                    .iter()
                    .map(move |(owner, method)| (*name, *owner, *method))
            });
        generic.chain(specialized)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ids() -> (NominalId, DefId, DefId, DefId, DefId) {
        (
            NominalId::for_test("dispatch.T"),
            DefId::for_test("dispatch.Show"),
            DefId::for_test("dispatch.Label"),
            DefId::for_test("dispatch.T::show#inherent"),
            DefId::for_test("dispatch.T::show#show"),
        )
    }

    #[test]
    fn inherent_method_wins_over_trait_methods() {
        let (head, show, _, inherent, via_show) = ids();
        let name = Symbol::intern("show");
        let mut table = DispatchTable::default();
        table.insert_generic(head, MethodOwner::Trait(show), name, via_show);
        table.insert_generic(head, MethodOwner::Inherent, name, inherent);
        assert_eq!(
            table.select(head, None, name),
            MethodSelection::Unique(MethodOwner::Inherent, inherent)
        );
        assert_eq!(
            table.method_of(head, None, MethodOwner::Trait(show), name),
            Some(via_show)
        );
    }

    #[test]
    fn two_trait_methods_without_an_inherent_one_are_ambiguous() {
        let (head, show, label, _, via_show) = ids();
        let via_label = DefId::for_test("dispatch.T::show#label");
        let name = Symbol::intern("show");
        let mut table = DispatchTable::default();
        table.insert_generic(head, MethodOwner::Trait(show), name, via_show);
        assert_eq!(
            table.select(head, None, name),
            MethodSelection::Unique(MethodOwner::Trait(show), via_show)
        );
        table.insert_generic(head, MethodOwner::Trait(label), name, via_label);
        let MethodSelection::Ambiguous(traits) = table.select(head, None, name) else {
            panic!("two trait methods must be ambiguous");
        };
        assert_eq!(traits.len(), 2);
    }

    #[test]
    fn a_specialisation_serves_only_its_instance() {
        let (head, show, _, _, generic) = ids();
        let special = DefId::for_test("dispatch.T$$i64::show");
        let name = Symbol::intern("show");
        let i64_box = NominalInstance {
            nominal: head,
            args: vec![crate::ResolvedTy::I64],
        };
        let bool_box = NominalInstance {
            nominal: head,
            args: vec![crate::ResolvedTy::Bool],
        };
        let mut table = DispatchTable::default();
        table.insert_generic(head, MethodOwner::Trait(show), name, generic);
        table.insert_specialized(i64_box.clone(), MethodOwner::Trait(show), name, special);
        assert_eq!(
            table.select(head, Some(&i64_box), name),
            MethodSelection::Unique(MethodOwner::Trait(show), special)
        );
        assert_eq!(
            table.select(head, Some(&bool_box), name),
            MethodSelection::Unique(MethodOwner::Trait(show), generic)
        );
    }
}
