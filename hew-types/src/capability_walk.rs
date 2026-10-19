//! Shared recursion bookkeeping for concrete value capabilities.

use crate::type_facts::{push_type_components, ValueCapability};
use crate::ResolvedTy;
use std::collections::HashMap;

type Key = (ResolvedTy, ValueCapability);

pub(crate) struct CapabilityWalk<R> {
    states: HashMap<Key, State<R>>,
    active: Vec<Key>,
    completed: Vec<Key>,
}

enum State<R> {
    Visiting,
    Complete(R),
}

pub(crate) enum CapabilityVisit<R> {
    Complete(R),
    Recursive,
    Growing,
    Entered(CapabilityEntry),
}

pub(crate) struct CapabilityEntry {
    key: Key,
    completed: usize,
}

impl<R> Default for CapabilityWalk<R> {
    fn default() -> Self {
        Self {
            states: HashMap::new(),
            active: Vec::new(),
            completed: Vec::new(),
        }
    }
}

impl<R: Clone> CapabilityWalk<R> {
    pub(crate) fn enter(
        &mut self,
        ty: &ResolvedTy,
        capability: ValueCapability,
    ) -> CapabilityVisit<R> {
        let key = (ty.clone(), capability);
        match self.states.get(&key) {
            Some(State::Complete(result)) => return CapabilityVisit::Complete(result.clone()),
            Some(State::Visiting) => return CapabilityVisit::Recursive,
            None => {}
        }
        if self.grows(ty, capability) {
            return CapabilityVisit::Growing;
        }
        self.states.insert(key.clone(), State::Visiting);
        self.active.push(key.clone());
        CapabilityVisit::Entered(CapabilityEntry {
            key,
            completed: self.completed.len(),
        })
    }

    /// A failed result invalidates every nested result that could have relied
    /// on its provisional cycle. Completed independent earlier queries survive.
    pub(crate) fn finish(&mut self, entry: CapabilityEntry, result: R, admitted: bool) {
        let active = self.active.pop();
        debug_assert_eq!(active.as_ref(), Some(&entry.key));
        if !admitted {
            for dependent in self.completed.drain(entry.completed..) {
                self.states.remove(&dependent);
            }
        }
        self.states
            .insert(entry.key.clone(), State::Complete(result));
        self.completed.push(entry.key);
    }

    fn grows(&self, ty: &ResolvedTy, capability: ValueCapability) -> bool {
        let ResolvedTy::Named { head, args, .. } = ty else {
            return false;
        };
        !args.is_empty()
            && self.active.iter().any(|(previous, operation)| {
                *operation == capability
                    && matches!(previous,
                ResolvedTy::Named { head: previous_head, args: previous_args, .. }
                    if previous_head == head && previous_args != args
                        && previous_args.len() == args.len()
                        && previous_args.iter().zip(args)
                            .all(|(previous, current)| contains_type(current, previous)))
            })
    }
}

fn contains_type(ty: &ResolvedTy, needle: &ResolvedTy) -> bool {
    let mut pending = vec![ty.clone()];
    while let Some(ty) = pending.pop() {
        if ty == *needle {
            return true;
        }
        push_type_components(&ty, &mut pending);
    }
    false
}
