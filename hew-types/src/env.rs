//! Type environment with lexical scoping.
//!
//! The type environment tracks variable bindings across nested scopes,
//! supporting let/var declarations and shadowing.

use crate::ty::Ty;
use hew_parser::ast::Span;
use std::collections::HashMap;

/// A binding in the type environment.
#[derive(Debug, Clone)]
pub struct Binding {
    /// The type of the bound value
    pub ty: Ty,
    /// Whether the binding is mutable (var vs let)
    pub is_mutable: bool,
    /// Whether the value has been moved (e.g., sent to an actor)
    pub is_moved: bool,
    /// Where the move happened, for error reporting
    pub moved_at: Option<Span>,
    /// Whether the variable has been read since definition
    pub is_used: bool,
    /// Count of read accesses (incremented by lookup, decremented by `unmark_used`).
    pub read_count: u32,
    /// Whether the variable has been reassigned after initial definition
    pub is_written: bool,
    /// Source span of the definition, for diagnostics. None for synthetic bindings.
    pub def_span: Option<Span>,
}

/// A diagnostic about a binding discovered at scope exit.
#[derive(Debug)]
pub struct ScopeWarning {
    /// The variable name
    pub name: String,
    /// Source span of the definition
    pub span: Span,
    /// What kind of warning
    pub kind: ScopeWarningKind,
}

/// The kind of scope-level warning.
#[derive(Debug)]
pub enum ScopeWarningKind {
    /// Variable defined but never read
    Unused,
    /// Declared `var` but never reassigned — could be `let`
    NeverMutated,
}

/// Lexically-scoped type environment.
///
/// Maintains a stack of scopes, where each scope maps names to bindings.
/// Lookup walks from innermost to outermost scope.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    scopes: Vec<HashMap<String, Binding>>,
}

impl TypeEnv {
    /// Create a new empty environment with one scope.
    #[must_use]
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    /// Push a new scope onto the stack.
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Pop the current scope from the stack.
    ///
    /// # Panics
    /// Panics if there are no scopes to pop.
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("cannot pop empty scope stack");
    }

    /// Define a variable in the current scope (synthetic, no source span — not warned about).
    pub fn define(&mut self, name: String, ty: Ty, is_mutable: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                Binding {
                    ty,
                    is_mutable,
                    is_moved: false,
                    moved_at: None,
                    is_used: true, // synthetic bindings are always "used"
                    read_count: 1,
                    is_written: false,
                    def_span: None,
                },
            );
        }
    }

    /// Define a user-visible variable with a source span for diagnostics.
    pub fn define_with_span(&mut self, name: String, ty: Ty, is_mutable: bool, span: Span) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                Binding {
                    ty,
                    is_mutable,
                    is_moved: false,
                    moved_at: None,
                    is_used: false,
                    read_count: 0,
                    is_written: false,
                    def_span: Some(span),
                },
            );
        }
    }

    /// Mark a variable as moved, returning `true` if found.
    pub fn mark_moved(&mut self, name: &str, span: Span) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.is_moved = true;
                binding.moved_at = Some(span);
                return true;
            }
        }
        false
    }

    /// Mark a variable as written (reassigned after definition).
    pub fn mark_written(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.is_written = true;
                return;
            }
        }
    }

    /// Pop the current scope, returning diagnostics about unused/unmutated bindings.
    pub fn pop_scope_with_warnings(&mut self) -> Vec<ScopeWarning> {
        let scope = self.scopes.pop().expect("cannot pop empty scope stack");
        let mut warnings = Vec::new();
        for (name, binding) in &scope {
            let Some(span) = &binding.def_span else {
                continue; // synthetic binding (self, params without spans, etc.)
            };
            if name.starts_with('_') {
                continue; // convention: _ prefix means intentionally unused
            }
            if !binding.is_used && binding.read_count == 0 {
                warnings.push(ScopeWarning {
                    name: name.clone(),
                    span: span.clone(),
                    kind: ScopeWarningKind::Unused,
                });
            } else if binding.is_mutable && !binding.is_written {
                warnings.push(ScopeWarning {
                    name: name.clone(),
                    span: span.clone(),
                    kind: ScopeWarningKind::NeverMutated,
                });
            }
        }
        warnings
    }

    /// Look up a variable by name, marking it as used.
    #[must_use]
    pub fn lookup(&mut self, name: &str) -> Option<&Binding> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.is_used = true;
                binding.read_count += 1;
                return Some(binding);
            }
        }
        None
    }

    /// Look up a variable by name without marking it as used.
    #[must_use]
    pub fn lookup_ref(&self, name: &str) -> Option<&Binding> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Some(binding);
            }
        }
        None
    }

    /// Look up a variable by name, returning the scope depth where it was found. Marks as used.
    #[must_use]
    pub fn lookup_with_depth(&mut self, name: &str) -> Option<(usize, &Binding)> {
        for (i, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(binding) = scope.get_mut(name) {
                binding.is_used = true;
                binding.read_count += 1;
                return Some((i, binding));
            }
        }
        None
    }

    /// Check if a variable is defined in the current (innermost) scope only.
    #[must_use]
    pub fn is_defined_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .is_some_and(|scope| scope.contains_key(name))
    }

    /// Get the depth of the scope stack.
    #[must_use]
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// Check if a variable name exists in any outer scope (not the current one).
    /// Returns the definition span of the shadowed binding if found.
    #[must_use]
    pub fn find_in_outer_scope(&self, name: &str) -> Option<Span> {
        // Skip the last (current) scope and check all outer scopes
        for scope in self.scopes.iter().rev().skip(1) {
            if let Some(binding) = scope.get(name) {
                return binding.def_span.clone();
            }
        }
        None
    }

    /// Return all variable names visible in the current scope stack.
    pub fn all_names(&self) -> impl Iterator<Item = &str> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.keys().map(String::as_str))
    }

    /// Undo the `is_used` mark on a variable (used for plain assignment LHS).
    /// Decrements the read count so that write-only variables are still detected.
    pub fn unmark_used(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.get_mut(name) {
                // Decrement the read count to undo the lookup that resolved the
                // assignment target. If the variable was genuinely read before
                // (read_count > 1), the count stays positive and the variable
                // remains "used". For write-only variables, count drops to 0.
                binding.read_count = binding.read_count.saturating_sub(1);
                if binding.read_count == 0 {
                    binding.is_used = false;
                }
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define_and_lookup() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        let binding = env.lookup("x").unwrap();
        assert_eq!(binding.ty, Ty::I32);
        assert!(!binding.is_mutable);
    }

    #[test]
    fn test_shadowing() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        env.push_scope();
        env.define("x".to_string(), Ty::Bool, true);

        let binding = env.lookup("x").unwrap();
        assert_eq!(binding.ty, Ty::Bool);
        assert!(binding.is_mutable);

        env.pop_scope();
        let binding = env.lookup("x").unwrap();
        assert_eq!(binding.ty, Ty::I32);
    }

    #[test]
    fn test_lookup_outer_scope() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        env.push_scope();
        env.define("y".to_string(), Ty::Bool, false);

        // Can still find x from outer scope
        assert!(env.lookup("x").is_some());
        assert!(env.lookup("y").is_some());
    }

    #[test]
    fn test_undefined() {
        let mut env = TypeEnv::new();
        assert!(env.lookup("x").is_none());
    }

    #[test]
    fn test_is_defined_in_current_scope() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        env.push_scope();

        assert!(!env.is_defined_in_current_scope("x"));
        env.define("x".to_string(), Ty::Bool, false);
        assert!(env.is_defined_in_current_scope("x"));
    }

    #[test]
    fn test_mark_moved() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::String, false);
        assert!(!env.lookup("x").unwrap().is_moved);

        assert!(env.mark_moved("x", 10..20));
        let binding = env.lookup("x").unwrap();
        assert!(binding.is_moved);
        assert_eq!(binding.moved_at, Some(10..20));
    }

    #[test]
    fn test_mark_moved_not_found() {
        let mut env = TypeEnv::new();
        assert!(!env.mark_moved("x", 0..1));
    }

    #[test]
    fn test_new_binding_not_moved() {
        let mut env = TypeEnv::new();
        env.define("x".to_string(), Ty::I32, false);
        let binding = env.lookup("x").unwrap();
        assert!(!binding.is_moved);
        assert_eq!(binding.moved_at, None);
    }

    #[test]
    fn test_define_with_span_tracks_usage() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, false, 0..5);
        // Not yet used
        let b = env.lookup_ref("x").unwrap();
        assert!(!b.is_used);
        assert!(!b.is_written);
        assert_eq!(b.def_span, Some(0..5));

        // lookup() marks as used
        let b = env.lookup("x").unwrap();
        assert!(b.is_used);
    }

    #[test]
    fn test_synthetic_define_always_used() {
        let mut env = TypeEnv::new();
        env.define("self_".to_string(), Ty::I32, false);
        let b = env.lookup_ref("self_").unwrap();
        assert!(b.is_used, "synthetic bindings should start as used");
        assert!(b.def_span.is_none());
    }

    #[test]
    fn test_mark_written() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        assert!(!env.lookup_ref("x").unwrap().is_written);
        env.mark_written("x");
        assert!(env.lookup_ref("x").unwrap().is_written);
    }

    #[test]
    fn test_unmark_used_on_first_assignment() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        // Simulate what synthesize does during assignment target resolution
        let _ = env.lookup("x"); // marks is_used = true
        assert!(env.lookup_ref("x").unwrap().is_used);
        // unmark_used should reverse it for first assignment
        env.unmark_used("x");
        assert!(!env.lookup_ref("x").unwrap().is_used);
    }

    #[test]
    fn test_unmark_used_preserves_after_genuine_read() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        // First: variable is genuinely used somewhere
        let _ = env.lookup("x"); // read_count = 1
                                 // Then: an assignment target lookup + unmark (simulating `x = 1`)
        let _ = env.lookup("x"); // read_count = 2
        env.unmark_used("x"); // read_count = 1
        assert!(
            env.lookup_ref("x").unwrap().is_used,
            "unmark_used should not undo usage when there was a genuine read"
        );
    }

    #[test]
    fn test_lookup_ref_does_not_mark_used() {
        let mut env = TypeEnv::new();
        env.define_with_span("x".to_string(), Ty::I32, false, 0..5);
        let _ = env.lookup_ref("x");
        assert!(
            !env.lookup_ref("x").unwrap().is_used,
            "lookup_ref should not mark as used"
        );
    }

    #[test]
    fn test_pop_scope_warns_unused() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.define_with_span("unused".to_string(), Ty::I32, false, 0..6);
        let warnings = env.pop_scope_with_warnings();
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].name, "unused");
        assert!(matches!(warnings[0].kind, ScopeWarningKind::Unused));
    }

    #[test]
    fn test_pop_scope_warns_never_mutated() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        let _ = env.lookup("x"); // mark used but never written
        let warnings = env.pop_scope_with_warnings();
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].name, "x");
        assert!(matches!(warnings[0].kind, ScopeWarningKind::NeverMutated));
    }

    #[test]
    fn test_pop_scope_no_warn_if_used_and_written() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        let _ = env.lookup("x"); // used
        env.mark_written("x"); // written
        let warnings = env.pop_scope_with_warnings();
        assert!(warnings.is_empty(), "no warning for used+written var");
    }

    #[test]
    fn test_pop_scope_no_warn_underscore_prefix() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.define_with_span("_ignored".to_string(), Ty::I32, false, 0..8);
        let warnings = env.pop_scope_with_warnings();
        assert!(
            warnings.is_empty(),
            "_ prefix should suppress unused warning"
        );
    }

    #[test]
    fn test_pop_scope_no_warn_synthetic() {
        let mut env = TypeEnv::new();
        env.push_scope();
        // synthetic define — no span, is_used = true
        env.define("self_".to_string(), Ty::I32, false);
        let warnings = env.pop_scope_with_warnings();
        assert!(
            warnings.is_empty(),
            "synthetic bindings should never produce warnings"
        );
    }

    #[test]
    fn test_pop_scope_unused_beats_never_mutated() {
        let mut env = TypeEnv::new();
        env.push_scope();
        // mutable AND unused — should get Unused, not NeverMutated
        env.define_with_span("x".to_string(), Ty::I32, true, 0..5);
        let warnings = env.pop_scope_with_warnings();
        assert_eq!(warnings.len(), 1);
        assert!(
            matches!(warnings[0].kind, ScopeWarningKind::Unused),
            "unused should take priority over never-mutated"
        );
    }

    #[test]
    fn test_all_names() {
        let mut env = TypeEnv::new();
        env.define("a".to_string(), Ty::I32, false);
        env.push_scope();
        env.define("b".to_string(), Ty::Bool, false);
        let names: Vec<_> = env.all_names().collect();
        assert!(names.contains(&"a"));
        assert!(names.contains(&"b"));
    }
}
