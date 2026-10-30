//! The one spelling of a Hew function in debuggers, symbol tables and reports.
//!
//! DWARF and `CodeView` subprogram names, `__hew_symtab`, observe handler names
//! and the debugger scripts all read [`display_names`]; nothing else spells a
//! function for a person. The mangled symbol stays the linkage name.
//!
//! The spelling is the dotted declaration path the source would use to name
//! the function, with concrete type arguments for a generic instance:
//!
//! | body | display name |
//! |---|---|
//! | free function | `fact`, `util.triple` |
//! | method, actor handler | `Dog.name`, `Counter.add` |
//! | generic instance | `unbox<i64>`, `std.builtins.Vec.into_iter<i64>` |
//! | closure | `make_counter.closure#1` (ordinal within its enclosing body) |
//! | process entry | `main` |
//!
//! Compiler-synthesized bodies with no source of their own are artificial:
//! debuggers skip them when stepping and the symtab omits them.

use std::collections::BTreeMap;

use hew_sir::{CallableId, CallableInstance, SemModule};

/// The display name of the Hew entry function: `break main` stops in it.
pub const ENTRY_DISPLAY_NAME: &str = "main";

/// The debug name of the C `main` the linker enters through. It is an
/// artificial subprogram so a debugger resolving `main` by name finds only
/// [`ENTRY_DISPLAY_NAME`]; the C entry keeps `main` as its linkage name.
pub const C_ENTRY_DEBUG_NAME: &str = "__hew_shim_entry";

/// Reserved prefix of every compiler-generated shim symbol.
pub const SHIM_PREFIX: &str = "__hew_shim_";

/// How one callable appears to a person.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DisplayName {
    pub text: String,
    /// The body has no source a person wrote: debuggers step over it and the
    /// symtab omits it.
    pub artificial: bool,
}

/// The display name of every callable in `module`, by callable id.
#[must_use]
pub fn display_names(module: &SemModule) -> BTreeMap<CallableId, DisplayName> {
    // Closure ordinals count each enclosing body's closures in literal order.
    let mut closure_ordinals = BTreeMap::new();
    let mut per_enclosing: BTreeMap<CallableId, Vec<_>> = BTreeMap::new();
    for closure in &module.closures {
        per_enclosing
            .entry(closure.instance.enclosing)
            .or_default()
            .push((closure.instance.literal, closure.id));
    }
    for literals in per_enclosing.values_mut() {
        literals.sort_unstable();
        for (index, (_, closure)) in literals.iter().enumerate() {
            closure_ordinals.insert(*closure, index + 1);
        }
    }
    let enclosing: BTreeMap<_, _> = module
        .closures
        .iter()
        .map(|closure| (closure.id, closure.instance.enclosing))
        .collect();

    let mut names = BTreeMap::new();
    for callable in &module.callables {
        let name = name_of(module, callable.id, &enclosing, &closure_ordinals, 0);
        names.insert(callable.id, name);
    }
    names
}

/// Bound on closure nesting followed when naming; a deeper chain is a
/// malformed module, and the name stops growing rather than recursing.
const MAX_CLOSURE_DEPTH: usize = 64;

fn name_of(
    module: &SemModule,
    id: CallableId,
    enclosing: &BTreeMap<hew_sir::ClosureId, CallableId>,
    ordinals: &BTreeMap<hew_sir::ClosureId, usize>,
    depth: usize,
) -> DisplayName {
    let Some(callable) = module.callables.get(id.0 as usize) else {
        return DisplayName {
            text: String::new(),
            artificial: true,
        };
    };
    let defs = &module.defs;
    let declared = || defs.dotted_declaration_path(callable.declaration);
    let source = |text: String| DisplayName {
        text,
        artificial: false,
    };
    let artificial = |text: String| DisplayName {
        text,
        artificial: true,
    };
    let entry_declaration = module
        .entry_callable
        .and_then(|entry| module.callables.get(entry.0 as usize))
        .map(|entry| entry.declaration);
    match &callable.instance {
        // The user's entry body, whether the process enters it directly or
        // through a synthesized exit adapter.
        CallableInstance::Monomorphic if entry_declaration == Some(callable.declaration) => {
            source(ENTRY_DISPLAY_NAME.to_owned())
        }
        CallableInstance::Monomorphic | CallableInstance::ActorMember => source(declared()),
        CallableInstance::Generic(key) => {
            let mut text = declared();
            text.push('<');
            for (index, ty) in key.type_args.iter().enumerate() {
                if index > 0 {
                    text.push_str(", ");
                }
                text.push_str(&ty.user_facing().to_string());
            }
            text.push('>');
            source(text)
        }
        CallableInstance::Closure(closure) => {
            let owner = match enclosing.get(closure) {
                Some(owner) if depth < MAX_CLOSURE_DEPTH => {
                    name_of(module, *owner, enclosing, ordinals, depth + 1).text
                }
                _ => declared(),
            };
            let ordinal = ordinals.get(closure).copied().unwrap_or(0);
            source(format!("{owner}.closure#{ordinal}"))
        }
        CallableInstance::ActorConstruction { .. } => source(format!("{}.spawn", declared())),
        CallableInstance::SupervisorChild { child, .. } => {
            source(format!("{}.child#{}", declared(), child + 1))
        }
        CallableInstance::SupervisorPoolCount { child, .. } => {
            source(format!("{}.pool_count#{}", declared(), child + 1))
        }
        CallableInstance::SupervisorBackoff { .. } => source(format!("{}.backoff", declared())),
        CallableInstance::EntryAdapter => artificial(format!("{SHIM_PREFIX}entry_adapter")),
        CallableInstance::ActorInterceptEntry { handler } => {
            artificial(format!("{SHIM_PREFIX}intercept_entry_{handler}"))
        }
        CallableInstance::ActorSupervisorIntercept { layer } => {
            artificial(format!("{SHIM_PREFIX}supervisor_intercept_{layer}"))
        }
    }
}

#[cfg(test)]
mod tests {
    use hew_hir::{lower_program_host_target, ResolutionCtx};
    use hew_types::{module_registry::ModuleRegistry, Checker};

    use super::*;

    fn names(source: &str) -> Vec<(String, bool)> {
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("the crate lives under the repository root")
            .to_path_buf();
        let mut checker = Checker::new(ModuleRegistry::new(vec![root]));
        let facts = checker.check_program(&parsed.program);
        assert!(facts.errors.is_empty(), "{:#?}", facts.errors);
        let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
        assert!(hir.diagnostics.is_empty(), "{:#?}", hir.diagnostics);
        let module = hew_sir::lower_module(&hir.module, &facts).module;
        let mut names: Vec<_> = display_names(&module)
            .into_values()
            .map(|name| (name.text, name.artificial))
            .collect();
        names.sort();
        names.dedup();
        names
    }

    fn source_names(source: &str) -> Vec<String> {
        names(source)
            .into_iter()
            .filter(|(_, artificial)| !artificial)
            .map(|(text, _)| text)
            .collect()
    }

    #[test]
    fn functions_methods_and_generic_instances_use_dotted_paths() {
        let names = source_names(
            "trait Named { fn name(self) -> string; }
type Dog { tag: string; }
impl Named for Dog { fn name(self) -> string { self.tag } }
type Box<T> { value: T; }
fn unbox<T>(b: Box<T>) -> T { b.value }
fn fact(n: i64) -> i64 { if n <= 1 { 1 } else { n * fact(n - 1) } }
fn main() {
    println(unbox(Box { value: fact(3) }));
    println(unbox(Box { value: \"s\" }));
    let d: dyn Named = Dog { tag: \"rex\" };
    println(d.name());
}",
        );
        for expected in ["main", "fact", "Dog.name", "unbox<i64>", "unbox<string>"] {
            assert!(
                names.iter().any(|name| name == expected),
                "missing {expected} in {names:?}"
            );
        }
    }

    #[test]
    fn actor_handlers_name_their_actor() {
        let names = source_names(
            "actor Counter {
    var count: i64;
    receive fn add(n: i64) { count = count + n; }
    receive fn total() -> i64 { count }
}
fn main() {
    let c = spawn Counter { count: 0 };
    let _ = c.add(1);
    let _ = c.total();
}",
        );
        for expected in ["main", "Counter.add", "Counter.total"] {
            assert!(
                names.iter().any(|name| name == expected),
                "missing {expected} in {names:?}"
            );
        }
    }

    #[test]
    fn closures_count_from_one_within_their_enclosing_body() {
        let names = source_names(
            "fn make_counter() -> fn[var]() -> i64 {
    let count: i64 = 0;
    || { count = count + 1; count }
}
fn apply(f: fn(i64) -> i64, v: i64) -> i64 { f(v) }
fn main() {
    let offset = 100;
    let add = |x: i64| x + offset;
    let double = |x: i64| x * 2;
    println(apply(add, apply(double, 5)));
    var c = make_counter();
    println(c());
}",
        );
        for expected in ["make_counter.closure#1", "main.closure#1", "main.closure#2"] {
            assert!(
                names.iter().any(|name| name == expected),
                "missing {expected} in {names:?}"
            );
        }
    }

    #[test]
    fn a_fails_main_names_its_body_main_and_its_adapter_artificial() {
        let names = names(
            "fn main() fails string {
    let x: Result<i64, string> = .Ok(1);
    println(x?);
}",
        );
        assert!(names.contains(&("main".to_owned(), false)), "{names:?}");
        assert!(
            names
                .iter()
                .filter(|(text, artificial)| text == "main" && !artificial)
                .count()
                == 1
        );
    }
}
