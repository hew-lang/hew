//! Checker probes for qualified cross-module actor identity.
//!
//! Two modules each exporting `pub actor Account` must produce DISTINCT
//! checker identities: `spawn bank.Account()` types to
//! `LocalPid<bank.Account>`, `spawn store.Account()` to
//! `LocalPid<store.Account>`, and the two `deposit` signatures are
//! independently resolvable in `fn_sigs` under their dotted keys. A bare
//! reference resolves local-first; a bare name exported by two modules with
//! no local actor is a typed `AmbiguousActorReference` error naming the
//! candidate modules — never silent first-wins.

use crate::common;

use hew_parser::ast::{ImportDecl, Item, Program, Spanned};
use hew_types::error::TypeErrorKind;
use hew_types::{Ty, TypeCheckOutput};

/// Parse `source` as a module body and wrap it as a resolved user-module
/// import reachable at `path` (module short name = last path segment).
fn module_import(path: &[&str], source: &str) -> Spanned<Item> {
    let items = common::parse_program(source).items;
    let decl = ImportDecl {
        path: path.iter().map(ToString::to_string).collect(),
        spec: None,
        module_alias: None,
        file_path: None,
        resolved_items: Some(items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    (Item::Import(decl), 0..0)
}

/// Type-check a root source together with pre-resolved module imports.
fn typecheck_with_modules(root_source: &str, modules: &[(&[&str], &str)]) -> TypeCheckOutput {
    let mut items: Vec<Spanned<Item>> = modules
        .iter()
        .map(|(path, source)| module_import(path, source))
        .collect();
    items.extend(common::parse_program(root_source).items);
    let program = Program {
        items,
        module_doc: None,
        module_graph: None,
    };
    let mut checker = common::isolated_checker();
    checker.check_program(&program)
}

const BANK_SRC: &str = "
pub actor Account {
    var balance: i64 = 0;
    receive fn deposit(n: i64) -> i64 {
        balance = balance + n;
        balance
    }
}
";

const STORE_SRC: &str = "
pub actor Account {
    var credit: i64 = 0;
    receive fn deposit(n: i64) -> bool {
        credit = credit + n;
        true
    }
}
";

/// Collect the inner actor-type names of every `LocalPid<T>` entry in
/// `expr_types`, sorted and deduplicated.
fn local_pid_inner_names(output: &TypeCheckOutput) -> Vec<String> {
    let mut names: Vec<String> = output
        .expr_types
        .values()
        .filter_map(|t| match t {
            Ty::Named { name, args, .. } if name == "LocalPid" && args.len() == 1 => {
                match &args[0] {
                    Ty::Named { name: inner, .. } => Some(inner.clone()),
                    _ => None,
                }
            }
            _ => None,
        })
        .collect();
    names.sort();
    names.dedup();
    names
}

// ── fixture 8: two-modules-same-actor checker discrimination ────────────────

#[test]
fn two_modules_same_actor_spawns_type_to_distinct_dotted_pids() {
    let output = typecheck_with_modules(
        "
fn main() {
    let a = spawn bank.Account();
    let s = spawn store.Account();
}
",
        &[(&["hew", "bank"], BANK_SRC), (&["hew", "store"], STORE_SRC)],
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:#?}",
        output.errors
    );
    let pids = local_pid_inner_names(&output);
    assert_eq!(
        pids,
        vec!["bank.Account".to_string(), "store.Account".to_string()],
        "spawn results must carry the dotted actor identity"
    );
}

#[test]
fn two_modules_same_actor_receive_sigs_independently_resolvable() {
    let output = typecheck_with_modules(
        "
fn main() {
    let a = spawn bank.Account();
    let s = spawn store.Account();
}
",
        &[(&["hew", "bank"], BANK_SRC), (&["hew", "store"], STORE_SRC)],
    );
    let bank_sig = output
        .fn_sigs
        .get("bank.Account::deposit")
        .expect("bank.Account::deposit must be registered under the dotted key");
    let store_sig = output
        .fn_sigs
        .get("store.Account::deposit")
        .expect("store.Account::deposit must be registered under the dotted key");
    assert!(
        matches!(bank_sig.return_type, Ty::I64),
        "bank.Account::deposit returns i64, got {:?}",
        bank_sig.return_type
    );
    assert!(
        matches!(store_sig.return_type, Ty::Bool),
        "store.Account::deposit returns bool (the 2nd import must not \
         clobber the 1st), got {:?}",
        store_sig.return_type
    );
    assert!(
        !output.fn_sigs.contains_key("Account::deposit"),
        "no bare receive-fn key may be written for module actors"
    );
}

// ── fixture 9: bare local-first resolution + ambiguity error ────────────────

#[test]
fn bare_spawn_resolves_local_actor_over_imported_same_name() {
    let output = typecheck_with_modules(
        "
actor Account {
    var local_n: i64 = 0;
    receive fn deposit(n: i64) -> i64 { n }
}

fn main() {
    let a = spawn Account();
}
",
        &[(&["hew", "bank"], BANK_SRC)],
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:#?}",
        output.errors
    );
    let pids = local_pid_inner_names(&output);
    assert_eq!(
        pids,
        vec!["Account".to_string()],
        "bare spawn must resolve the root-local actor, not the import"
    );
}

#[test]
fn bare_spawn_of_doubly_exported_actor_is_typed_ambiguity_error() {
    let output = typecheck_with_modules(
        "
fn main() {
    let a = spawn Account();
}
",
        &[(&["hew", "bank"], BANK_SRC), (&["hew", "store"], STORE_SRC)],
    );
    let ambiguous: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::AmbiguousActorReference {
                    actor_name,
                    candidate_modules,
                } if actor_name == "Account"
                    && candidate_modules == &["bank".to_string(), "store".to_string()]
            )
        })
        .collect();
    assert_eq!(
        ambiguous.len(),
        1,
        "expected exactly one AmbiguousActorReference naming bank and store, \
         got errors: {:#?}",
        output.errors
    );
    let err = ambiguous[0];
    assert!(
        err.message.contains("`bank.Account`") && err.message.contains("`store.Account`"),
        "diagnostic must name both candidates; got: {}",
        err.message
    );
}

#[test]
fn bare_spawn_of_uniquely_exported_actor_resolves_to_that_module() {
    let output = typecheck_with_modules(
        "
fn main() {
    let a = spawn Account();
}
",
        &[(&["hew", "bank"], BANK_SRC)],
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:#?}",
        output.errors
    );
    let pids = local_pid_inner_names(&output);
    assert_eq!(
        pids,
        vec!["bank.Account".to_string()],
        "a bare name exported by exactly one module resolves to that module"
    );
}
