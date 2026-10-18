//! Static trait calls retain checked method arguments until impl selection.

use hew_hir::{lower_program_host_target, HirItem, ResolutionCtx};
use hew_sir::{lower_module, verify_module, SirLoweringStatus};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn checked_hir(source: &str) -> (hew_hir::HirModule, hew_types::TypeCheckOutput) {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(Vec::new())).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let lowered = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
    (lowered.module, checked)
}

#[test]
fn generic_trait_method_instances_preserve_owned_values() {
    let (hir, checked) = checked_hir(include_str!(
        "../../tests/core-acceptance/cases/generic-trait-method-values.hew"
    ));
    let lowered = lower_module(&hir, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    let errors = verify_module(&lowered.module);
    assert!(
        errors.is_empty(),
        "{errors:?}\n{}",
        hew_sir::dump_sir(&lowered.module)
    );
}

#[test]
fn projected_impl_and_method_binders_use_the_checked_body_schema() {
    let source = format!(
        "{}\n{}",
        include_str!("../../tests/core-acceptance/cases/projection_bound_support/replies.hew"),
        r#"trait Exchange {
    fn keep<V>(self, consume value: V) -> V;
    fn respond<V: Message>(self, value: V) -> V.Reply;
}

type Pair<L, R> {
    left: L;
    right: R;
}

impl<T: Message, U> Exchange for Pair<Vec<U>, T> where T.Reply: ReplyCode {
    fn keep<V>(self, consume value: V) -> V {
        assert(self.right.reply().reply_code() == 42);
        value
    }
    fn respond<V: Message>(self, value: V) -> V.Reply {
        value.reply()
    }
}

fn exchange<E: Exchange>(value: E) {
    assert(value.keep("owned".to_upper()) == "OWNED");
    assert(code(value.respond(Ping {})) == 42);
}

fn main() {
    assert(code(Pending { message: Ping {} }) == 42);
    exchange(Pair { left: [7], right: Ping {} });
}
"#
    );
    let (hir, checked) = checked_hir(&source);
    let lowered = lower_module(&hir, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    let errors = verify_module(&lowered.module);
    assert!(
        errors.is_empty(),
        "{errors:?}\n{}",
        hew_sir::dump_sir(&lowered.module)
    );
}

#[test]
fn generic_impl_default_uses_registered_receiver_binders() {
    let (hir, checked) = checked_hir(
        r#"trait Keep {
    fn keep<V>(self, consume value: V) -> V { value }
}

type Pair<L, R> { left: L; right: R; }
impl<A, B> Keep for Pair<B, A> {}
fn keep<K: Keep>(value: K) -> string { value.keep("owned".to_upper()) }
fn main() {
    let pair = Pair { left: 7, right: true };
    assert(keep(pair) == "OWNED");
    assert(pair.keep("direct".to_upper()) == "DIRECT");
}
"#,
    );
    let lowered = lower_module(&hir, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    assert!(verify_module(&lowered.module).is_empty());
}

#[test]
fn impl_binders_can_come_from_applied_trait_arguments() {
    let (hir, checked) = checked_hir(
        r#"trait Echo<T> {
    fn echo(self, consume value: T) -> T;
}
type Fixed {}
impl<T> Echo<T> for Fixed {
    fn echo(self, consume value: T) -> T { value }
}
fn echo<E: Echo<string>>(value: E) -> string { value.echo("bound".to_upper()) }
fn main() { assert(echo(Fixed {}) == "BOUND"); }
"#,
    );
    let lowered = lower_module(&hir, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    assert!(verify_module(&lowered.module).is_empty());
}

#[test]
fn structural_trait_calls_retain_the_exact_filler_witness() {
    let (hir, checked) = checked_hir(
        r"trait Ready { fn ready(self) -> i64; }
trait Other { fn ready(self) -> i64; }
type Wrap<T> { value: T; }
impl<T> Other for Wrap<T> { fn ready(self) -> i64 { 42 } }
fn ready<R: Ready>(value: R) -> i64 { value.ready() }
fn main() -> i64 { ready(Wrap { value: 7 }) }
",
    );
    let lowered = lower_module(&hir, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    assert!(verify_module(&lowered.module).is_empty());
}

#[test]
fn generic_trait_method_requires_its_checked_instantiation() {
    let source = r"trait Identity {
    fn keep<U>(self, consume value: U) -> U;
}

type Holder {
    value: i64;
}

impl Identity for Holder {
    fn keep<U>(self, consume value: U) -> U {
        value
    }
}

fn via<T: Identity>(item: T) -> i64 {
    item.keep(42)
}

fn main() -> i64 {
    via(Holder { value: 0 })
}
";
    let (hir, checked) = checked_hir(source);
    let valid = lower_module(&hir, &checked);
    assert!(
        valid
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        valid.callable_statuses
    );
    assert!(verify_module(&valid.module).is_empty());

    let site = hir
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "via" => {
                Some(function.body.tail.as_ref().expect("bound call").site)
            }
            _ => None,
        })
        .expect("generic caller");

    // The argument value and result still identify i64, but downstream stages
    // must not reconstruct a missing or malformed checker instantiation.
    for replacement in [None, Some(vec![ResolvedTy::I64, ResolvedTy::String])] {
        let mut broken = hir.clone();
        if let Some(arguments) = replacement {
            broken.call_site_type_args.insert(site, arguments);
        } else {
            assert!(broken.call_site_type_args.remove(&site).is_some());
        }
        let lowered = lower_module(&broken, &checked);
        assert!(
            lowered.callable_statuses.iter().any(|(_, status)| matches!(
                status,
                SirLoweringStatus::Unsupported { reason, .. }
                    if reason.contains("requires 1 checker-resolved method type argument(s)")
            )),
            "{:?}",
            lowered.callable_statuses
        );
    }
}
