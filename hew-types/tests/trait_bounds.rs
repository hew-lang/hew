use hew_types::error::TypeErrorKind;
use hew_types::Checker;

#[test]
fn trait_bound_violation_reports_error() {
    let source = r#"
        trait Describable {
            fn describe(self) -> string;
        }

        type Dog {
            name: string;
        }

        impl Describable for Dog {
            fn describe(self) -> string {
                self.name
            }
        }

        fn show<T: Describable>(item: T) {
            println(item.describe());
        }

        fn main() {
            show(42);
        }
    "#;

    let parse = hew_parser::parse(source);
    assert!(parse.errors.is_empty(), "parser errors: {:?}", parse.errors);

    let mut checker = Checker::new();
    let output = checker.check_program(&parse.program);
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::BoundsNotSatisfied),
        "expected a BoundsNotSatisfied error, got {:?}",
        output.errors
    );
}
