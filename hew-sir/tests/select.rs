use hew_sir::{BoundaryDecision, CallResult, SemTerminator, SuspendKind};
use hew_types::ResolvedTy;

#[path = "support/select.rs"]
mod fixture;

#[test]
fn task_select_preserves_borrowed_sources_and_typed_resume() {
    for (count, timeout) in [(1, false), (2, false), (2, true), (0, true)] {
        let diagnostics = hew_sir::verify_module(&fixture::module(count, timeout));
        assert!(diagnostics.is_empty(), "{count}/{timeout}: {diagnostics:?}");
    }
}

#[test]
fn task_select_rejects_invalid_observation_contracts() {
    for mutation in 0..6 {
        let mut module = fixture::module(2, true);
        let SemTerminator::Suspend {
            kind,
            inputs,
            result,
            resumes,
            ..
        } = &mut module.functions[0].blocks[0].terminator
        else {
            unreachable!()
        };
        match mutation {
            0 => inputs[0].decision = BoundaryDecision::Move,
            1 => inputs[2].decision = BoundaryDecision::Borrow,
            2 => {
                inputs.swap(0, 2);
            }
            3 => {
                let CallResult::Value(result) = result else {
                    unreachable!()
                };
                result.ty = ResolvedTy::I32;
            }
            4 => {
                resumes.push(resumes[0].clone());
            }
            5 => {
                inputs.clear();
                *kind = SuspendKind::Select { has_timeout: false };
            }
            _ => unreachable!(),
        }
        let errors = hew_sir::verify_module(&module);
        assert!(
            errors.iter().any(|error| format!("{error:?}")
                .contains("suspension has no matching input/result/resume contract")),
            "{mutation}: {errors:?}"
        );
    }
}
