use super::check_source;
use crate::check::effects::SuspensionEffect;

#[test]
fn canonical_io_declarations_publish_suspension_without_symbol_alias_authority() {
    let source = "extern \"C\" { fn hew_file_read_bytes(path: string) -> bytes; } pub fn read(path: string) -> bytes { unsafe { await hew_file_read_bytes(path) } }";
    let module = ["std".to_string(), "fs".to_string()];
    let output = super::check_source_in_canonical_std_module(source, &module);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output.direct_call_targets.values().any(|target| matches!(
        target,
        crate::CallTarget::Runtime(crate::RuntimeCallFamily::AsyncIo(
            crate::runtime_call::AsyncIoOp::FileReadBytes
        ))
    )));
    assert!(output.suspension_effects.bodies.iter().any(|(body, effect)|
        matches!(body, crate::check::effects::EffectBody::Declaration(id) if id.full_path() == "std.fs.read")
            && *effect == SuspensionEffect::MaySuspend));
    let output =
        super::check_source_in_canonical_std_module(&source.replace("await ", ""), &module);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert!(output.suspension_effects.bodies.iter().any(|(body, effect)|
        matches!(body, crate::check::effects::EffectBody::Declaration(id) if id.full_path() == "std.fs.read")
            && *effect == SuspensionEffect::MaySuspend));
    let user = check_source(&source.replace("await ", ""));
    assert!(user.errors.is_empty(), "{:?}", user.errors);
    assert!(user.direct_call_targets.values().all(|target| !matches!(
        target,
        crate::CallTarget::Runtime(crate::RuntimeCallFamily::AsyncIo(_))
    )));
}
