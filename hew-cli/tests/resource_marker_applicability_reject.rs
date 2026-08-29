//! Ownership-marker applicability must fail in the parser before checking.
//!
//! A type alias has no `ResourceMarker` field. The CLI must stop at an
//! ownership marker applied to one and retain the source span in its rendered
//! diagnostic.

mod support;

use std::process::Command;

use support::{hew_binary, strip_ansi};

#[test]
fn ownership_marker_on_type_alias_fails_closed_before_hir() {
    let fixture = support::tempdir();
    let source = fixture.path().join("resource_alias_marker.hew");
    std::fs::write(
        &source,
        r"#[resource]
type Metres = i64;

fn main() {}
",
    )
    .expect("write resource-alias fixture");

    let output = Command::new(hew_binary())
        .args(["check", source.to_str().expect("fixture path is UTF-8")])
        .current_dir(fixture.path())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "ownership marker on type alias must stop before HIR; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("E_RESOURCE_MARKER_TARGET")
            && stderr.contains("#[resource]")
            && stderr.contains("resource_alias_marker.hew:1:1")
            && stderr.contains("^^^^^^^^^^^"),
        "CLI must render the ownership-marker error at its source attribute:\n{stderr}"
    );
}

#[test]
fn ownership_markers_on_nominal_types_still_check_clean() {
    let fixture = support::tempdir();
    let source = fixture.path().join("nominal_resource_markers.hew");
    std::fs::write(
        &source,
        r"#[resource]
type ResourceToken { id: i64 }
impl ResourceToken {
    fn close(self) {}
}

#[linear]
type LinearTicket { id: i64 }
impl LinearTicket {
    fn redeem(consuming self) {}
}

fn main() {
    let resource = ResourceToken { id: 1 };
    resource.close();
    let ticket = LinearTicket { id: 2 };
    ticket.redeem();
}
",
    )
    .expect("write nominal ownership fixture");

    let output = Command::new(hew_binary())
        .args(["check", source.to_str().expect("fixture path is UTF-8")])
        .current_dir(fixture.path())
        .output()
        .expect("invoke hew check");

    assert!(
        output.status.success(),
        "nominal ownership markers must remain valid; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}
