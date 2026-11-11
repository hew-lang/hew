//! A debugger's last failure survives retirement of the image that raised it.

#![cfg(target_os = "linux")]

use hew_debug_abi::{FileRecord, SiteEntry, TableWriter};
use hew_runtime::debug_accessors::hew_debug_last_fault;
use hew_runtime::fault::{
    hew_fault_drop, hew_fault_new, hew_fault_report, hew_fault_set_site_in_image,
};
use std::ffi::{CStr, CString};
use std::os::unix::ffi::OsStrExt;
use std::process::Command;

#[test]
fn last_failure_outlives_its_unloaded_image() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("tables.c");
    let image = dir.path().join("tables.so");
    let files = TableWriter::files(&[FileRecord {
        name: "retired.hew",
        line_starts: &[0, 10],
        md5: [0; 16],
    }]);
    let sites = TableWriter::sites(&[SiteEntry {
        file_id: 0,
        byte_offset: 12,
        column: 3,
    }]);
    let bytes = |data: &[u8]| data.iter().map(u8::to_string).collect::<Vec<_>>().join(",");
    std::fs::write(
        &source,
        format!(
            "const unsigned char files[] = {{{}}};\nconst unsigned char sites[] = {{{}}};\n",
            bytes(&files),
            bytes(&sites)
        ),
    )
    .unwrap();
    assert!(Command::new("cc")
        .args(["-shared", "-fPIC"])
        .arg(&source)
        .arg("-o")
        .arg(&image)
        .status()
        .unwrap()
        .success());
    let image_path = CString::new(image.as_os_str().as_bytes()).unwrap();
    // SAFETY: this test exclusively owns the trusted shared object and its handle.
    unsafe {
        let handle = libc::dlopen(image_path.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL);
        assert!(!handle.is_null());
        let files = libc::dlsym(handle, c"files".as_ptr());
        let sites = libc::dlsym(handle, c"sites".as_ptr());
        assert!(!files.is_null() && !sites.is_null());
        let fault = hew_fault_new(212);
        hew_fault_set_site_in_image(fault, 0, files.cast(), sites.cast());
        assert_eq!(libc::dlclose(handle), 0);
        let mut info: libc::Dl_info = std::mem::zeroed();
        assert_eq!(libc::dladdr(files, &mut info), 0, "image must be unloaded");
        let record = &*hew_debug_last_fault();
        assert_eq!(CStr::from_ptr(record.site.file).to_bytes(), b"retired.hew");
        assert_eq!((record.site.line, record.site.col), (2, 3));
        // Reporting the retained fault also reads its source after retirement.
        assert_eq!(hew_fault_report(fault), 0);
        hew_fault_drop(fault);
    }
}
