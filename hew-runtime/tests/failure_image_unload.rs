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

static IMAGE_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

#[test]
fn last_failure_outlives_its_unloaded_image() {
    let _guard = IMAGE_TEST_LOCK.lock().unwrap();
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
        hew_fault_set_site_in_image(fault, 0, files.cast(), sites.cast(), std::ptr::null());
        assert_eq!(libc::dlclose(handle), 0);
        let mut info: libc::Dl_info = std::mem::zeroed();
        assert_eq!(
            libc::dladdr(files, &raw mut info),
            0,
            "image must be unloaded"
        );
        let record = &*hew_debug_last_fault();
        assert_eq!(CStr::from_ptr(record.site.file).to_bytes(), b"retired.hew");
        assert_eq!((record.site.line, record.site.col), (2, 3));
        // Reporting the retained fault also reads its source after retirement.
        assert_eq!(hew_fault_report(fault), 0);
        hew_fault_drop(fault);
    }
}

#[cfg(target_arch = "x86_64")]
#[test]
fn frames_from_the_originating_image_survive_unload() {
    use hew_debug_abi::SymbolRecord;
    use std::sync::atomic::{AtomicPtr, Ordering};

    static FAULT: AtomicPtr<hew_runtime::fault::HewFault> = AtomicPtr::new(std::ptr::null_mut());
    extern "C" fn capture(files: *const u8, sites: *const u8, symbols: *const u8) {
        // SAFETY: the wrapper supplies live tables from its loaded image.
        unsafe {
            let fault = hew_fault_new(212);
            hew_fault_set_site_in_image(fault, 0, files, sites, symbols);
            FAULT.store(fault, Ordering::Release);
        }
    }

    let _guard = IMAGE_TEST_LOCK.lock().unwrap();
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("image.c");
    let assembly = dir.path().join("tables.S");
    let image = dir.path().join("image.so");
    let files = TableWriter::files(&[FileRecord {
        name: "retired-frame.hew",
        line_starts: &[0],
        md5: [0; 16],
    }]);
    let sites = TableWriter::sites(&[SiteEntry {
        file_id: 0,
        byte_offset: 0,
        column: 1,
    }]);
    let (symbols, slots) = TableWriter::symtab(&[SymbolRecord {
        name: "image.worker",
        file_id: 0,
        decl_line: 1,
    }]);
    let bytes = |data: &[u8]| data.iter().map(u8::to_string).collect::<Vec<_>>().join(",");
    std::fs::write(&source, r#"
extern const unsigned char files[], sites[], symbols[];
__attribute__((noinline, visibility("hidden")))
void image_worker(void (*callback)(const unsigned char *, const unsigned char *, const unsigned char *)) {
    callback(files, sites, symbols);
    __asm__ volatile("" ::: "memory");
}
void run_trace(void (*callback)(const unsigned char *, const unsigned char *, const unsigned char *)) {
    image_worker(callback);
}
"#).unwrap();
    std::fs::write(&assembly, format!(
        ".section .rodata\n.globl files\nfiles: .byte {}\n.globl sites\nsites: .byte {}\n.globl symbols\nsymbols: .byte {}\n.long image_worker - symbols\n.long image_worker + 4096 - symbols\n.byte {}\n.section .note.GNU-stack,\"\",@progbits\n",
        bytes(&files), bytes(&sites), bytes(&symbols[..slots[0].start]), bytes(&symbols[slots[0].end + 4..])
    )).unwrap();
    assert!(Command::new("cc")
        .args(["-shared", "-fPIC", "-fno-omit-frame-pointer"])
        .arg(&source)
        .arg(&assembly)
        .arg("-o")
        .arg(&image)
        .status()
        .unwrap()
        .success());
    let path = CString::new(image.as_os_str().as_bytes()).unwrap();
    // SAFETY: this test owns the image and preserves it through the callback.
    unsafe {
        let handle = libc::dlopen(path.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL);
        assert!(!handle.is_null());
        let entry = libc::dlsym(handle, c"run_trace".as_ptr());
        assert!(!entry.is_null());
        let run: unsafe extern "C" fn(extern "C" fn(*const u8, *const u8, *const u8)) =
            std::mem::transmute(entry);
        run(capture);
        let fault = FAULT.swap(std::ptr::null_mut(), Ordering::AcqRel);
        assert!(!fault.is_null());
        assert_eq!(libc::dlclose(handle), 0);
        let mut info: libc::Dl_info = std::mem::zeroed();
        assert_eq!(
            libc::dladdr(entry, &raw mut info),
            0,
            "image must be unloaded"
        );
        let record = &*hew_debug_last_fault();
        let frame = record
            .frames()
            .iter()
            .find(|frame| {
                !frame.hew_fn.is_null()
                    && CStr::from_ptr(frame.hew_fn).to_bytes() == b"image.worker"
            })
            .expect("origin image frame");
        assert_eq!(CStr::from_ptr(frame.file).to_bytes(), b"retired-frame.hew");
        assert_eq!(frame.line, 1);
        assert_eq!(hew_fault_report(fault), 0);
        hew_fault_drop(fault);
    }
}
