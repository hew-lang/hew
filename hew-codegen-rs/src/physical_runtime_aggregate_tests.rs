//! Runtime out-parameters must publish ownership before projected cleanup.

use super::*;

#[derive(Debug, Default)]
struct Owner {
    references: usize,
    copies: usize,
    index_copies: usize,
    releases: usize,
}

thread_local! {
    static INDEX_ACTIVE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    static OWNERS: RefCell<BTreeMap<usize, Owner>> = RefCell::default();
    static INVALID: RefCell<Vec<usize>> = const { RefCell::new(Vec::new()) };
}

unsafe extern "C" fn literal(data: *const u8, len: u32, out: *mut *mut c_void) {
    // SAFETY: generated literal calls supply valid UTF-8 and a writable result.
    unsafe { hew_runtime::string::hew_string_literal_new(data, len, out.cast()) };
    // SAFETY: the runtime just initialized this output slot.
    let pointer = unsafe { out.read() };
    OWNERS.with_borrow_mut(|owners| {
        owners.insert(
            pointer as usize,
            Owner {
                references: 1,
                ..Owner::default()
            },
        );
    });
}

unsafe extern "C" fn copy(pointer: *const c_void) -> *mut c_void {
    let valid = OWNERS.with_borrow_mut(|owners| {
        let Some(owner) = owners
            .get_mut(&(pointer as usize))
            .filter(|owner| owner.references > 0)
        else {
            return false;
        };
        owner.references += 1;
        owner.copies += 1;
        owner.index_copies += usize::from(INDEX_ACTIVE.get());
        true
    });
    if !valid {
        INVALID.with_borrow_mut(|invalid| invalid.push(pointer as usize));
        return std::ptr::null_mut();
    }
    // SAFETY: the pointer has a live registered runtime owner.
    unsafe { hew_runtime::string::hew_string_clone(pointer.cast()).cast() }
}

unsafe extern "C" fn drop_string(pointer: *mut c_void) {
    let valid = OWNERS.with_borrow_mut(|owners| {
        let Some(owner) = owners
            .get_mut(&(pointer as usize))
            .filter(|owner| owner.references > 0)
        else {
            return false;
        };
        owner.references -= 1;
        owner.releases += 1;
        true
    });
    if valid {
        // SAFETY: consume exactly one registered runtime reference.
        unsafe { hew_runtime::string::hew_string_drop(pointer.cast()) };
    } else {
        INVALID.with_borrow_mut(|invalid| invalid.push(pointer as usize));
    }
}

unsafe extern "C" fn vector_index(vector: *const c_void, index: i64, out: *mut c_void) -> bool {
    INDEX_ACTIVE.set(true);
    // SAFETY: preserve the generated call's checked vector and writable result ABI.
    let found = unsafe { hew_runtime::vec::hew_vec_get_clone(vector.cast(), index, out) };
    INDEX_ACTIVE.set(false);
    found
}

unsafe extern "C" fn map_index(
    map: *const c_void,
    key: *const c_void,
    out: *mut c_void,
    present: *mut bool,
    fault: *mut *mut c_void,
) -> i32 {
    INDEX_ACTIVE.set(true);
    // SAFETY: preserve the generated map lookup ABI and its output slots.
    let status = unsafe {
        hew_runtime::hashmap::hew_hashmap_get_clone_layout(map.cast(), key, out, present, fault)
    };
    INDEX_ACTIVE.set(false);
    status
}

fn collection_module(map: bool) -> PhysicalModule {
    let setup = if map {
        "var entries: HashMap<i64, Entry> = HashMap.new(); entries.insert(0, Entry { key: \"alpha\", value: \"omega\" });"
    } else {
        "var entries: Vec<Entry> = Vec.new(); entries.push(Entry { key: \"alpha\", value: \"omega\" });"
    };
    physical(&format!(
        r"
        type Entry {{ key: string, value: string }}
        fn inspect(index: i64, take: bool) -> i64 {{
            {setup}
            let entry = entries[index];
            if take {{
                let {{ key, value }} = entry;
                key.len() + value.len()
            }} else {{
                entry.key.len() + entry.value.len()
            }}
        }}
        fn main() -> i64 {{ inspect(0, false) }}
    "
    ))
}

fn check_owners(failed: bool) -> String {
    let owners = OWNERS.with_borrow_mut(std::mem::take);
    let invalid = INVALID.with_borrow_mut(std::mem::take);
    let report = format!("owners={owners:?}; invalid={invalid:?}");
    // Release leaked references after recording evidence, so a failing regression
    // does not leave allocations in the test runner.
    for (&pointer, owner) in &owners {
        for _ in 0..owner.references {
            // SAFETY: these recorded references were not consumed by generated code.
            unsafe { hew_runtime::string::hew_string_drop((pointer as *mut c_void).cast()) };
        }
    }
    if owners.len() == 2
        && invalid.is_empty()
        && owners.values().all(|owner| {
            owner.references == 0
                && owner.releases == 1 + owner.copies
                && owner.index_copies == usize::from(!failed)
        })
    {
        String::new()
    } else {
        report
    }
}

fn run_index(map: bool, failed: bool) {
    let physical = collection_module(map);
    let callee = physical
        .callables
        .iter()
        .find(|callee| callee.declaration.full_path() == "inspect")
        .unwrap();
    let symbol = emitted_symbol(&physical, callee);
    let mut failures = Vec::new();
    for optimized in [false, true] {
        for take in [false, true] {
            let ctx = Context::create();
            let llvm = llvm(&ctx, &physical);
            llvm.get_function(&symbol)
                .unwrap()
                .set_linkage(Linkage::External);
            let engine = engine(&llvm, optimized);
            for (name, address) in [
                ("hew_string_literal_new", literal as *const () as usize),
                ("hew_string_clone", copy as *const () as usize),
                ("hew_string_drop", drop_string as *const () as usize),
            ] {
                engine.add_global_mapping(&llvm.get_function(name).unwrap(), address);
            }
            let (name, address) = if map {
                (
                    "hew_hashmap_get_clone_layout",
                    map_index as *const () as usize,
                )
            } else {
                ("hew_vec_get_clone", vector_index as *const () as usize)
            };
            engine.add_global_mapping(&llvm.get_function(name).unwrap(), address);
            type Inspect = unsafe extern "C" fn(i64, u8, *mut i64, *mut *mut c_void) -> i32;
            let mut output = -1;
            let mut fault = std::ptr::null_mut();
            // SAFETY: private callable scalar parameters and result/fault slots
            // match the emitted ABI; all owners originate in the real runtime.
            let status = unsafe {
                engine.get_function::<Inspect>(&symbol).unwrap().call(
                    i64::from(failed),
                    u8::from(take),
                    &raw mut output,
                    &raw mut fault,
                )
            };
            let has_fault = !fault.is_null();
            if has_fault {
                // SAFETY: failure returns one owned runtime fault.
                unsafe { hew_runtime::fault::hew_fault_drop(fault.cast()) };
            }
            let owners = check_owners(failed);
            if !owners.is_empty()
                || status
                    != if failed {
                        HEW_TRAP_INDEX_OUT_OF_BOUNDS
                    } else {
                        0
                    }
                || has_fault != failed
                || output != if failed { -1 } else { 10 }
            {
                failures.push(format!("O2={optimized} take={take} status={status} output={output} fault={has_fault}: {owners}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "map={map} failed={failed}:\n{}",
        failures.join("\n")
    );
}

#[test]
fn vector_indexed_aggregate_copies_and_releases_fields_at_o0_o2() {
    run_index(false, false);
}

#[test]
fn map_indexed_aggregate_copies_and_releases_fields_at_o0_o2() {
    run_index(true, false);
}

#[test]
fn failed_vector_index_never_copies_or_drops_result_fields_at_o0_o2() {
    run_index(false, true);
}

#[test]
fn failed_map_index_never_copies_or_drops_result_fields_at_o0_o2() {
    run_index(true, true);
}
