//! Direct checks of production Vec operations on arbitrary valid bounded states.
use super::*;
fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
    assert!(false, "vector: unexpected panic");
    kani::assume(false);
    loop {}
}
unsafe fn relocated_realloc(old: *mut u8, layout: Layout, size: usize) -> *mut u8 {
    let fresh = alloc(Layout::from_size_align(size, layout.align()).unwrap());
    assert!(!fresh.is_null());
    ptr::copy_nonoverlapping(old, fresh, layout.size().min(size));
    dealloc(old, layout);
    fresh
}
kani::stub_set!(
    checks,
    stub(core::panicking::panic_fmt, unexpected_panic),
    stub(std::alloc::realloc, relocated_realloc),
);
unsafe fn unexpected_abort() -> ! {
    assert!(false, "vector: valid operation aborted");
    kani::assume(false);
    loop {}
}
fn unexpected_process_abort() -> ! {
    unsafe { unexpected_abort() }
}
kani::stub_set!(
    normal_checks,
    use_stub_set(checks),
    stub(crate::vec::abort_vector, unexpected_abort),
    stub(std::process::abort, unexpected_process_abort),
);
fn header(layout: HewValueLayout) -> HewVec {
    HewVec {
        data: ptr::null_mut(),
        len: 0,
        front: 0,
        cap: 0,
        elem_size: layout.size,
        elem_kind: ElemKind::Plain,
        layout: ptr::null(),
        layout_storage: layout,
    }
}

trait Scalar: Copy {
    fn arbitrary() -> Self;
    fn bits(self) -> u64;
}
macro_rules! integer_scalar {
    ($($ty:ty),*) => {$(impl Scalar for $ty {
        fn arbitrary()->Self {kani::any()}
        fn bits(self)->u64 {self as u64}
    })*};
}
integer_scalar!(bool, i8, u8, i16, u16, i32, i64);
impl Scalar for f32 {
    fn arbitrary() -> Self {
        Self::from_bits(kani::any())
    }
    fn bits(self) -> u64 {
        self.to_bits() as u64
    }
}
impl Scalar for f64 {
    fn arbitrary() -> Self {
        Self::from_bits(kani::any())
    }
    fn bits(self) -> u64 {
        self.to_bits()
    }
}
impl Scalar for *mut c_void {
    fn arbitrary() -> Self {
        kani::any::<usize>() as Self
    }
    fn bits(self) -> u64 {
        self as usize as u64
    }
}
macro_rules! scalar_operations {
    ($proof:ident,$ty:ty,$push:ident,$get:ident,$set:ident,$pop:ident) => {
        #[kani::proof]
        #[kani::unwind(6)]
        #[kani::use_stub_set(normal_checks)]
        fn $proof() {
            let first = <$ty as Scalar>::arbitrary();
            let second = <$ty as Scalar>::arbitrary();
            let third = <$ty as Scalar>::arbitrary();
            unsafe {
                let mut layout = plain_layout();
                layout.size = size_of::<$ty>();
                layout.align = align_of::<$ty>();
                let mut v = header(layout);
                v.layout = ptr::addr_of!(v.layout_storage);
                v.cap = 4;
                v.len = 1;
                v.data = allocate_buffer(&v, 4);
                v.data.cast::<$ty>().write(first);
                $push(&mut v, second);
                assert_eq!(v.len, 2, "vector: typed push length");
                assert_eq!(
                    v.data.cast::<$ty>().read().bits(),
                    first.bits(),
                    "vector: typed push changed prefix"
                );
                assert_eq!(
                    v.data.cast::<$ty>().add(1).read().bits(),
                    second.bits(),
                    "vector: typed push used wrong stride/value"
                );
                assert_eq!(
                    $get(&mut v, 1).bits(),
                    second.bits(),
                    "vector: typed get used wrong stride/value"
                );
                $set(&mut v, 0, third);
                assert_eq!(
                    v.data.cast::<$ty>().read().bits(),
                    third.bits(),
                    "vector: typed set used wrong stride/value"
                );
                assert_eq!(
                    v.data.cast::<$ty>().add(1).read().bits(),
                    second.bits(),
                    "vector: typed set damaged neighbor"
                );
                assert_eq!(
                    $pop(&mut v).bits(),
                    second.bits(),
                    "vector: typed pop used wrong stride/value"
                );
                assert_eq!(v.len, 1, "vector: typed pop length");
                assert_eq!(
                    v.data.cast::<$ty>().read().bits(),
                    third.bits(),
                    "vector: typed pop changed prefix"
                );
                release_buffer(&v);
            }
        }
    };
}
scalar_operations!(
    vector_bool_ops,
    bool,
    hew_vec_push_bool,
    hew_vec_get_bool,
    hew_vec_set_bool,
    hew_vec_pop_bool
);
scalar_operations!(
    vector_i8_ops,
    i8,
    hew_vec_push_i8,
    hew_vec_get_i8,
    hew_vec_set_i8,
    hew_vec_pop_i8
);
scalar_operations!(
    vector_u8_ops,
    u8,
    hew_vec_push_u8,
    hew_vec_get_u8,
    hew_vec_set_u8,
    hew_vec_pop_u8
);
scalar_operations!(
    vector_i16_ops,
    i16,
    hew_vec_push_i16,
    hew_vec_get_i16,
    hew_vec_set_i16,
    hew_vec_pop_i16
);
scalar_operations!(
    vector_u16_ops,
    u16,
    hew_vec_push_u16,
    hew_vec_get_u16,
    hew_vec_set_u16,
    hew_vec_pop_u16
);
scalar_operations!(
    vector_i32_ops,
    i32,
    hew_vec_push_i32,
    hew_vec_get_i32,
    hew_vec_set_i32,
    hew_vec_pop_i32
);
scalar_operations!(
    vector_i64_ops,
    i64,
    hew_vec_push_i64,
    hew_vec_get_i64,
    hew_vec_set_i64,
    hew_vec_pop_i64
);
scalar_operations!(
    vector_f32_ops,
    f32,
    hew_vec_push_f32,
    hew_vec_get_f32,
    hew_vec_set_f32,
    hew_vec_pop_f32
);
scalar_operations!(
    vector_f64_ops,
    f64,
    hew_vec_push_f64,
    hew_vec_get_f64,
    hew_vec_set_f64,
    hew_vec_pop_f64
);
scalar_operations!(
    vector_pointer_ops,
    *mut c_void,
    hew_vec_push_ptr,
    hew_vec_get_ptr,
    hew_vec_set_ptr,
    hew_vec_pop_ptr
);
fn plain_layout() -> HewValueLayout {
    HewValueLayout {
        code_owner: ptr::null(),
        managed: ptr::null(),
        release_start: None,
        size: 4,
        align: 4,
        ownership_kind: HewTypeOwnershipKind::Plain,
        clone_fn: None,
        drop_fn: None,
    }
}
unsafe fn seed(v: &mut HewVec, values: &[i32]) {
    v.layout = ptr::addr_of!(v.layout_storage);
    v.cap = 4;
    v.len = values.len();
    v.data = allocate_buffer(v, 4);
    for (i, x) in values.iter().enumerate() {
        v.data.cast::<i32>().add(i).write(*x);
    }
}
unsafe fn same(v: &mut HewVec, expected: &[i32]) {
    assert_eq!(v.len, expected.len(), "vector: wrong length");
    assert!(v.len <= v.cap, "vector: length exceeds backing");
    for (i, x) in expected.iter().enumerate() {
        assert_eq!(
            hew_vec_get_i32(v, i as i64),
            *x,
            "vector: wrong element/order"
        );
    }
}
unsafe fn share(source: &HewVec, target: &mut HewVec) {
    target.data = source.data;
    target.cap = source.cap;
    target.len = source.len;
    target.layout = ptr::addr_of!(target.layout_storage);
    (*buffer_owner(source)).fetch_add(1, Ordering::Relaxed);
}
#[kani::proof]
#[kani::unwind(66)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn vector_capacity_covers_request_without_wrapping() {
    let capacity: usize = kani::any();
    let needed: usize = kani::any();
    let actual = capacity_for(capacity, needed);
    // Independent wide-arithmetic policy: smallest doubled capacity meeting
    // the request, or rejection if that capacity cannot fit a machine word.
    let mut expected = if capacity == 0 {
        4u128
    } else {
        capacity as u128
    };
    if capacity >= needed {
        expected = capacity as u128;
    } else {
        for _ in 0..usize::BITS {
            if expected < needed as u128 {
                expected *= 2;
            }
        }
    }
    assert_eq!(
        actual,
        usize::try_from(expected).ok(),
        "vector: growth wrapped, rejected a representable capacity, or lost the request"
    );
}

#[kani::proof]
#[kani::unwind(6)]
#[kani::use_stub_set(normal_checks)]
fn vector_shared_set_preserves_other_owner() {
    let a: i32 = kani::any();
    let b: i32 = kani::any();
    unsafe {
        let mut source = header(plain_layout());
        seed(&mut source, &[a]);
        let mut target = header(plain_layout());
        share(&source, &mut target);
        hew_vec_set_i32(&mut target, 0, b);
        same(&mut source, &[a]);
        same(&mut target, &[b]);
        release_buffer(&source);
        same(&mut target, &[b]);
        release_buffer(&target);
    }
}

static LAYOUT_REJECT: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
unsafe fn checked_layout_abort() -> ! {
    assert!(
        LAYOUT_REJECT.load(Ordering::Relaxed),
        "vector: rejected a representable buffer"
    );
    kani::cover!(true, "vector cover: buffer overflow rejected");
    kani::assume(false);
    loop {}
}
#[kani::proof]
#[kani::use_stub_set(checks)]
#[kani::stub(crate::vec::abort_vector, checked_layout_abort)]
fn vector_buffer_layout_checks_full_width_extents() {
    let capacity: usize = kani::any();
    let width: usize = kani::any();
    let shift: u32 = kani::any();
    kani::assume(shift < usize::BITS);
    let align = 1usize << shift;
    kani::assume(width & (align - 1) == 0);
    let mut descriptor = plain_layout();
    descriptor.size = width;
    descriptor.align = align;
    let mut vector = header(descriptor);
    vector.layout = ptr::addr_of!(vector.layout_storage);
    let offset = (align as u128).max(size_of::<AtomicUsize>() as u128);
    let payload = (capacity as u128 * width as u128).max(1);
    let total = offset + payload;
    let allocation_align = (align as u128).max(align_of::<AtomicUsize>() as u128);
    let rounded = (total + allocation_align - 1) & !(allocation_align - 1);
    let valid = rounded <= isize::MAX as u128;
    LAYOUT_REJECT.store(!valid, Ordering::Relaxed);
    let (layout, actual_offset) = unsafe { buffer_allocation(&vector, capacity) };
    assert!(valid, "vector: unrepresentable buffer accepted");
    assert_eq!(
        layout.size() as u128,
        total,
        "vector: allocation extent wrapped or lost payload"
    );
    assert_eq!(
        actual_offset as u128, offset,
        "vector: reference header overlaps payload"
    );
    assert_eq!(
        layout.align() as u128,
        allocation_align,
        "vector: wrong allocation alignment"
    );
    assert_eq!(
        actual_offset & (align - 1),
        0,
        "vector: misaligned element region"
    );
    kani::cover!(valid, "vector cover: representable buffer");
}

#[kani::proof]
#[kani::unwind(7)]
#[kani::use_stub_set(normal_checks)]
fn vector_cow_mutations_preserve_sequences() {
    let values: [i32; 4] = kani::any();
    let value: i32 = kani::any();
    let op: u8 = kani::any();
    let i: usize = kani::any();
    let j: usize = kani::any();
    kani::assume(op < 7 && i < 4 && j < 4);
    unsafe {
        let mut source = header(plain_layout());
        seed(&mut source, &values);
        let mut target = header(plain_layout());
        share(&source, &mut target);
        let mut expected = [values[0], values[1], values[2], values[3], 0];
        let mut len = 4;
        match op {
            0 => {
                hew_vec_push_i32(&mut target, value);
                expected[4] = value;
                len = 5;
            }
            1 => {
                hew_vec_set_i32(&mut target, i as i64, value);
                expected[i] = value;
            }
            2 => {
                assert_eq!(
                    hew_vec_pop_i32(&mut target),
                    values[3],
                    "vector: wrong popped value"
                );
                len = 3;
            }
            3 => {
                assert_eq!(
                    hew_vec_remove_at_i32(&mut target, i as i64),
                    values[i],
                    "vector: wrong removed value"
                );
                for k in i..3 {
                    expected[k] = values[k + 1];
                }
                len = 3;
            }
            4 => {
                hew_vec_swap(&mut target, i as i64, j as i64);
                expected[i] = values[j];
                expected[j] = values[i];
            }
            5 => {
                hew_vec_reverse_i32(&mut target);
                expected[..4].reverse();
            }
            _ => {
                hew_vec_truncate(&mut target, i as i64);
                len = i;
            }
        }
        same(&mut source, &values);
        same(&mut target, &expected[..len]);
        release_buffer(&source);
        same(&mut target, &expected[..len]);
        release_buffer(&target);
        kani::cover!(op == 0, "vector cover: shared growth");
        kani::cover!(op == 6, "vector cover: shared truncate");
    }
}

#[kani::proof]
#[kani::unwind(7)]
#[kani::use_stub_set(normal_checks)]
fn vector_interior_inputs_survive_reallocation() {
    let values: [i32; 4] = kani::any();
    let i: usize = kani::any();
    let j: usize = kani::any();
    let shared: bool = kani::any();
    kani::assume(i < 4 && j < 5);
    unsafe {
        let mut vector = header(plain_layout());
        seed(&mut vector, &values);
        let mut alias = header(plain_layout());
        if shared {
            share(&vector, &mut alias);
        }
        let source = hew_vec_get_owned(&vector, i as i64);
        hew_vec_push_owned(&mut vector, source);
        let mut expected = [values[0], values[1], values[2], values[3], values[i]];
        same(&mut vector, &expected);
        let source = hew_vec_get_owned(&vector, i as i64);
        hew_vec_set_owned(&mut vector, j as i64, source);
        expected[j] = values[i];
        same(&mut vector, &expected);
        if shared {
            same(&mut alias, &values);
            release_buffer(&alias);
        }
        release_buffer(&vector);
        kani::cover!(shared, "vector cover: shared interior source");
        kani::cover!(!shared, "vector cover: relocated interior source");
    }
}

#[kani::proof]
#[kani::unwind(8)]
#[kani::use_stub_set(normal_checks)]
fn vector_append_preserves_self_and_shared_sources() {
    let a: [i32; 3] = kani::any();
    let b: [i32; 3] = kani::any();
    let shared: bool = kani::any();
    let same_header: bool = kani::any();
    unsafe {
        let mut dst = header(plain_layout());
        seed(&mut dst, &a);
        let mut src = header(plain_layout());
        let tail = if same_header || shared { a } else { b };
        if same_header {
            hew_vec_append(&raw mut dst, &raw const dst);
        } else if shared {
            share(&dst, &mut src);
            hew_vec_append(&mut dst, &src);
        } else {
            seed(&mut src, &b);
            hew_vec_append(&mut dst, &src);
        }
        same(&mut dst, &[a[0], a[1], a[2], tail[0], tail[1], tail[2]]);
        if !same_header {
            same(&mut src, &tail);
            release_buffer(&src);
        }
        release_buffer(&dst);
        kani::cover!(same_header, "vector cover: self append");
        kani::cover!(
            !same_header && shared,
            "vector cover: append shared backing"
        );
    }
}

#[kani::proof]
#[kani::unwind(5)]
#[kani::use_stub_set(normal_checks)]
fn vector_get_clone_rejects_all_invalid_signed_indices() {
    let values: [i32; 3] = kani::any();
    let index: i64 = kani::any();
    let sentinel: i32 = kani::any();
    unsafe {
        let mut vector = header(plain_layout());
        seed(&mut vector, &values);
        let mut output = sentinel;
        let found = hew_vec_get_clone(&vector, index, (&raw mut output).cast());
        let valid = (0..3).contains(&index);
        assert_eq!(found, valid, "vector: wrong bounds decision");
        assert_eq!(
            output,
            if valid {
                values[index as usize]
            } else {
                sentinel
            },
            "vector: wrong output or write on miss"
        );
        same(&mut vector, &values);
        release_buffer(&vector);
        kani::cover!(index < 0, "vector cover: negative index");
        kani::cover!(index >= 3, "vector cover: past-end index");
        kani::cover!(valid, "vector cover: valid index");
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct Element {
    resource: *mut Resource,
    scalar: u64,
}
struct Resource {
    refs: usize,
}
static CLONES: AtomicUsize = AtomicUsize::new(0);
static DROPS: AtomicUsize = AtomicUsize::new(0);
static FREED: AtomicUsize = AtomicUsize::new(0);

unsafe extern "C" fn clone_element(src: *const c_void, dst: *mut c_void) -> i32 {
    let source = &*src.cast::<Element>();
    let target = &*dst.cast::<Element>();
    assert_eq!(
        source.resource, target.resource,
        "vector: clone destination not initialized"
    );
    assert_eq!(
        source.scalar, target.scalar,
        "vector: scalar field lost during clone"
    );
    assert!(
        (*source.resource).refs > 0,
        "vector: cloned released element"
    );
    (*source.resource).refs += 1;
    CLONES.fetch_add(1, Ordering::Relaxed);
    0
}
pub(crate) unsafe extern "C-unwind" fn drop_element(slot: *mut c_void) {
    let resource = (*slot.cast::<Element>()).resource;
    assert!((*resource).refs > 0, "vector: element released twice");
    (*resource).refs -= 1;
    DROPS.fetch_add(1, Ordering::Relaxed);
    if (*resource).refs == 0 {
        drop(Box::from_raw(resource));
        FREED.fetch_add(1, Ordering::Relaxed);
    }
}
fn element_layout() -> HewValueLayout {
    HewValueLayout {
        code_owner: ptr::null(),
        managed: ptr::null(),
        release_start: None,
        size: size_of::<Element>(),
        align: align_of::<Element>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: Some(clone_element),
        drop_fn: Some(drop_element),
    }
}
fn reset_counts() {
    CLONES.store(0, Ordering::Relaxed);
    DROPS.store(0, Ordering::Relaxed);
    FREED.store(0, Ordering::Relaxed);
}

#[kani::proof]
#[kani::unwind(7)]
#[kani::use_stub_set(normal_checks)]
fn vector_truncate_checks_entire_abi_domain_before_releasing() {
    let new_len: i64 = kani::any();
    let scalar: u64 = kani::any();
    reset_counts();
    unsafe {
        let input = Element {
            resource: Box::into_raw(Box::new(Resource { refs: 1 })),
            scalar,
        };
        let mut vector = header(element_layout());
        vector.layout = ptr::addr_of!(vector.layout_storage);
        hew_vec_push_owned_move(&mut vector, (&raw const input).cast());
        hew_vec_truncate(&mut vector, new_len);
        // For a one-element vector, only exactly zero removes its owner.
        let removed = usize::from(new_len == 0);
        assert_eq!(
            vector.len,
            1 - removed,
            "vector: truncate narrowed ABI length"
        );
        assert_eq!(
            DROPS.load(Ordering::Relaxed),
            removed,
            "vector: truncate dropped wrong owner count"
        );
        assert_eq!(
            FREED.load(Ordering::Relaxed),
            removed,
            "vector: truncate leaked removed owner"
        );
        assert_eq!(
            CLONES.load(Ordering::Relaxed),
            0,
            "vector: truncate cloned an owner"
        );
        if new_len != 0 {
            let stored = &*hew_vec_get_owned(&vector, 0).cast::<Element>();
            assert_eq!(
                stored.resource, input.resource,
                "vector: no-op truncate changed owner"
            );
            assert_eq!(
                stored.scalar, scalar,
                "vector: no-op truncate changed payload"
            );
            assert_eq!(
                (*input.resource).refs,
                1,
                "vector: no-op truncate changed reference count"
            );
        }
        hew_vec_clear(&mut vector);
        release_buffer(&vector);
        assert_eq!(
            FREED.load(Ordering::Relaxed),
            1,
            "vector: truncate leaked retained owner"
        );
        kani::cover!(new_len == 0, "vector cover: truncate removes owner");
        kani::cover!(
            new_len == -4_294_967_296,
            "vector cover: negative truncate wrap witness"
        );
        kani::cover!(
            new_len == 4_294_967_296,
            "vector cover: positive truncate wrap witness"
        );
    }
}

#[kani::proof]
#[kani::unwind(7)]
#[kani::use_stub_set(normal_checks)]
fn vector_failed_owned_reads_preserve_outputs_and_owners() {
    let index: i64 = kani::any();
    let operation: u8 = kani::any();
    let scalar: u64 = kani::any();
    let sentinel: u64 = kani::any();
    kani::assume(index < 0 || index >= 1);
    kani::assume(operation < 4);
    reset_counts();
    unsafe {
        let input = Element {
            resource: Box::into_raw(Box::new(Resource { refs: 1 })),
            scalar,
        };
        let mut vector = header(element_layout());
        vector.layout = ptr::addr_of!(vector.layout_storage);
        hew_vec_push_owned_move(&mut vector, (&raw const input).cast());
        let mut out = Element {
            resource: ptr::null_mut(),
            scalar: sentinel,
        };
        let mut status = -99;
        let found = match operation {
            0 => hew_vec_get_clone(&vector, index, (&raw mut out).cast()),
            1 => hew_vec_borrow_owned(&vector, index, (&raw mut out).cast()),
            2 => hew_vec_take_owned(&mut vector, index, (&raw mut out).cast()),
            _ => hew_vec_get_clone_managed(
                &vector,
                index,
                (&raw mut out).cast(),
                ptr::null(),
                &raw mut status,
            ),
        };
        assert!(!found, "vector: failed read accepted an invalid ABI index");
        assert!(
            out.resource.is_null(),
            "vector: failed read overwrote output owner"
        );
        assert_eq!(
            out.scalar, sentinel,
            "vector: failed read overwrote output payload"
        );
        assert_eq!(
            status,
            if operation == 3 { 0 } else { -99 },
            "vector: failed read status changed"
        );
        assert_eq!(vector.len, 1, "vector: failed read changed live range");
        let stored = &*hew_vec_get_owned(&vector, 0).cast::<Element>();
        assert_eq!(
            stored.resource, input.resource,
            "vector: failed read changed source owner"
        );
        assert_eq!(
            stored.scalar, scalar,
            "vector: failed read changed source payload"
        );
        assert_eq!(
            (*input.resource).refs,
            1,
            "vector: failed read acquired or released an owner"
        );
        assert_eq!(
            CLONES.load(Ordering::Relaxed),
            0,
            "vector: failed read cloned an owner"
        );
        assert_eq!(
            DROPS.load(Ordering::Relaxed),
            0,
            "vector: failed read dropped an owner"
        );
        hew_vec_clear(&mut vector);
        release_buffer(&vector);
        assert_eq!(
            FREED.load(Ordering::Relaxed),
            1,
            "vector: failed read leaked an owner"
        );
        kani::cover!(operation == 0, "vector cover: failed clone read");
        kani::cover!(operation == 1, "vector cover: failed borrow read");
        kani::cover!(operation == 2, "vector cover: failed take read");
        kani::cover!(operation == 3, "vector cover: failed managed read");
        kani::cover!(index < 0, "vector cover: failed negative owned read");
        kani::cover!(
            index == 4_294_967_296,
            "vector cover: failed oversized owned read"
        );
    }
}

#[kani::proof]
#[kani::unwind(7)]
#[kani::use_stub_set(normal_checks)]
fn vector_owned_copy_move_get_pop_preserve_ownership() {
    let scalar: u64 = kani::any();
    let moved: bool = kani::any();
    reset_counts();
    unsafe {
        let mut input = Element {
            resource: Box::into_raw(Box::new(Resource { refs: 1 })),
            scalar,
        };
        let mut vector = header(element_layout());
        vector.layout = ptr::addr_of!(vector.layout_storage);
        if moved {
            hew_vec_push_owned_move(&mut vector, (&raw const input).cast());
        } else {
            hew_vec_push_owned(&mut vector, (&raw const input).cast());
        }
        assert_eq!(
            (*input.resource).refs,
            if moved { 1 } else { 2 },
            "vector: wrong copy/move ownership"
        );
        assert_eq!(
            CLONES.load(Ordering::Relaxed),
            usize::from(!moved),
            "vector: wrong clone policy"
        );
        let mut result = std::mem::MaybeUninit::<Element>::uninit();
        assert!(hew_vec_get_clone(&vector, 0, result.as_mut_ptr().cast()));
        let mut result = result.assume_init();
        assert_eq!(result.scalar, scalar, "vector: clone lost scalar fields");
        assert_eq!(
            (*input.resource).refs,
            if moved { 2 } else { 3 },
            "vector: get did not produce independent owner"
        );
        let mut popped = std::mem::MaybeUninit::<Element>::uninit();
        assert_eq!(
            hew_vec_pop_owned(&mut vector, popped.as_mut_ptr().cast()),
            1
        );
        let mut popped = popped.assume_init();
        assert_eq!(popped.scalar, scalar, "vector: pop lost scalar fields");
        assert_eq!(vector.len, 0, "vector: pop kept moved slot live");
        assert_eq!(
            DROPS.load(Ordering::Relaxed),
            0,
            "vector: pop destroyed moved owner"
        );
        let mut untouched: [u64; 2] = kani::any();
        let before = untouched;
        assert_eq!(
            hew_vec_pop_owned(&mut vector, untouched.as_mut_ptr().cast()),
            0
        );
        assert_eq!(untouched[0], before[0], "vector: empty pop wrote output");
        assert_eq!(untouched[1], before[1], "vector: empty pop wrote output");
        release_buffer(&vector);
        drop_element((&raw mut popped).cast());
        drop_element((&raw mut result).cast());
        if !moved {
            drop_element((&raw mut input).cast());
        }
        assert_eq!(
            FREED.load(Ordering::Relaxed),
            1,
            "vector: missing or duplicate finalization"
        );
        assert_eq!(
            DROPS.load(Ordering::Relaxed),
            CLONES.load(Ordering::Relaxed) + 1,
            "vector: unbalanced ownership"
        );
        kani::cover!(moved, "vector cover: move input");
        kani::cover!(!moved, "vector cover: copied input");
    }
}

#[kani::proof]
#[kani::unwind(7)]
#[kani::use_stub_set(normal_checks)]
fn vector_owned_self_overwrite_then_clear_releases_once() {
    let scalar: u64 = kani::any();
    let replacement_scalar: u64 = kani::any();
    let moved: bool = kani::any();
    reset_counts();
    unsafe {
        let input = Element {
            resource: Box::into_raw(Box::new(Resource { refs: 1 })),
            scalar,
        };
        let mut vector = header(element_layout());
        vector.layout = ptr::addr_of!(vector.layout_storage);
        hew_vec_push_owned_move(&mut vector, (&raw const input).cast());
        let borrowed = hew_vec_get_owned(&vector, 0);
        hew_vec_set_owned(&mut vector, 0, borrowed);
        assert_eq!(
            (*input.resource).refs,
            1,
            "vector: self overwrite changed ownership count"
        );
        assert_eq!(
            CLONES.load(Ordering::Relaxed),
            1,
            "vector: self overwrite skipped retain"
        );
        assert_eq!(
            DROPS.load(Ordering::Relaxed),
            1,
            "vector: overwrite did not release old value"
        );
        assert_eq!(
            (*hew_vec_get_owned(&vector, 0).cast::<Element>()).scalar,
            scalar,
            "vector: self overwrite lost scalar"
        );
        let mut replacement = Element {
            resource: Box::into_raw(Box::new(Resource { refs: 1 })),
            scalar: replacement_scalar,
        };
        if moved {
            hew_vec_set_owned_move(&mut vector, 0, (&raw const replacement).cast());
        } else {
            hew_vec_set_owned(&mut vector, 0, (&raw const replacement).cast());
        }
        assert_eq!(
            FREED.load(Ordering::Relaxed),
            1,
            "vector: displaced resource was not finalized"
        );
        assert_eq!(
            (*replacement.resource).refs,
            if moved { 1 } else { 2 },
            "vector: replacement owner was released or leaked"
        );
        let stored = &*hew_vec_get_owned(&vector, 0).cast::<Element>();
        assert_eq!(
            stored.resource, replacement.resource,
            "vector: wrong replacement identity"
        );
        assert_eq!(
            stored.scalar, replacement_scalar,
            "vector: wrong replacement payload"
        );
        hew_vec_clear(&mut vector);
        assert_eq!(vector.len, 0, "vector: clear did not retire live range");
        assert_eq!(
            DROPS.load(Ordering::Relaxed),
            3,
            "vector: clear lost or duplicated a release"
        );
        if !moved {
            assert_eq!(
                (*replacement.resource).refs,
                1,
                "vector: clear damaged external owner"
            );
            drop_element((&raw mut replacement).cast());
        }
        assert_eq!(
            FREED.load(Ordering::Relaxed),
            2,
            "vector: final owner was not uniquely finalized"
        );
        assert_eq!(
            DROPS.load(Ordering::Relaxed),
            CLONES.load(Ordering::Relaxed) + 2,
            "vector: replacement ownership imbalance"
        );
        release_buffer(&vector);
        kani::cover!(moved, "vector cover: moved replacement");
        kani::cover!(!moved, "vector cover: copied replacement");
    }
}
