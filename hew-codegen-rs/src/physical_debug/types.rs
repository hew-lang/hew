//! Type lowering for `hew build -g`: how each storage type is described to a
//! debugger.
//!
//! Every Hew value reads without a helper script. A type is named as the
//! source spells it (`Box<i64>`, `Vec<string>`, `(i64, string, f64)`,
//! `fn(i64) -> i64`, `dyn Named`), so two instantiations of one generic are two
//! types. Shapes use only constructs every consumer renders the same way on
//! ELF, Mach-O and CodeView:
//!
//! - a scalar is a typedef with its Hew name over a C base type, so a debugger
//!   that renders base types by encoding still shows `i64`;
//! - an enum is a plain tagged struct: a `tag` of an enumeration type whose
//!   enumerators are the variant names, and a `payload` union with one struct
//!   per variant;
//! - a heap handle (`string`, `Vec<T>`, `HashMap<K, V>`, `HashSet<T>`, `[T; N]`,
//!   an indirect enum) is a typedef with its Hew name over a pointer to a
//!   struct that names the runtime header's members, so `*v` shows them.
//!
//! Collection headers are positioned by the runtime's own definitions and the
//! target's word size; a debugger script reads member names only.

use super::DebugEmitter;

use hew_mir::physical::{PhysicalDebugField, PhysicalDebugVariant, PhysicalTarget, SemVariantKind};
use hew_mir::{PhysicalLayout, PhysicalRepr};
use hew_types::{BuiltinType, ResolvedTy};
use inkwell::debug_info::{AsDIScope, DIFlags, DIFlagsConstants, DIType};
use inkwell::AddressSpace;

const DW_ATE_BOOLEAN: u32 = 0x02;
const DW_ATE_FLOAT: u32 = 0x04;
const DW_ATE_SIGNED: u32 = 0x05;
const DW_ATE_UNSIGNED: u32 = 0x07;
const DW_ATE_UNSIGNED_CHAR: u32 = 0x08;
const DW_ATE_UTF: u32 = 0x10;

/// Word positions of the `HewVec` members a debugger reads.
///
/// `hew_cabi::vec::HewVec` is `repr(C)` with pointer-sized leading members
/// `{data, len, front, cap}`; the target's word size places them.
const VEC_PTR_WORD: u64 = 0;
const VEC_LEN_WORD: u64 = 1;
const VEC_CAP_WORD: u64 = 3;
/// Words of the `HewVec` prefix the header struct describes.
const VEC_HEADER_WORDS: u64 = 4;

/// Word positions of the `HewLayoutHashMap` members a debugger reads.
///
/// `hew_runtime::hashmap::HewLayoutHashMap` is `repr(C)` with pointer-sized
/// leading members `{entries, len, cap, key_offset, val_offset, stride}`.
const MAP_ENTRIES_WORD: u64 = 0;
const MAP_LEN_WORD: u64 = 1;
const MAP_CAP_WORD: u64 = 2;
const MAP_STRIDE_WORD: u64 = 5;
/// Words of the `HewLayoutHashMap` prefix the header struct describes.
const MAP_HEADER_WORDS: u64 = 6;

/// A `HewLayoutHashSet` is one word: the map holding its elements as keys.
const SET_MAP_WORD: u64 = 0;

/// What a debugger needs to know about one scalar Hew type.
struct Scalar {
    /// The Hew spelling, given to the typedef.
    name: &'static str,
    /// The C base type the typedef names, so expression evaluators work.
    base: &'static str,
    encoding: u32,
}

impl<'ctx> DebugEmitter<'ctx> {
    /// The DIE for one storage type, memoized per type.
    ///
    /// A nominal type is entered as a declaration before its members are
    /// described, so a reference back to it through a pointer (a record
    /// holding `Vec<Self>`, an indirect enum's node) resolves to that
    /// declaration instead of recursing. A debugger completes the declaration
    /// from the definition of the same name.
    pub(super) fn resolve_type(
        &self,
        ty: &ResolvedTy,
        layout: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        if let Some(cached) = self.types.borrow().get(ty) {
            return Some(*cached);
        }
        let name = type_name(ty);
        if let Some(placeholder) = self.placeholder(ty, &name, layout, target) {
            self.types.borrow_mut().insert(ty.clone(), placeholder);
        }
        let described = self.build_type(ty, &name, layout, target);
        let mut types = self.types.borrow_mut();
        match described {
            Some(described) => {
                types.insert(ty.clone(), described);
            }
            None => {
                types.remove(ty);
            }
        }
        described
    }

    /// The declaration a nominal type stands for while its members are being
    /// described.
    fn placeholder(
        &self,
        ty: &ResolvedTy,
        name: &str,
        layout: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let indirect = self.enums.contains_key(ty)
            && target
                .variant_layout(ty)
                .is_some_and(|cases| cases.is_indirect);
        if indirect {
            let node = self.declaration(&node_name(name));
            return Some(self.handle(name, node, layout));
        }
        matches!(layout.repr, PhysicalRepr::Struct(_)).then(|| self.declaration(name))
    }

    fn build_type(
        &self,
        ty: &ResolvedTy,
        name: &str,
        layout: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let bits = layout.size.checked_mul(8)?;
        if let Some(scalar) = scalar(ty, bits) {
            let base = self.basic(scalar.base, bits, scalar.encoding);
            return Some(self.typedef(scalar.name, base, layout.align));
        }
        if let Some(variants) = self.enums.get(ty) {
            return self.build_enum(ty, name, layout, variants, target);
        }
        match &layout.repr {
            PhysicalRepr::Unit => None,
            // An actor handle is the actor's id; a typedef keeps its name.
            PhysicalRepr::Integer { .. } | PhysicalRepr::Float { .. } => {
                let base = self.layout_type(layout)?;
                Some(self.typedef(name, base, layout.align))
            }
            PhysicalRepr::Pointer => self.build_handle(ty, name, layout, target),
            PhysicalRepr::Array { .. } | PhysicalRepr::Vector { .. } => {
                let array = self.layout_type(layout)?;
                Some(self.typedef(name, array, layout.align))
            }
            PhysicalRepr::Struct(fields) => self.build_struct(ty, name, layout, fields, target),
        }
    }

    /// A value held through a pointer: name the heap object it addresses.
    fn build_handle(
        &self,
        ty: &ResolvedTy,
        name: &str,
        layout: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        if *ty == ResolvedTy::String {
            let header = self.build_string(layout)?;
            return Some(self.handle(name, header, layout));
        }
        let header = match ty {
            ResolvedTy::Array(element, _) => self.vec_header(name, element, layout, target)?,
            ResolvedTy::Named {
                head: hew_types::TypeHead::Builtin(builtin),
                args,
                ..
            } => match (builtin, args.as_slice()) {
                (BuiltinType::Vec, [element]) => self.vec_header(name, element, layout, target)?,
                (BuiltinType::HashMap, [key, value]) => {
                    self.map_header(&header_name(name), name, key, Some(value), layout, target)?
                }
                (BuiltinType::HashSet, [element]) => {
                    let map = self.map_header(
                        &format!("{name}.Map"),
                        name,
                        element,
                        None,
                        layout,
                        target,
                    )?;
                    let word = layout.size;
                    let map = self.pointer(map, layout);
                    let members = [self.member("map", map, layout, SET_MAP_WORD * word)];
                    self.structure(&header_name(name), word, layout.align, &members)
                }
                _ => self.opaque_byte(),
            },
            // Every other handle is opaque: its contents are the runtime's.
            _ => self.opaque_byte(),
        };
        Some(self.handle(name, header, layout))
    }

    /// `HewVec` as `{ptr: *T, len, cap}`, positioned by the target word.
    fn vec_header(
        &self,
        name: &str,
        element: &ResolvedTy,
        word: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let element_layout = target.layout(element)?;
        let element = self.resolve_type(element, element_layout, target)?;
        let size = self.word_type(word);
        let bytes = word.size;
        let members = [
            self.member(
                "ptr",
                self.pointer(element, word),
                word,
                VEC_PTR_WORD * bytes,
            ),
            self.member("len", size, word, VEC_LEN_WORD * bytes),
            self.member("cap", size, word, VEC_CAP_WORD * bytes),
        ];
        Some(self.structure(
            &header_name(name),
            VEC_HEADER_WORDS * bytes,
            word.align,
            &members,
        ))
    }

    /// `HewLayoutHashMap` as `{entries: *Entry, len, cap, stride}`, where each
    /// `Entry` is `{state, key, value}` at the runtime's own slot offsets. A
    /// set's slots carry no value.
    fn map_header(
        &self,
        header: &str,
        name: &str,
        key: &ResolvedTy,
        value: Option<&ResolvedTy>,
        word: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let key_layout = target.layout(key)?;
        let key_type = self.resolve_type(key, key_layout, target)?;
        let value = match value {
            Some(value) => {
                let layout = target.layout(value)?;
                Some((layout, self.resolve_type(value, layout, target)?))
            }
            None => None,
        };
        let (value_size, value_align) =
            value.map_or((0, 1), |(layout, _)| (layout.size, layout.align));
        let (key_offset, value_offset, stride, entries_align) =
            hew_runtime::hashmap::compute_slot_layout(
                convert(key_layout.size)?,
                convert(key_layout.align)?,
                convert(value_size)?,
                convert(value_align)?,
            )?;
        let (key_offset, value_offset, stride) = (
            convert::<_, u64>(key_offset)?,
            convert::<_, u64>(value_offset)?,
            convert::<_, u64>(stride)?,
        );
        let entries_align = convert::<_, u32>(entries_align)?;

        let state = self.slot_state();
        let mut slot = vec![self.member_bits("state", state, 8, 8, 0)];
        slot.push(self.member("key", key_type, key_layout, key_offset));
        if let Some((layout, value_type)) = value {
            slot.push(self.member("value", value_type, layout, value_offset));
        }
        let entry = self.structure(&format!("{name}.Entry"), stride, entries_align, &slot);

        let size = self.word_type(word);
        let bytes = word.size;
        let members = [
            self.member(
                "entries",
                self.pointer(entry, word),
                word,
                MAP_ENTRIES_WORD * bytes,
            ),
            self.member("len", size, word, MAP_LEN_WORD * bytes),
            self.member("cap", size, word, MAP_CAP_WORD * bytes),
            self.member("stride", size, word, MAP_STRIDE_WORD * bytes),
        ];
        Some(self.structure(header, MAP_HEADER_WORDS * bytes, word.align, &members))
    }

    /// The runtime's slot states, so a script selects `OCCUPIED` by name.
    fn slot_state(&self) -> DIType<'ctx> {
        use hew_runtime::hashmap::{EMPTY, OCCUPIED, TOMBSTONE};
        let enumerators = [
            ("EMPTY", EMPTY),
            ("OCCUPIED", OCCUPIED),
            ("TOMBSTONE", TOMBSTONE),
        ]
        .map(|(name, value)| self.builder.create_enumerator(name, i64::from(value), true));
        self.builder
            .create_enumeration_type(
                self.file.as_debug_info_scope(),
                "HashSlotState",
                self.file,
                0,
                8,
                8,
                &enumerators,
                self.basic("uint8_t", 8, DW_ATE_UNSIGNED),
            )
            .as_type()
    }

    /// Describe the canonical managed allocation without turning its handle
    /// into a C string. Debugger summaries read the length and trailing bytes.
    fn build_string(&self, layout: &PhysicalLayout) -> Option<DIType<'ctx>> {
        let header_size = hew_cabi::string::string_debug_header_size(layout.size)?;
        let length = self.word_type(layout);
        let byte = self.basic("uint8_t", 8, DW_ATE_UNSIGNED_CHAR);
        let data = self
            .builder
            .create_array_type(byte, 0, 8, &[std::ops::Range { start: 0, end: 0 }])
            .as_type();
        let members = [
            self.member(
                "byte_len",
                length,
                layout,
                hew_cabi::string::STRING_DEBUG_BYTE_LEN_OFFSET as u64,
            ),
            self.member_bits("data", data, 0, 8, header_size * 8),
        ];
        Some(self.structure("hew_string", header_size, layout.align, &members))
    }

    fn build_struct(
        &self,
        ty: &ResolvedTy,
        name: &str,
        layout: &PhysicalLayout,
        fields: &[PhysicalLayout],
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let offsets = field_offsets(fields);
        let mut members = Vec::with_capacity(fields.len());
        match ty.callable_body() {
            ResolvedTy::Closure { .. } | ResolvedTy::Function { .. } => {
                let [environment, descriptor] = fields else {
                    return None;
                };
                let environment = self.build_environment(ty, name, environment, target)?;
                members.push(self.member("env", environment, &fields[0], offsets[0]));
                members.push(self.member(
                    "descriptor",
                    self.pointer(self.opaque_byte(), descriptor),
                    descriptor,
                    offsets[1],
                ));
            }
            ResolvedTy::TraitObject { .. } => {
                for (index, role) in ["data", "vtable"].into_iter().enumerate() {
                    let field = fields.get(index)?;
                    let pointer = self.pointer(self.opaque_byte(), field);
                    members.push(self.member(role, pointer, field, offsets[index]));
                }
            }
            ResolvedTy::Tuple(elements) if elements.len() == fields.len() => {
                for (index, (element, field)) in elements.iter().zip(fields).enumerate() {
                    let element = self.resolve_type(element, field, target)?;
                    members.push(self.member(&position(index), element, field, offsets[index]));
                }
            }
            _ => match self
                .records
                .get(ty)
                .filter(|rows| rows.len() == fields.len())
            {
                Some(rows) => {
                    for (index, (row, field)) in rows.iter().zip(fields).enumerate() {
                        let member = self.resolve_type(&row.ty, field, target)?;
                        members.push(self.member(&row.name, member, field, offsets[index]));
                    }
                }
                // A runtime carrier (`bytes`, a node id, a location) has no
                // source fields; its members are its words, by position.
                None => {
                    let names: Vec<String> = if *ty == ResolvedTy::Bytes {
                        ["ptr", "offset", "len"].map(str::to_owned).to_vec()
                    } else {
                        (0..fields.len()).map(position).collect()
                    };
                    for (index, field) in fields.iter().enumerate() {
                        let member = self.layout_type(field)?;
                        members.push(self.member(names.get(index)?, member, field, offsets[index]));
                    }
                }
            },
        }
        Some(self.structure(name, layout.size, layout.align, &members))
    }

    /// A callable's environment: the initialization mask and each capture.
    fn build_environment(
        &self,
        ty: &ResolvedTy,
        name: &str,
        word: &PhysicalLayout,
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let captures = match ty.callable_body() {
            ResolvedTy::Closure { captures, .. } => captures.as_slice(),
            _ => &[],
        };
        let environment = target.environment_layout(ty)?;
        let PhysicalRepr::Struct(storage) = &environment.repr else {
            return Some(self.pointer(self.opaque_byte(), word));
        };
        let [mask, fields @ ..] = storage.as_slice() else {
            return None;
        };
        if fields.len() != captures.len() {
            return None;
        }
        let offsets = field_offsets(storage);
        let mut members = vec![self.member("init", self.layout_type(mask)?, mask, offsets[0])];
        for (index, (capture, field)) in captures.iter().zip(fields).enumerate() {
            let capture = self.resolve_type(capture, field, target)?;
            members.push(self.member(&position(index), capture, field, offsets[index + 1]));
        }
        let environment = self.structure(
            &format!("{name}.env"),
            environment.size,
            environment.align,
            &members,
        );
        Some(self.pointer(environment, word))
    }

    /// An enum as a plain tagged struct: `{tag, payload}`, where `tag` names
    /// the active variant and `payload` is a union with one struct per
    /// variant. An indirect enum is a handle to that struct.
    fn build_enum(
        &self,
        ty: &ResolvedTy,
        name: &str,
        layout: &PhysicalLayout,
        variants: &[PhysicalDebugVariant],
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let cases = target.variant_layout(ty)?;
        if cases.variants.len() != variants.len() {
            return None;
        }
        if cases.is_indirect {
            let node = self.tagged(
                &node_name(name),
                &cases.object,
                variants,
                &cases.variants,
                target,
            )?;
            return Some(self.handle(name, node, layout));
        }
        self.tagged(name, layout, variants, &cases.variants, target)
    }

    fn tagged(
        &self,
        name: &str,
        layout: &PhysicalLayout,
        variants: &[PhysicalDebugVariant],
        cases: &[PhysicalLayout],
        target: &PhysicalTarget,
    ) -> Option<DIType<'ctx>> {
        let PhysicalRepr::Struct(carrier) = &layout.repr else {
            return None;
        };
        let [tag, payload] = carrier.as_slice() else {
            return None;
        };
        let tag_bits = tag.size.checked_mul(8)?;
        let payload_align = u64::from(payload.align).max(1);
        let payload_offset = tag.size.div_ceil(payload_align) * payload_align;

        let enumerators: Vec<_> = variants
            .iter()
            .enumerate()
            .map(|(index, variant)| {
                self.builder.create_enumerator(
                    &variant.name,
                    i64::try_from(index).unwrap_or(i64::MAX),
                    true,
                )
            })
            .collect();
        let tag_type = self
            .builder
            .create_enumeration_type(
                self.file.as_debug_info_scope(),
                &format!("{name}.Tag"),
                self.file,
                0,
                tag_bits,
                tag.align * 8,
                &enumerators,
                self.basic(unsigned_base(tag_bits)?, tag_bits, DW_ATE_UNSIGNED),
            )
            .as_type();

        let mut arms = Vec::with_capacity(variants.len());
        for (variant, case) in variants.iter().zip(cases) {
            let PhysicalRepr::Struct(field_layouts) = &case.repr else {
                return None;
            };
            if field_layouts.len() != variant.fields.len() {
                return None;
            }
            let offsets = field_offsets(field_layouts);
            let mut fields = Vec::with_capacity(field_layouts.len());
            for (index, (field, field_layout)) in
                variant.fields.iter().zip(field_layouts).enumerate()
            {
                let member = self.resolve_type(&field.ty, field_layout, target)?;
                fields.push(self.member(
                    &variant_field_name(variant.kind, index, field),
                    member,
                    field_layout,
                    offsets[index],
                ));
            }
            let arm = self.structure(
                &format!("{name}.{}", variant.name),
                case.size,
                case.align,
                &fields,
            );
            arms.push(self.member(&variant.name, arm, case, 0));
        }
        let union = self
            .builder
            .create_union_type(
                self.file.as_debug_info_scope(),
                &format!("{name}.Payload"),
                self.file,
                0,
                payload.size.checked_mul(8)?,
                payload.align * 8,
                DIFlags::ZERO,
                &arms,
                0,
                &self.unique(&format!("{name}.Payload")),
            )
            .as_type();
        let members = [
            self.member("tag", tag_type, tag, 0),
            self.member("payload", union, payload, payload_offset),
        ];
        Some(self.structure(name, layout.size, layout.align, &members))
    }

    /// Describe storage by its physical shape alone, for runtime carrier words
    /// that have no source type.
    fn layout_type(&self, layout: &PhysicalLayout) -> Option<DIType<'ctx>> {
        let bits = layout.size.checked_mul(8)?;
        match &layout.repr {
            PhysicalRepr::Unit | PhysicalRepr::Struct(_) => None,
            PhysicalRepr::Integer { bits: width } => {
                let width = u64::from(*width);
                Some(self.basic(unsigned_base(width)?, width, DW_ATE_UNSIGNED))
            }
            PhysicalRepr::Float { bits: width } => {
                let base = match width {
                    32 => "float",
                    64 => "double",
                    _ => return None,
                };
                Some(self.basic(base, u64::from(*width), DW_ATE_FLOAT))
            }
            PhysicalRepr::Pointer => Some(self.pointer(self.opaque_byte(), layout)),
            PhysicalRepr::Array { element, len } | PhysicalRepr::Vector { element, len } => {
                let inner = self.layout_type(element)?;
                Some(
                    self.builder
                        .create_array_type(
                            inner,
                            bits,
                            layout.align * 8,
                            &[std::ops::Range {
                                start: 0,
                                end: i64::from(*len),
                            }],
                        )
                        .as_type(),
                )
            }
        }
    }

    fn basic(&self, name: &str, bits: u64, encoding: u32) -> DIType<'ctx> {
        self.builder
            .create_basic_type(name, bits, encoding, DIFlags::ZERO)
            .unwrap_or_else(|_| unreachable!("a basic type with a name is never rejected"))
            .as_type()
    }

    /// A pointer-sized unsigned count.
    fn word_type(&self, word: &PhysicalLayout) -> DIType<'ctx> {
        let bits = word.size * 8;
        self.typedef(
            "usize",
            self.basic(
                unsigned_base(bits).unwrap_or("uint64_t"),
                bits,
                DW_ATE_UNSIGNED,
            ),
            word.align,
        )
    }

    fn opaque_byte(&self) -> DIType<'ctx> {
        self.basic("uint8_t", 8, DW_ATE_UNSIGNED_CHAR)
    }

    fn typedef(&self, name: &str, ty: DIType<'ctx>, align: u32) -> DIType<'ctx> {
        self.builder
            .create_typedef(
                ty,
                name,
                self.file,
                0,
                self.file.as_debug_info_scope(),
                align * 8,
            )
            .as_type()
    }

    fn pointer(&self, pointee: DIType<'ctx>, word: &PhysicalLayout) -> DIType<'ctx> {
        self.builder
            .create_pointer_type(
                "",
                pointee,
                word.size * 8,
                word.align * 8,
                AddressSpace::default(),
            )
            .as_type()
    }

    /// A handle: the Hew name over a pointer to the object it addresses.
    fn handle(&self, name: &str, object: DIType<'ctx>, word: &PhysicalLayout) -> DIType<'ctx> {
        self.typedef(name, self.pointer(object, word), word.align)
    }

    fn member(
        &self,
        name: &str,
        ty: DIType<'ctx>,
        layout: &PhysicalLayout,
        offset: u64,
    ) -> DIType<'ctx> {
        self.member_bits(name, ty, layout.size * 8, layout.align * 8, offset * 8)
    }

    fn member_bits(
        &self,
        name: &str,
        ty: DIType<'ctx>,
        size_bits: u64,
        align_bits: u32,
        offset_bits: u64,
    ) -> DIType<'ctx> {
        self.builder
            .create_member_type(
                self.file.as_debug_info_scope(),
                name,
                self.file,
                0,
                size_bits,
                align_bits,
                offset_bits,
                DIFlags::ZERO,
                ty,
            )
            .as_type()
    }

    fn structure(
        &self,
        name: &str,
        size: u64,
        align: u32,
        members: &[DIType<'ctx>],
    ) -> DIType<'ctx> {
        self.builder
            .create_struct_type(
                self.file.as_debug_info_scope(),
                name,
                self.file,
                0,
                size * 8,
                align * 8,
                DIFlags::ZERO,
                None,
                members,
                0,
                None,
                &self.unique(name),
            )
            .as_type()
    }

    /// A member-less declaration a debugger completes by name.
    fn declaration(&self, name: &str) -> DIType<'ctx> {
        self.builder
            .create_struct_type(
                self.file.as_debug_info_scope(),
                name,
                self.file,
                0,
                0,
                0,
                DIFlags::FWD_DECL,
                None,
                &[],
                0,
                None,
                name,
            )
            .as_type()
    }

    /// A composite's unique identifier. Names already separate
    /// instantiations; the module ordinal keeps two distinct composites from
    /// ever sharing one, which CodeView consumers merge by.
    fn unique(&self, name: &str) -> String {
        let ordinal = self.composites.get();
        self.composites.set(ordinal + 1);
        format!("{name}#{ordinal}")
    }
}

/// The scalar a debugger sees for `ty`, by its Hew name.
fn scalar(ty: &ResolvedTy, bits: u64) -> Option<Scalar> {
    let (name, signed) = match ty {
        ResolvedTy::I8 => ("i8", true),
        ResolvedTy::I16 => ("i16", true),
        ResolvedTy::I32 => ("i32", true),
        ResolvedTy::I64 => ("i64", true),
        ResolvedTy::Isize => ("isize", true),
        ResolvedTy::Duration => ("duration", true),
        ResolvedTy::U8 => ("u8", false),
        ResolvedTy::U16 => ("u16", false),
        ResolvedTy::U32 => ("u32", false),
        ResolvedTy::U64 => ("u64", false),
        ResolvedTy::Usize => ("usize", false),
        ResolvedTy::F32 => return Some(scalar_of("f32", "float", DW_ATE_FLOAT)),
        ResolvedTy::F64 => return Some(scalar_of("f64", "double", DW_ATE_FLOAT)),
        ResolvedTy::Bool => return Some(scalar_of("bool", "_Bool", DW_ATE_BOOLEAN)),
        ResolvedTy::Char => return Some(scalar_of("char", "char32_t", DW_ATE_UTF)),
        _ => return None,
    };
    Some(if signed {
        scalar_of(name, signed_base(bits)?, DW_ATE_SIGNED)
    } else {
        scalar_of(name, unsigned_base(bits)?, DW_ATE_UNSIGNED)
    })
}

const fn scalar_of(name: &'static str, base: &'static str, encoding: u32) -> Scalar {
    Scalar {
        name,
        base,
        encoding,
    }
}

fn signed_base(bits: u64) -> Option<&'static str> {
    Some(match bits {
        8 => "int8_t",
        16 => "int16_t",
        32 => "int32_t",
        64 => "int64_t",
        _ => return None,
    })
}

fn unsigned_base(bits: u64) -> Option<&'static str> {
    Some(match bits {
        8 => "uint8_t",
        16 => "uint16_t",
        32 => "uint32_t",
        64 => "uint64_t",
        _ => return None,
    })
}

/// The source spelling of a type, from the type checker's own renderer.
fn type_name(ty: &ResolvedTy) -> String {
    ty.user_facing().to_string()
}

fn header_name(name: &str) -> String {
    format!("{name}.Header")
}

fn node_name(name: &str) -> String {
    format!("{name}.Node")
}

/// A positional member: `_0`, `_1`, a valid identifier in every debugger's
/// expression language.
fn position(index: usize) -> String {
    format!("_{index}")
}

fn variant_field_name(kind: SemVariantKind, index: usize, field: &PhysicalDebugField) -> String {
    match kind {
        SemVariantKind::Struct => field.name.clone(),
        SemVariantKind::Tuple | SemVariantKind::Unit => position(index),
    }
}

/// Byte offsets of `fields` laid out in order at their natural alignment, as
/// LLVM lays out the matching struct.
fn field_offsets(fields: &[PhysicalLayout]) -> Vec<u64> {
    let mut offset = 0u64;
    fields
        .iter()
        .map(|field| {
            let align = u64::from(field.align).max(1);
            offset = offset.div_ceil(align) * align;
            let at = offset;
            offset += field.size;
            at
        })
        .collect()
}

/// A layout number in another width; `None` when it does not fit, which leaves
/// the type undescribed instead of described wrongly.
fn convert<From, To: TryFrom<From>>(value: From) -> Option<To> {
    To::try_from(value).ok()
}
