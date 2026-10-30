//! Debug types a `-g` build describes, read back from the emitted object.
//!
//! The object is read with `gimli`, independent of LLVM, so these tests see
//! what a debugger sees: type names, shapes, member offsets and encodings.
//! CodeView targets carry no DWARF; `physical_debug_tests` covers them.
#![cfg(not(target_env = "msvc"))]

use std::borrow::Cow;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use gimli::{constants, AttributeValue, DwAte, DwTag, EndianSlice, RunTimeEndian};
use hew_codegen_rs::physical::physical_target_for_inventory;
use hew_codegen_rs::{
    emit_physical_object, native_emission_triple, DebugSource, OptLevel, PhysicalEmitOptions,
};
use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};
use object::{Object, ObjectSection, ObjectSymbol, RelocationTarget};

fn verified(source: &str) -> hew_mir::VerifiedPhysicalModule {
    let mut parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    hew_compile::attach_prelude_std_modules(&mut parsed.program);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR errors: {:#?}",
        hir.diagnostics
    );
    let semantic = hew_sir::lower_module(&hir.module, &facts).module;
    let triple = native_emission_triple();
    let inventory = hew_mir::physical::physical_type_inventory(&semantic);
    let target = physical_target_for_inventory(&triple, &inventory).expect("physical target");
    hew_mir::lower_physical_module(&semantic, target).expect("verified physical module")
}

/// Emit `text` with debug metadata and return the native object.
fn emit(dir: &Path, name: &str, text: &str) -> PathBuf {
    emit_with_ir(dir, name, text).0
}

/// Emit `text` with debug metadata; return the native object and textual IR.
fn emit_with_ir(dir: &Path, name: &str, text: &str) -> (PathBuf, PathBuf) {
    let source = dir.join(format!("{name}.hew"));
    std::fs::write(&source, text).expect("write fixture source");
    let artefacts = emit_physical_object(
        &verified(text),
        &PhysicalEmitOptions {
            module_name: name,
            out_dir: dir,
            target_triple: None,
            opt_level: OptLevel::O0,
            emit_llvm: true,
            address_sanitizer: false,
            debug_source: Some(DebugSource {
                path: &source,
                text,
            }),
            link_freestanding_wasm: false,
            build_info: None,
        },
    )
    .expect("emit physical object");
    (
        artefacts.native_obj_path.expect("native object"),
        artefacts.ll_path.expect("textual IR"),
    )
}

/// One DIE, with the attributes these tests read.
#[derive(Debug)]
struct Die {
    tag: DwTag,
    name: Option<String>,
    /// Unit offset of `DW_AT_type`.
    ty: Option<usize>,
    byte_size: Option<u64>,
    member_offset: Option<u64>,
    encoding: Option<DwAte>,
    value: Option<i64>,
    declaration: bool,
    children: Vec<usize>,
}

/// Every DIE of the object's Hew compile unit, indexed by unit offset.
struct Dies {
    dies: Vec<Die>,
    at: HashMap<usize, usize>,
}

/// Section bytes with their relocations applied: an object's DWARF refers to
/// strings and other sections through relocations a linker would resolve.
fn relocated<'data>(file: &object::File<'data>, name: &str) -> Cow<'data, [u8]> {
    let Some(section) = file.section_by_name(name) else {
        return Cow::Borrowed(&[]);
    };
    let data = section.uncompressed_data().expect("section data");
    let mut relocations = section.relocations().peekable();
    if relocations.peek().is_none() {
        return data;
    }
    let mut bytes = data.into_owned();
    for (offset, relocation) in relocations {
        let base = match relocation.target() {
            RelocationTarget::Symbol(index) => file
                .symbol_by_index(index)
                .expect("relocation symbol")
                .address(),
            RelocationTarget::Section(index) => file
                .section_by_index(index)
                .expect("relocation section")
                .address(),
            _ => 0,
        };
        let offset = usize::try_from(offset).expect("relocation offset");
        let width = usize::from(relocation.size() / 8);
        let slot = &mut bytes[offset..offset + width];
        let mut existing = [0u8; 8];
        existing[..width].copy_from_slice(slot);
        let implicit = if relocation.has_implicit_addend() {
            u64::from_le_bytes(existing)
        } else {
            0
        };
        let value = base
            .wrapping_add_signed(relocation.addend())
            .wrapping_add(implicit);
        slot.copy_from_slice(&value.to_le_bytes()[..width]);
    }
    Cow::Owned(bytes)
}

fn read_dies(object: &Path) -> Dies {
    let bytes = std::fs::read(object).expect("read object");
    let file = object::File::parse(&*bytes).expect("parse object");
    let endian = if file.is_little_endian() {
        RunTimeEndian::Little
    } else {
        RunTimeEndian::Big
    };
    // Mach-O spells DWARF sections `__debug_*`.
    let sections = gimli::DwarfSections::load(|id| -> Result<_, gimli::Error> {
        let elf = relocated(&file, id.name());
        if !elf.is_empty() {
            return Ok(elf);
        }
        Ok(relocated(&file, &id.name().replacen('.', "__", 1)))
    })
    .expect("load DWARF");
    let dwarf = sections.borrow(|section| EndianSlice::new(section, endian));

    let mut units = dwarf.units();
    let header = units.next().expect("read unit").expect("a compile unit");
    let unit = dwarf.unit(header).expect("parse unit");
    let mut dies: Vec<Die> = Vec::new();
    let mut at = HashMap::new();
    let mut stack: Vec<usize> = Vec::new();
    let mut entries = unit.entries();
    while let Some(entry) = entries.next_dfs().expect("walk DIEs") {
        let depth = usize::try_from(entry.depth()).expect("DIE depth");
        stack.truncate(depth);
        let mut die = Die {
            tag: entry.tag(),
            name: None,
            ty: None,
            byte_size: None,
            member_offset: None,
            encoding: None,
            value: None,
            declaration: false,
            children: Vec::new(),
        };
        for attribute in entry.attrs() {
            match (attribute.name(), attribute.value()) {
                (constants::DW_AT_name, value) => {
                    let name = dwarf.attr_string(&unit, value).expect("name string");
                    die.name = Some(name.to_string_lossy().into_owned());
                }
                (constants::DW_AT_type, AttributeValue::UnitRef(offset)) => {
                    die.ty = Some(offset.0);
                }
                (constants::DW_AT_byte_size, value) => die.byte_size = value.udata_value(),
                (constants::DW_AT_data_member_location, value) => {
                    die.member_offset = value.udata_value();
                }
                (constants::DW_AT_encoding, AttributeValue::Encoding(encoding)) => {
                    die.encoding = Some(encoding);
                }
                (constants::DW_AT_const_value, value) => {
                    die.value = value
                        .sdata_value()
                        .or_else(|| value.udata_value().and_then(|raw| i64::try_from(raw).ok()));
                }
                (constants::DW_AT_declaration, _) => die.declaration = true,
                _ => {}
            }
        }
        let index = dies.len();
        at.insert(entry.offset().0, index);
        if let Some(parent) = stack.last() {
            dies[*parent].children.push(index);
        }
        dies.push(die);
        stack.push(index);
    }
    Dies { dies, at }
}

impl Dies {
    fn get(&self, index: usize) -> &Die {
        &self.dies[index]
    }

    /// The type a DIE refers to.
    fn type_of(&self, index: usize) -> usize {
        let offset = self
            .get(index)
            .ty
            .unwrap_or_else(|| panic!("{:?} has a type", self.get(index).name));
        self.at[&offset]
    }

    /// Follow typedefs and pointers down to the described type.
    fn target(&self, mut index: usize) -> usize {
        while matches!(
            self.get(index).tag,
            constants::DW_TAG_typedef | constants::DW_TAG_pointer_type
        ) {
            index = self.type_of(index);
        }
        index
    }

    /// The type of the source local `name`.
    fn local(&self, name: &str) -> usize {
        let variable = self
            .dies
            .iter()
            .position(|die| {
                matches!(
                    die.tag,
                    constants::DW_TAG_variable | constants::DW_TAG_formal_parameter
                ) && die.name.as_deref() == Some(name)
            })
            .unwrap_or_else(|| panic!("local `{name}` is described"));
        self.type_of(variable)
    }

    /// The defining (not declaration) DIE with this tag and name.
    fn definition(&self, tag: DwTag, name: &str) -> usize {
        self.dies
            .iter()
            .position(|die| die.tag == tag && die.name.as_deref() == Some(name) && !die.declaration)
            .unwrap_or_else(|| panic!("{tag} `{name}` is defined"))
    }

    fn name(&self, index: usize) -> &str {
        self.get(index).name.as_deref().unwrap_or("")
    }

    /// `(name, offset, type)` of each member of a composite.
    fn members(&self, index: usize) -> Vec<(String, u64, usize)> {
        self.get(index)
            .children
            .iter()
            .filter(|child| self.get(**child).tag == constants::DW_TAG_member)
            .map(|child| {
                let member = self.get(*child);
                (
                    member.name.clone().unwrap_or_default(),
                    member.member_offset.unwrap_or(0),
                    self.type_of(*child),
                )
            })
            .collect()
    }

    fn member(&self, index: usize, name: &str) -> (u64, usize) {
        self.members(index)
            .into_iter()
            .find(|(member, _, _)| member == name)
            .map(|(_, offset, ty)| (offset, ty))
            .unwrap_or_else(|| panic!("`{}` has a member `{name}`", self.name(index)))
    }

    fn member_names(&self, index: usize) -> Vec<String> {
        self.members(index)
            .into_iter()
            .map(|(name, _, _)| name)
            .collect()
    }

    /// `(name, value)` of each enumerator of an enumeration type.
    fn enumerators(&self, index: usize) -> Vec<(String, i64)> {
        self.get(index)
            .children
            .iter()
            .map(|child| self.get(*child))
            .filter(|die| die.tag == constants::DW_TAG_enumerator)
            .map(|die| {
                (
                    die.name.clone().unwrap_or_default(),
                    die.value.unwrap_or(-1),
                )
            })
            .collect()
    }
}

/// Every value shape a debugger should read without a script.
const TYPES: &str = "\
enum Shape {
    Circle(i64);
    Rect(i64, i64);
    Empty;
}

enum Event {
    Moved { x: i64; y: i64; }
    Quit;
}

type Rect {
    width: i64;
    height: i64;
}

type Box<T> {
    value: T;
}

trait Named {
    fn name(self) -> string;
}

type Dog {
    tag: string;
}

impl Named for Dog {
    fn name(self) -> string {
        f\"dog {self.tag}\"
    }
}

actor Counter {
    var count: i64;

    receive fn bump() -> i64 {
        count = count + 1;
        count
    }
}

fn parse(n: i64) -> Result<i64, string> {
    if n > 0 { .Ok(n) } else { .Err(\"negative\") }
}

fn pick() -> dyn Named {
    Dog { tag: \"rex\" }
}

fn main() {
    let shape = Shape.Circle(20);
    let event = Event.Moved { x: 1, y: 2 };
    let rect = Rect { width: 3, height: 4 };
    let boxed_int = Box { value: 42 };
    let boxed_text = Box { value: \"boxed\" };
    let maybe: Option<i64> = .Some(5);
    let parsed = parse(7);
    let letter: char = 'x';
    let small: i8 = -3;
    let short: i16 = 300;
    let byte: u8 = 200;
    let offset = 100;
    let add = |x: i64| x + offset;
    let named = pick();
    let tup = (7, \"seven\", 2.5);
    var words: Vec<string> = Vec.new();
    words.push(\"alpha\");
    var counts: HashMap<string, i64> = HashMap.new();
    counts.insert(\"one\", 1);
    var squares: HashMap<i64, i64> = HashMap.new();
    squares.insert(2, 4);
    var shapes: HashMap<i64, Rect> = HashMap.new();
    shapes.insert(1, rect);
    var seen: HashSet<i64> = HashSet.new();
    seen.insert(4);
    let counter = spawn Counter { count: 0 };
    match counter.bump() { .Ok(x) => println(x), .Err(_) => println(-1) }
    match parsed { .Ok(x) => println(x), .Err(e) => println(e) }
    match maybe { .Some(x) => println(x), .None => println(0) }
    match event { .Moved { x, y } => println(x + y), .Quit => println(0) }
    match shape { .Circle(r) => println(r), .Rect(w, h) => println(w * h), .Empty => println(0) }
    println(f\"{boxed_int.value} {boxed_text.value} {letter} {small} {short} {byte} {add(1)}\");
    println(f\"{named.name()} {tup.0} {words.len()} {counts.len()} {squares.len()}\");
    println(f\"{shapes.len()} {seen.len()}\");
}
";

fn types() -> Dies {
    let dir = tempfile::tempdir().expect("temp dir");
    let object = emit(dir.path(), "types", TYPES);
    read_dies(&object)
}

/// Each local is named as the source spells its type, and every generic
/// instantiation is its own type.
#[test]
fn types_carry_their_source_names() {
    let dies = types();
    let spelled = |local: &str| dies.name(dies.local(local)).to_owned();
    assert_eq!(spelled("shape"), "Shape");
    assert_eq!(spelled("rect"), "Rect");
    assert_eq!(spelled("boxed_int"), "Box<i64>");
    assert_eq!(spelled("boxed_text"), "Box<string>");
    assert_eq!(spelled("maybe"), "Option<i64>");
    assert_eq!(spelled("parsed"), "Result<i64, string>");
    assert_eq!(spelled("tup"), "(i64, string, f64)");
    assert_eq!(spelled("words"), "Vec<string>");
    assert_eq!(spelled("counts"), "HashMap<string, i64>");
    assert_eq!(spelled("seen"), "HashSet<i64>");
    assert_eq!(spelled("named"), "dyn Named");
    assert_eq!(spelled("counter"), "Counter");
    assert_eq!(spelled("small"), "i8");
    assert_eq!(spelled("short"), "i16");
    assert_eq!(spelled("byte"), "u8");
    assert_eq!(spelled("offset"), "i64");
    assert!(
        spelled("add").ends_with("(i64) -> i64") && spelled("add").starts_with("fn"),
        "a closure is named for its signature: {}",
        spelled("add")
    );
    assert_ne!(
        dies.local("boxed_int"),
        dies.local("boxed_text"),
        "Box<i64> and Box<string> are distinct types"
    );
    assert_eq!(
        dies.name(dies.member(dies.local("boxed_text"), "value").1),
        "string"
    );
}

/// A scalar is a typedef with its Hew name over a C base type of the right
/// encoding; `char` is a UTF code point, not a C byte.
#[test]
fn scalars_are_named_typedefs_over_c_base_types() {
    let dies = types();
    for (local, name, base, encoding, size) in [
        ("small", "i8", "int8_t", constants::DW_ATE_signed, 1),
        ("short", "i16", "int16_t", constants::DW_ATE_signed, 2),
        ("byte", "u8", "uint8_t", constants::DW_ATE_unsigned, 1),
        ("offset", "i64", "int64_t", constants::DW_ATE_signed, 8),
        ("letter", "char", "char32_t", constants::DW_ATE_UTF, 4),
    ] {
        let typedef = dies.local(local);
        assert_eq!(dies.get(typedef).tag, constants::DW_TAG_typedef, "{local}");
        assert_eq!(dies.name(typedef), name, "{local}");
        let base_type = dies.get(dies.type_of(typedef));
        assert_eq!(base_type.tag, constants::DW_TAG_base_type, "{local}");
        assert_eq!(base_type.name.as_deref(), Some(base), "{local}");
        assert_eq!(base_type.encoding, Some(encoding), "{local}");
        assert_eq!(base_type.byte_size, Some(size), "{local}");
    }
}

/// An enum is a plain tagged struct a debugger prints without a script:
/// `{tag = Circle, payload = {Circle = {_0 = 20}, ...}}`. A tuple variant's
/// fields are positions; a struct variant keeps its source field names.
#[test]
fn enums_are_tagged_structs_with_a_payload_union() {
    let dies = types();
    assert!(
        dies.dies
            .iter()
            .all(|die| die.tag != constants::DW_TAG_variant_part),
        "no consumer-specific variant part is emitted"
    );
    for (local, variants) in [
        (
            "shape",
            vec![
                ("Circle", vec!["_0"]),
                ("Rect", vec!["_0", "_1"]),
                ("Empty", vec![]),
            ],
        ),
        ("event", vec![("Moved", vec!["x", "y"]), ("Quit", vec![])]),
        ("maybe", vec![("Some", vec!["_0"]), ("None", vec![])]),
        ("parsed", vec![("Ok", vec!["_0"]), ("Err", vec!["_0"])]),
    ] {
        let carrier = dies.local(local);
        assert_eq!(
            dies.get(carrier).tag,
            constants::DW_TAG_structure_type,
            "{local}"
        );
        assert_eq!(dies.member_names(carrier), ["tag", "payload"], "{local}");
        let (tag_offset, tag) = dies.member(carrier, "tag");
        assert_eq!(tag_offset, 0, "{local}");
        assert_eq!(
            dies.get(tag).tag,
            constants::DW_TAG_enumeration_type,
            "{local}"
        );
        let expected: Vec<(String, i64)> = variants
            .iter()
            .enumerate()
            .map(|(index, (name, _))| ((*name).to_owned(), i64::try_from(index).unwrap()))
            .collect();
        assert_eq!(
            dies.enumerators(tag),
            expected,
            "{local}: tag values follow declaration order"
        );

        let (payload_offset, payload) = dies.member(carrier, "payload");
        assert!(payload_offset > 0, "{local}: the payload follows the tag");
        assert_eq!(
            dies.get(payload).tag,
            constants::DW_TAG_union_type,
            "{local}"
        );
        let arms = dies.members(payload);
        assert_eq!(arms.len(), variants.len(), "{local}");
        for ((arm, offset, arm_type), (variant, fields)) in arms.iter().zip(&variants) {
            assert_eq!(arm, variant, "{local}");
            assert_eq!(*offset, 0, "{local}: every arm overlays the payload");
            assert_eq!(dies.member_names(*arm_type), *fields, "{local}.{variant}");
        }
    }
    let shape = dies.local("shape");
    let circle = dies.member(dies.member(shape, "payload").1, "Circle").1;
    assert_eq!(dies.name(dies.member(circle, "_0").1), "i64");
}

/// Records, tuples, closures and trait objects name their members, each typed
/// per element.
#[test]
fn aggregates_name_and_type_each_member() {
    let dies = types();
    let rect = dies.local("rect");
    assert_eq!(dies.member_names(rect), ["width", "height"]);
    let tup = dies.local("tup");
    let element_types: Vec<String> = dies
        .members(tup)
        .into_iter()
        .map(|(name, _, ty)| format!("{name}: {}", dies.name(ty)))
        .collect();
    assert_eq!(element_types, ["_0: i64", "_1: string", "_2: f64"]);
    let add = dies.local("add");
    assert_eq!(dies.member_names(add), ["env", "descriptor"]);
    let environment = dies.target(dies.member(add, "env").1);
    let captures: Vec<String> = dies
        .members(environment)
        .into_iter()
        .map(|(name, _, ty)| format!("{name}: {}", dies.name(ty)))
        .collect();
    assert_eq!(
        captures[1..],
        ["_0: i64"],
        "the environment names each capture"
    );
    assert_eq!(dies.member_names(dies.local("named")), ["data", "vtable"]);
}

/// `string` is a handle to the managed header: its length and bytes.
#[test]
fn a_string_is_a_handle_to_its_length_and_bytes() {
    let dies = types();
    let header = dies.target(dies.member(dies.local("boxed_text"), "value").1);
    assert_eq!(dies.name(header), "hew_string");
    assert_eq!(dies.member_names(header), ["byte_len", "data"]);
}

/// `Vec<T>` is a handle to `{ptr: *T, len, cap}` at the runtime header's own
/// member offsets.
#[test]
fn a_vec_header_matches_the_runtime_layout() {
    use hew_cabi::vec::HewVec;
    let dies = types();
    let words = dies.local("words");
    assert_eq!(dies.get(words).tag, constants::DW_TAG_typedef);
    let header = dies.target(words);
    assert_eq!(dies.name(header), "Vec<string>.Header");
    let expected = [
        ("ptr", std::mem::offset_of!(HewVec, data)),
        ("len", std::mem::offset_of!(HewVec, len)),
        ("cap", std::mem::offset_of!(HewVec, cap)),
    ];
    let members: Vec<(String, u64)> = dies
        .members(header)
        .into_iter()
        .map(|(name, offset, _)| (name, offset))
        .collect();
    let expected: Vec<(String, u64)> = expected
        .iter()
        .map(|(name, offset)| ((*name).to_owned(), *offset as u64))
        .collect();
    assert_eq!(members, expected, "a reordered HewVec must fail this test");
    let (_, ptr) = dies.member(header, "ptr");
    assert_eq!(dies.get(ptr).tag, constants::DW_TAG_pointer_type);
    assert_eq!(
        dies.name(dies.type_of(ptr)),
        "string",
        "ptr addresses the elements"
    );
}

/// `HashMap<K, V>` is a handle to `{entries, len, cap, stride}`, and each
/// entry places `state`, `key` and `value` where the runtime's slot layout
/// puts them.
#[test]
fn hash_map_slots_match_the_runtime_layout() {
    use hew_runtime::hashmap::{compute_slot_layout, HewLayoutHashMap, OCCUPIED};
    let dies = types();
    for (local, name, key, value) in [
        ("squares", "HashMap<i64, i64>", (8, 8), (8, 8)),
        ("counts", "HashMap<string, i64>", (8, 8), (8, 8)),
        ("shapes", "HashMap<i64, Rect>", (8, 8), (16, 8)),
    ] {
        let handle = dies.local(local);
        assert_eq!(dies.name(handle), name);
        let header = dies.target(handle);
        let expected: Vec<(String, u64)> = [
            ("entries", std::mem::offset_of!(HewLayoutHashMap, entries)),
            ("len", std::mem::offset_of!(HewLayoutHashMap, len)),
            ("cap", std::mem::offset_of!(HewLayoutHashMap, cap)),
            ("stride", std::mem::offset_of!(HewLayoutHashMap, stride)),
        ]
        .iter()
        .map(|(member, offset)| ((*member).to_owned(), *offset as u64))
        .collect();
        let members: Vec<(String, u64)> = dies
            .members(header)
            .into_iter()
            .map(|(member, offset, _)| (member, offset))
            .collect();
        assert_eq!(
            members, expected,
            "{name}: a reordered header must fail this test"
        );

        let entry = dies.target(dies.member(header, "entries").1);
        assert_eq!(dies.name(entry), format!("{name}.Entry"));
        let (key_offset, value_offset, stride, _) =
            compute_slot_layout(key.0, key.1, value.0, value.1).expect("slot layout");
        assert_eq!(dies.member(entry, "state").0, 0, "{name}");
        assert_eq!(dies.member(entry, "key").0, key_offset as u64, "{name}");
        assert_eq!(dies.member(entry, "value").0, value_offset as u64, "{name}");
        assert_eq!(dies.get(entry).byte_size, Some(stride as u64), "{name}");

        let state = dies.member(entry, "state").1;
        assert_eq!(dies.name(state), "HashSlotState");
        assert!(
            dies.enumerators(state)
                .contains(&("OCCUPIED".to_owned(), i64::from(OCCUPIED))),
            "{name}: a script selects live slots by the OCCUPIED enumerator"
        );
    }
    let shapes = dies.target(dies.member(dies.target(dies.local("shapes")), "entries").1);
    assert_eq!(dies.name(dies.member(shapes, "value").1), "Rect");
}

/// `HashSet<T>` is a handle to the set, which holds a map whose slots carry
/// the element as their key and no value.
#[test]
fn a_hash_set_is_a_map_without_values() {
    use hew_runtime::hashmap::compute_slot_layout;
    use hew_runtime::hashset::HewLayoutHashSet;
    let dies = types();
    let set = dies.target(dies.local("seen"));
    assert_eq!(dies.name(set), "HashSet<i64>.Header");
    assert_eq!(
        dies.get(set).byte_size,
        Some(std::mem::size_of::<HewLayoutHashSet>() as u64),
        "the set is exactly its map pointer"
    );
    let map = dies.target(dies.member(set, "map").1);
    let entry = dies.target(dies.member(map, "entries").1);
    assert_eq!(dies.member_names(entry), ["state", "key"]);
    let (key_offset, _, stride, _) = compute_slot_layout(8, 8, 0, 1).expect("slot layout");
    assert_eq!(dies.member(entry, "key").0, key_offset as u64);
    assert_eq!(dies.get(entry).byte_size, Some(stride as u64));
}

/// Two actors whose state holds each other's handle, the shape of a local ask
/// cycle. Their `Option<Ping>` and `Option<Pong>` are distinct types; when
/// both were described as `Option`, LLVM merged one into the other and left
/// a variable pointing at the deleted node.
const ASK_CYCLE: &str = "\
actor Ping {
    var peer: Option<Pong>;

    receive fn link(p: Pong) {
        peer = .Some(p);
    }

    receive fn go() -> i64 {
        match peer {
            .Some(p) => match p.back() {
                .Ok(v) => v,
                .Err(_) => -1,
            }
            .None => -3,
        }
    }

    receive fn pong() -> i64 {
        1
    }
}

actor Pong {
    var peer: Option<Ping>;

    receive fn link(p: Ping) {
        peer = .Some(p);
    }

    receive fn back() -> i64 {
        match peer {
            .Some(p) => match p.pong() {
                .Ok(v) => v,
                .Err(_) => -2,
            }
            .None => -4,
        }
    }
}

fn main() {
    let a = spawn Ping { peer: .None };
    let b = spawn Pong { peer: .None };
    let _ = a.link(b);
    let _ = b.link(a);
    match a.go() {
        .Ok(v) => println(v),
        .Err(_) => println(\"failed\"),
    }
}
";

#[test]
fn mutually_referring_actors_emit_with_debug_metadata() {
    let dir = tempfile::tempdir().expect("temp dir");
    let dies = read_dies(&emit(dir.path(), "ask_cycle", ASK_CYCLE));
    for name in ["Option<Ping>", "Option<Pong>"] {
        dies.definition(constants::DW_TAG_structure_type, name);
    }
}

/// A type that reaches itself through a handle describes the inner reference
/// as a declaration of the same name, not an endless expansion.
const RECURSIVE: &str = "\
type Tree {
    label: string;
    children: Vec<Tree>;
}

indirect enum List {
    Cons(i64, List);
    Nil;
}

fn main() {
    let leaf = Tree { label: \"leaf\", children: Vec.new() };
    var children: Vec<Tree> = Vec.new();
    children.push(leaf);
    let root = Tree { label: \"root\", children: children };
    let list = List.Cons(1, List.Cons(2, List.Nil));
    match list { .Cons(head, _) => println(head), .Nil => println(0) }
    println(root.children.len());
}
";

#[test]
fn recursive_types_resolve_through_a_declaration() {
    let dir = tempfile::tempdir().expect("temp dir");
    let dies = read_dies(&emit(dir.path(), "recursive", RECURSIVE));
    let tree = dies.local("root");
    assert_eq!(dies.name(tree), "Tree");
    let children = dies.target(dies.member(tree, "children").1);
    let element = dies.type_of(dies.member(children, "ptr").1);
    assert_eq!(
        dies.name(element),
        "Tree",
        "the element is the record itself"
    );

    let list = dies.local("list");
    assert_eq!(dies.name(list), "List");
    let node = dies.target(list);
    assert_eq!(dies.name(node), "List.Node");
    assert_eq!(dies.member_names(node), ["tag", "payload"]);
    let cons = dies.member(dies.member(node, "payload").1, "Cons").1;
    assert_eq!(dies.name(dies.member(cons, "_1").1), "List");
}

/// A declaration entered for a recursive type and its definition carry one
/// identifier: CodeView completes a forward reference by that unique name.
#[test]
fn a_recursive_declaration_shares_its_definition_identifier() {
    let dir = tempfile::tempdir().expect("temp dir");
    let (_, ir) = emit_with_ir(dir.path(), "recursive", RECURSIVE);
    let ir = std::fs::read_to_string(ir).expect("read IR");
    for name in ["Tree", "List.Node"] {
        let identifiers: Vec<&str> = ir
            .lines()
            .filter(|line| {
                line.contains("DICompositeType(tag: DW_TAG_structure_type")
                    && line.contains(&format!("name: \"{name}\""))
            })
            .filter_map(|line| line.split("identifier: \"").nth(1))
            .filter_map(|rest| rest.split('"').next())
            .collect();
        assert!(
            identifiers.len() >= 2,
            "`{name}` has a declaration and a definition: {identifiers:?}"
        );
        assert!(
            identifiers.windows(2).all(|pair| pair[0] == pair[1]),
            "`{name}` keeps one identifier: {identifiers:?}"
        );
    }
}
