#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Arbitrary)]
struct HewProgram {
    imports: Vec<ImportChoice>,
    items: Vec<HewItem>,
    flavour: u8,
}

#[derive(Debug, Clone, Copy, Arbitrary)]
enum ImportChoice {
    String,
    Time,
    Json,
}

#[derive(Debug, Arbitrary)]
enum HewItem {
    Function(FunctionSpec),
    Record(RecordSpec),
    Enum(EnumSpec),
    Trait(TraitSpec),
    Impl(ImplSpec),
    Actor(ActorSpec),
    Generator(GeneratorSpec),
    Wire(WireSpec),
    Surface(SurfaceSpec),
}

#[derive(Debug, Arbitrary)]
struct FunctionSpec {
    seed: u8,
}

#[derive(Debug, Arbitrary)]
struct RecordSpec {
    fields: u8,
}

#[derive(Debug, Arbitrary)]
struct EnumSpec {
    with_payload: bool,
}

#[derive(Debug, Arbitrary)]
struct TraitSpec {
    methods: u8,
}

#[derive(Debug, Arbitrary)]
struct ImplSpec {
    target: u8,
    with_loop: bool,
}

#[derive(Debug, Arbitrary)]
struct ActorSpec {
    lifecycle: bool,
}

#[derive(Debug, Arbitrary)]
struct GeneratorSpec {
    count: u8,
}

#[derive(Debug, Arbitrary)]
struct WireSpec {
    fields: u8,
}

#[derive(Debug, Arbitrary)]
struct SurfaceSpec {
    variant: u8,
}

impl HewProgram {
    fn to_source(&self) -> String {
        let mut out = String::new();
        let imports = if self.imports.is_empty() {
            vec![ImportChoice::String]
        } else {
            self.imports.iter().copied().take(3).collect()
        };
        for import in imports {
            out.push_str(import.as_source());
            out.push('\n');
        }

        out.push_str(
            r#"
record CoreType {
    value: i64,
}

enum CoreChoice {
    Zero;
    Num(i64);
}

trait CoreTrait {
    fn apply(self: Self, x: i64) -> i64;
}

impl CoreTrait for CoreType {
    fn apply(self: CoreType, x: i64) -> i64 {
        self.value + x
    }
}

actor CoreActor {
    let count: i64;

    init(initial: i64) {
        count = initial;
    }

    receive fn add(n: i64) -> i64 {
        count = count + n;
        count
    }

    receive fn current() -> i64 {
        count
    }
}

gen fn core_numbers() -> i64 {
    yield 1;
    yield 2;
    yield 3;
}
"#,
        );

        out.push_str(&self.core_function());

        for (idx, item) in self.items.iter().take(10).enumerate() {
            out.push('\n');
            out.push_str(&item.to_source(idx));
        }

        out.push_str(
            r#"
fn main() -> Result<i64, i64> {
    let total = core_pipeline(2)?;
    let summary = f"main {total}";
    Ok(total)
}
"#,
        );
        out
    }

    fn core_function(&self) -> String {
        let loop_limit = i32::from(self.flavour % 4) + 2;
        format!(
            r#"
fn core_div(a: i64, b: i64) -> Result<i64, i64> {{
    if b == 0 {{
        Err(1)
    }} else {{
        Ok(a / b)
    }}
}}

fn core_pipeline(seed: i64) -> Result<i64, i64> {{
    let closure = |x| x + seed;
    let thunk = || closure(1);
    let maybe = Some(thunk());
    let picked = match maybe {{
        Some(v) => v,
        None => 0,
    }};
    let r = core_div(picked + seed, 1)?;
    let mut_sum = match CoreChoice::Num(r) {{
        CoreChoice::Zero => 0,
        CoreChoice::Num(v) => v,
    }};

    var acc = 0;
    var i = 0;
    while i < {loop_limit} {{
        acc = acc + i;
        i = i + 1;
    }}

    let same = seed is seed;
    let guarded = unsafe {{
        if same {{ acc }} else {{ 0 }}
    }};
    let pat = re"^[a-z]+$";
    let blob = bytes[1, 2, 3];
    Ok(mut_sum + guarded)
}}
"#
        )
    }
}

impl HewItem {
    fn to_source(&self, idx: usize) -> String {
        match self {
            Self::Function(spec) => spec.to_source(idx),
            Self::Record(spec) => spec.to_source(idx),
            Self::Enum(spec) => spec.to_source(idx),
            Self::Trait(spec) => spec.to_source(idx),
            Self::Impl(spec) => spec.to_source(),
            Self::Actor(spec) => spec.to_source(idx),
            Self::Generator(spec) => spec.to_source(idx),
            Self::Wire(spec) => spec.to_source(idx),
            Self::Surface(spec) => spec.to_source(idx),
        }
    }
}

impl ImportChoice {
    fn as_source(self) -> &'static str {
        match self {
            Self::String => "import std::string;",
            Self::Time => "import std::time;",
            Self::Json => "import std::encoding::json;",
        }
    }
}

impl FunctionSpec {
    fn to_source(&self, idx: usize) -> String {
        let v = i32::from(self.seed % 5) + 1;
        format!(
            r#"fn fuzz_fn_{idx}(input: i64) -> Result<i64, i64> {{
    let mapper = |x| x + {v};
    let mapped = mapper(input);
    let result = core_div(mapped, 1)?;
    Ok(result)
}}
"#
        )
    }
}

impl RecordSpec {
    fn to_source(&self, idx: usize) -> String {
        let count = usize::from(self.fields % 3) + 1;
        let mut out = format!("record FuzzType{idx} {{\n");
        for i in 0..count {
            let ty = match i % 3 {
                0 => "i64",
                1 => "string",
                _ => "bool",
            };
            let sep = if i + 1 == count { "" } else { "," };
            out.push_str(&format!("    field{i}: {ty}{sep}\n"));
        }
        out.push_str("}\n");
        out
    }
}

impl EnumSpec {
    fn to_source(&self, idx: usize) -> String {
        if self.with_payload {
            format!("enum FuzzEnum{idx} {{\n    Empty;\n    One(i64);\n    Pair(i64, i64);\n}}\n")
        } else {
            format!("enum FuzzEnum{idx} {{\n    A;\n    B;\n    C;\n}}\n")
        }
    }
}

impl TraitSpec {
    fn to_source(&self, idx: usize) -> String {
        let methods = usize::from(self.methods % 2) + 1;
        let mut out = format!("trait FuzzTrait{idx} {{\n");
        for m in 0..methods {
            out.push_str(&format!("    fn method_{m}(self: Self, x: i64) -> i64;\n"));
        }
        out.push_str("}\n");
        out
    }
}

impl ImplSpec {
    fn to_source(&self) -> String {
        let n = usize::from(self.target % 10);
        if self.with_loop {
            format!(
                r#"impl FuzzTrait{n} for FuzzType{n} {{
    fn method_0(self: FuzzType{n}, x: i64) -> i64 {{
        var i = 0;
        var sum = x;
        while i < 2 {{
            sum = sum + i;
            i = i + 1;
        }}
        sum
    }}
}}
"#
            )
        } else {
            format!(
                r#"impl FuzzTrait{n} for FuzzType{n} {{
    fn method_0(self: FuzzType{n}, x: i64) -> i64 {{
        match x {{
            0 => 0,
            _ => x,
        }}
    }}
}}
"#
            )
        }
    }
}

impl ActorSpec {
    fn to_source(&self, idx: usize) -> String {
        let mut out = format!(
            r#"actor FuzzActor{idx} {{
    let total: i64;

    receive fn bump(v: i64) -> i64 {{
        total = total + v;
        total
    }}
"#
        );
        if self.lifecycle {
            out.push_str(
                r#"
    #[on(start)]
    fn boot() {
        total += 1;
    }
"#,
            );
        }
        out.push_str("}\n");
        out
    }
}

impl GeneratorSpec {
    fn to_source(&self, idx: usize) -> String {
        let count = usize::from(self.count % 4) + 1;
        let mut out = format!("gen fn fuzz_gen_{idx}() -> i64 {{\n");
        for i in 0..count {
            out.push_str(&format!("    yield {i};\n"));
        }
        out.push_str("}\n");
        out
    }
}

impl WireSpec {
    fn to_source(&self, idx: usize) -> String {
        let count = usize::from(self.fields % 3) + 1;
        let mut out = format!("wire type FuzzWire{idx} {{\n");
        for i in 0..count {
            let ty = match i % 5 {
                0 => "i64",
                1 => "string",
                2 => "bytes",
                3 => "bool",
                _ => "duration",
            };
            out.push_str(&format!("    field{i}: {ty} = {};\n", i + 1));
        }
        out.push_str("}\n");
        out
    }
}

impl SurfaceSpec {
    fn to_source(&self, idx: usize) -> String {
        match self.variant % 4 {
            0 => format!(
                r#"fn fuzz_surface_is_{idx}(x: i64) -> bool {{
    x is x
}}
"#
            ),
            1 => format!(
                r#"fn fuzz_surface_unsafe_{idx}(x: i64) -> i64 {{
    unsafe {{ x + 1 }}
}}
"#
            ),
            2 => format!(
                r#"#[intrinsic("fuzz.surface.{idx}")]
fn fuzz_intrinsic_{idx}(x: i64) -> i64 {{}}
"#
            ),
            _ => format!(
                r#"fn fuzz_surface_literals_{idx}() -> i64 {{
    let pat = re"^[a-z]+$";
    let blob = bytes[1, 2, 3];
    1
}}
"#
            ),
        }
    }
}

fuzz_target!(|program: HewProgram| {
    let source = program.to_source();
    let _ = hew_parser::parse(&source);
});
