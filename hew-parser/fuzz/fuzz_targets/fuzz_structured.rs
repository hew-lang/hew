#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Arbitrary)]
struct HewProgram {
    imports: Vec<ImportChoice>,
    items: Vec<HewItem>,
    flavor: u8,
}

#[derive(Debug, Clone, Copy, Arbitrary)]
enum ImportChoice {
    String,
    Fs,
    Net,
    Time,
    Json,
}

#[derive(Debug, Arbitrary)]
enum HewItem {
    Function(FunctionSpec),
    Struct(StructSpec),
    Enum(EnumSpec),
    Trait(TraitSpec),
    Impl(ImplSpec),
    Actor(ActorSpec),
    Generator(GeneratorSpec),
}

#[derive(Debug, Arbitrary)]
struct FunctionSpec {
    seed: u8,
}

#[derive(Debug, Arbitrary)]
struct StructSpec {
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
    with_gen: bool,
}

#[derive(Debug, Arbitrary)]
struct GeneratorSpec {
    count: u8,
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
        out.push('\n');

        out.push_str(
            r#"type CoreType {
    value: int;
}

enum CoreChoice {
    Zero;
    Num(int);
}

trait CoreTrait {
    fn apply(self: Self, x: int) -> int;
}

impl CoreTrait for CoreType {
    fn apply(self: CoreType, x: int) -> int {
        self.value + x
    }
}

actor CoreActor {
    let count: int;

    receive fn add(n: int) -> int {
        self.count = self.count + n;
        self.count
    }

    receive fn current() -> int {
        self.count
    }

    receive gen fn stream() -> int {
        var i = 0;
        while i < 3 {
            yield i;
            i = i + 1;
        }
    }
}

gen fn core_numbers() -> int {
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
fn main() -> Result<int, int> {
    let total = core_pipeline(2)?;
    let summary = f"main {total}";
    Ok(total)
}
"#,
        );

        out
    }

    fn core_function(&self) -> String {
        let loop_limit = i32::from(self.flavor % 4) + 2;
        format!(
            r#"
fn core_div(a: int, b: int) -> Result<int, int> {{
    if b == 0 {{
        Err(1)
    }} else {{
        Ok(a / b)
    }}
}}

fn core_pipeline(seed: int) -> Result<int, int> {{
    let closure = (x) => x + seed;
    let thunk = () => closure(1);

    let nums: Vec<int> = Vec::new();
    nums.push(closure(1));
    nums.push(closure(2));

    let table: HashMap<String, int> = HashMap::new();
    table.insert("first", nums.get(0));
    table.insert("second", nums.get(1));

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

    loop {{
        acc = acc + 1;
        break;
    }}

    for j in 0 .. 3 {{
        acc = acc + j;
    }}

    let scoped = scope |s| {{
        let task = s.launch {{
            closure(3)
        }};
        acc + 1
    }};

    let interp = f"seed {{seed}} value {{scoped}}";
    Ok(mut_sum + scoped)
}}
"#
        )
    }
}

impl HewItem {
    fn to_source(&self, idx: usize) -> String {
        match self {
            HewItem::Function(spec) => spec.to_source(idx),
            HewItem::Struct(spec) => spec.to_source(idx),
            HewItem::Enum(spec) => spec.to_source(idx),
            HewItem::Trait(spec) => spec.to_source(idx),
            HewItem::Impl(spec) => spec.to_source(),
            HewItem::Actor(spec) => spec.to_source(idx),
            HewItem::Generator(spec) => spec.to_source(idx),
        }
    }
}

impl ImportChoice {
    fn as_source(self) -> &'static str {
        match self {
            ImportChoice::String => "import std::string;",
            ImportChoice::Fs => "import std::fs;",
            ImportChoice::Net => "import std::net;",
            ImportChoice::Time => "import std::time;",
            ImportChoice::Json => "import std::encoding::json;",
        }
    }
}

impl FunctionSpec {
    fn to_source(&self, idx: usize) -> String {
        let v = i32::from(self.seed % 5) + 1;
        format!(
            r#"fn fuzz_fn_{idx}(input: int) -> Result<int, int> {{
    let mapper = (x) => x + {v};
    let mapped = mapper(input);
    let result = core_div(mapped, 1)?;
    Ok(result)
}}
"#
        )
    }
}

impl StructSpec {
    fn to_source(&self, idx: usize) -> String {
        let count = usize::from(self.fields % 3) + 1;
        let mut out = format!("type FuzzType{idx} {{\n");
        for i in 0..count {
            let ty = match i % 4 {
                0 => "int",
                1 => "String",
                2 => "Vec<int>",
                _ => "HashMap<String, int>",
            };
            out.push_str(&format!("    field{i}: {ty};\n"));
        }
        out.push_str("}\n");
        out
    }
}

impl EnumSpec {
    fn to_source(&self, idx: usize) -> String {
        if self.with_payload {
            format!(
                "enum FuzzEnum{idx} {{\n    Empty;\n    One(int);\n    Pair(int, int);\n}}\n"
            )
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
            out.push_str(&format!(
                "    fn method_{m}(self: Self, x: int) -> int;\n"
            ));
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
    fn method_0(self: FuzzType{n}, x: int) -> int {{
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
    fn method_0(self: FuzzType{n}, x: int) -> int {{
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
    let total: int;

    receive fn bump(v: int) -> int {{
        self.total = self.total + v;
        self.total
    }}
    
    receive fn show() -> String {{
        f"actor {idx} {{self.total}}"
    }}
"#
        );
        if self.with_gen {
            out.push_str(
                r#"
    receive gen fn iter() -> int {
        var i = 0;
        while i < 2 {
            yield i;
            i = i + 1;
        }
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
        let mut out = format!("gen fn fuzz_gen_{idx}() -> int {{\n");
        for i in 0..count {
            out.push_str(&format!("    yield {i};\n"));
        }
        out.push_str("}\n");
        out
    }
}

fuzz_target!(|program: HewProgram| {
    let source = program.to_source();
    let _ = hew_parser::parse(&source);
});
