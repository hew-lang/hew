#!/usr/bin/env node
/**
 * Grammar Fuzzer for Hew
 *
 * Generates or accepts Hew source programs and tokenizes them through
 * the TextMate grammar (vscode-textmate) and/or parses them through
 * tree-sitter-hew, checking for crashes, timeouts, untokenized regions,
 * and keyword coverage.
 *
 * Usage:
 *   node grammar-fuzz.mjs                    # Run all checks (TextMate + tree-sitter)
 *   node grammar-fuzz.mjs --textmate         # TextMate only
 *   node grammar-fuzz.mjs --tree-sitter      # tree-sitter only
 *   node grammar-fuzz.mjs --diff             # Both, compare token categories
 *   node grammar-fuzz.mjs --corpus <dir>     # Use external .hew files
 *   node grammar-fuzz.mjs --generate <n>     # Generate n programs (default: 200)
 *   node grammar-fuzz.mjs --timeout <ms>     # Per-file timeout (default: 5000)
 *   node grammar-fuzz.mjs --verbose          # Show per-file results
 */

import { readFileSync, readdirSync, writeFileSync, rmSync } from 'node:fs';
import { join, resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { execFileSync } from 'node:child_process';
import vsctm from 'vscode-textmate';
const { Registry, INITIAL } = vsctm;
import onig from 'vscode-oniguruma';
const { loadWASM, createOnigScanner, createOnigString } = onig;

const __dirname = dirname(fileURLToPath(import.meta.url));
const HEW_ROOT = resolve(__dirname, '../..');
const SYNTAX_DATA_PATH = join(HEW_ROOT, 'docs/syntax-data.json');
const TM_GRAMMAR_PATH = resolve(HEW_ROOT, '../vscode-hew/syntaxes/hew.tmLanguage.json');
const TREE_SITTER_DIR = resolve(HEW_ROOT, '../tree-sitter-hew');

// ── CLI argument parsing ─────────────────────────────────────────────

function parseArgs() {
  const args = process.argv.slice(2);
  const opts = {
    textmateOnly: false,
    treeSitterOnly: false,
    diff: false,
    corpus: null,
    generate: 200,
    timeout: 5000,
    verbose: false,
  };

  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case '--textmate': opts.textmateOnly = true; break;
      case '--tree-sitter': opts.treeSitterOnly = true; break;
      case '--diff': opts.diff = true; break;
      case '--corpus': opts.corpus = args[++i]; break;
      case '--generate': opts.generate = parseInt(args[++i], 10); break;
      case '--timeout': opts.timeout = parseInt(args[++i], 10); break;
      case '--verbose': opts.verbose = true; break;
      case '--help': case '-h':
        console.log('Usage: node grammar-fuzz.mjs [--textmate|--tree-sitter|--diff] [--corpus <dir>] [--generate <n>] [--timeout <ms>] [--verbose]');
        process.exit(0);
      default:
        console.error(`Unknown argument: ${args[i]}`);
        process.exit(1);
    }
  }

  // Default: run both
  if (!opts.textmateOnly && !opts.treeSitterOnly && !opts.diff) {
    opts.diff = true;
  }

  return opts;
}

// ── Colours ──────────────────────────────────────────────────────────

const C = {
  green: s => `\x1b[32m${s}\x1b[0m`,
  red: s => `\x1b[31m${s}\x1b[0m`,
  yellow: s => `\x1b[33m${s}\x1b[0m`,
  cyan: s => `\x1b[36m${s}\x1b[0m`,
  dim: s => `\x1b[2m${s}\x1b[0m`,
  bold: s => `\x1b[1m${s}\x1b[0m`,
};

// ── Syntax data ──────────────────────────────────────────────────────

const syntaxData = JSON.parse(readFileSync(SYNTAX_DATA_PATH, 'utf-8'));

// ── Corpus generator ─────────────────────────────────────────────────

function generateCorpus(count) {
  const programs = [];

  // Category 1: Keyword coverage — every keyword in a plausible context
  programs.push({
    name: 'keywords_control_flow',
    category: 'keywords',
    source: `
fn main() {
    let x = 42;
    if x > 0 {
        return x;
    } else {
        return 0;
    }

    match x {
        0 => break,
        _ => continue,
    }

    loop { break; }
    for i in 0..10 { continue; }
    while x > 0 { x = x - 1; }
    yield 1;
    defer println("done");
}
`,
  });

  programs.push({
    name: 'keywords_declarations',
    category: 'keywords',
    source: `
import std::string;
package foo;

pub fn example() -> i32 { 0 }
pub async fn async_example() -> i32 { 0 }
pub gen fn gen_example() -> i32 { yield 1; }

let x = 10;
var y = 20;
const Z: i32 = 30;

type Point { x: i32; y: i32; }
indirect enum Tree { Leaf(i32); Node(Tree, Tree); }
struct Flat { a: i32; }
trait Printable { fn print(self: Self) -> string; }
impl Printable for Point { fn print(self: Point) -> string { f"{self.x}" } }

fn convert(x: i32) -> f64 {
    x as f64
}

fn generic<T>(x: T) -> T where T: Copy {
    x
}

extern fn ffi_call(n: i32) -> i32;
`,
  });

  programs.push({
    name: 'keywords_actors',
    category: 'keywords',
    source: `
actor Counter {
    let count: i32;

    init {
        self.count = 0;
    }

    receive fn add(n: i32) -> i32 {
        self.count = self.count + n;
        self.count
    }

    receive gen fn stream() -> i32 {
        yield self.count;
    }
}

supervisor CounterSup {
    child counter: Counter;
    restart permanent;
    budget 5;
    strategy one_for_one;
}

fn main() {
    let ref = spawn Counter {};
    scope |s| {
        let task = s.launch { 42 };
        select {
            msg from ref => {},
            after 1000ms => {},
        }
    };
    let val = await ref.add(1);
    join task1, task2;
    cooperate;
    move ref;
}
`,
  });

  programs.push({
    name: 'keywords_wire',
    category: 'keywords',
    source: `
wire Message {
    reserved 1;
    optional name: string = 2;
    deprecated old_field: i32 = 3;
    default value: i32 = 4;
}
`,
  });

  programs.push({
    name: 'keywords_machine',
    category: 'keywords',
    source: `
machine TrafficLight {
    state Red;
    state Yellow;
    state Green;

    event Timer;
    event Emergency;

    on Timer when Red => Green;
    on Timer when Green => Yellow;
    on Timer when Yellow => Red;
    on Emergency when _ => Red;
}
`,
  });

  programs.push({
    name: 'keywords_logical_reserved',
    category: 'keywords',
    source: `
fn main() {
    let a = true;
    let b = false;
    if a && !b { return 1; }
}
`,
  });

  programs.push({
    name: 'keywords_supervisor_config',
    category: 'keywords',
    source: `
supervisor MySup {
    child worker: Worker;
    restart permanent;
    restart transient;
    restart temporary;
    strategy one_for_one;
    strategy one_for_all;
    strategy rest_for_one;
}
`,
  });

  programs.push({
    name: 'keywords_other',
    category: 'keywords',
    source: `
fn risky() {
    unsafe { do_ffi(); }
    let d: dyn Printable = get_printable();
    pure fn add(a: i32, b: i32) -> i32 { a + b }
}
`,
  });

  // Category 2: All types
  programs.push({
    name: 'types_all',
    category: 'types',
    source: `
fn types() {
    let a: i8 = 1;
    let b: i16 = 2;
    let c: i32 = 3;
    let d: i64 = 4;
    let e: u8 = 5;
    let f: u16 = 6;
    let g: u32 = 7;
    let h: u64 = 8;
    let i: isize = 9;
    let j: usize = 10;
    let k: f32 = 1.0;
    let l: f64 = 2.0;
    let m: bool = true;
    let n: char = 'x';
    let o: string = "hello";
    let p: bytes = b"data";
    let q: duration = 5s;
    let v: Vec<i32> = Vec::new();
    let h: HashMap<string, i32> = HashMap::new();
    let opt: Option<i32> = Some(1);
    let res: Result<i32, string> = Ok(42);
    let none: Option<i32> = None;
    let err: Result<i32, string> = Err("bad");
    let r: ActorRef<Counter> = spawn Counter {};
    let st: Stream<i32> = get_stream();
    let si: Sink<i32> = get_sink();
    let task: Task<i32> = spawn_task();
    let sc: Scope = get_scope();
    let g: Generator<i32> = get_gen();
    let ag: AsyncGenerator<i32> = get_async_gen();
    let send: Send = get_sendable();
    let frozen: Frozen = get_frozen();
    let copy: Copy = get_copy();
    let arc: Arc<i32> = Arc::new(1);
    let rc: Rc<i32> = Rc::new(2);
    let weak: Weak<i32> = get_weak();
    let range: Range<i32> = 0..10;
    let as2: ActorStream<i32> = get_actor_stream();
    fn ret_void() -> void {}
    fn ret_never() -> never { loop {} }
}
`,
  });

  // Category 3: Operators
  programs.push({
    name: 'operators_all',
    category: 'operators',
    source: `
fn operators(a: i32, b: i32) {
    let add = a + b;
    let sub = a - b;
    let mul = a * b;
    let div = a / b;
    let rem = a % b;

    let eq = a == b;
    let ne = a != b;
    let lt = a < b;
    let le = a <= b;
    let gt = a > b;
    let ge = a >= b;

    let and = eq && ne;
    let or = eq || ne;
    let not = !eq;

    let band = a & b;
    let bor = a | b;
    let bxor = a ^ b;
    let bnot = ~a;
    let shl = a << 2;
    let shr = a >> 2;

    let text = "hello";
    let pmatch = text =~ r"hel.*";
    let pnomatch = text !~ r"xyz";

    var x = 0;
    x += 1;
    x -= 1;
    x *= 2;
    x /= 2;
    x %= 3;
    x &= 0xff;
    x |= 0x01;
    x ^= 0x10;
    x <<= 1;
    x >>= 1;

    let range = 0..10;
    let range_inc = 0..=10;

    let opt: Option<i32> = Some(42);
    let val = opt?;

    let lambda = (x) => x + 1;
    fn returns_result() -> Result<i32, string> { Ok(1) }
}
`,
  });

  // Category 4: String variants
  programs.push({
    name: 'strings_all',
    category: 'strings',
    source: `
fn strings() {
    let plain = "hello world";
    let escaped = "line1\\nline2\\ttab\\"quote\\\\backslash";
    let interpolated = f"value is {42 + 1}";
    let nested_interp = f"outer {f"inner {1}"}";
    let raw = r"no \\escapes \\here";
    let regex = re"^[a-z]+$";
    let byte_str = b"\\x00\\x01\\x02";
    let empty = "";
    let unicode = "\\u{1F600} \\u{0041}";
    let multiline = "first line
second line
third line";
}
`,
  });

  // Category 5: Comments
  programs.push({
    name: 'comments_all',
    category: 'comments',
    source: `
// Line comment
/// Documentation comment
//! Inner doc comment

/* Block comment */
/* Nested /* block */ comment */

fn example() -> i32 {
    // inline comment
    let x = 42; // trailing comment
    /* mid-expression */ x
}
`,
  });

  // Category 6: Numbers and literals
  programs.push({
    name: 'numbers_all',
    category: 'literals',
    source: `
fn numbers() {
    let dec = 42;
    let neg = -17;
    let hex = 0xFF;
    let oct = 0o77;
    let bin = 0b1010;
    let big = 1_000_000;
    let flt = 3.14;
    let flt_e = 1.5e10;
    let flt_neg_e = 2.7e-3;
    let dur_ns = 100ns;
    let dur_us = 50us;
    let dur_ms = 250ms;
    let dur_s = 5s;
    let dur_m = 10m;
    let dur_h = 2h;
}
`,
  });

  // Category 7: Attributes
  programs.push({
    name: 'attributes',
    category: 'attributes',
    source: `
#[test]
fn test_something() { }

#[inline]
fn fast() -> i32 { 0 }

#[deprecated("use new_fn instead")]
fn old_fn() { }

#[export]
fn ffi_visible() -> i32 { 42 }
`,
  });

  // Category 8: Complex nesting
  programs.push({
    name: 'deep_nesting',
    category: 'pathological',
    source: `
fn deep() -> i32 {
    if true {
        if true {
            if true {
                match 1 {
                    1 => {
                        let x = {
                            let y = {
                                let z = (((42)));
                                z
                            };
                            y
                        };
                        x
                    },
                    _ => 0,
                }
            } else { 0 }
        } else { 0 }
    } else { 0 }
}
`,
  });

  // ── Bad / malformed input categories ────────────────────────────────

  // Category 9: Truncated programs (cut mid-expression)
  programs.push({
    name: 'truncated_fn',
    category: 'bad_input',
    source: `fn incomplete(x: i32) -> i32 {
    let y = x +`,
  });

  programs.push({
    name: 'truncated_string',
    category: 'bad_input',
    source: `fn broken() {
    let s = "unterminated string
    let x = 42;
}`,
  });

  programs.push({
    name: 'truncated_block_comment',
    category: 'bad_input',
    source: `/* this comment never closes
fn main() {
    let x = 42;
}`,
  });

  // Category 10: Garbage / random tokens
  programs.push({
    name: 'garbage_symbols',
    category: 'bad_input',
    source: `@#$%^&*()!~\`{}[]|\\:;"'<>,.?/`,
  });

  programs.push({
    name: 'garbage_mixed',
    category: 'bad_input',
    source: `fn @@@ let ### if $$$ while %%% { } } { ] [ ) (`,
  });

  programs.push({
    name: 'garbage_unicode',
    category: 'bad_input',
    source: `fn 日本語() -> 整数 {
    let 変数 = 42;
    let emoji = "🦀🔥💯";
    yield 変数;
}`,
  });

  // Category 11: Keywords in wrong positions
  programs.push({
    name: 'keywords_as_values',
    category: 'bad_input',
    source: `fn main() {
    let fn = if;
    let match = while;
    let return = break;
    let actor = supervisor;
    let spawn = receive;
}`,
  });

  programs.push({
    name: 'keywords_jammed',
    category: 'bad_input',
    source: `fnletvarifelsematchloopforwhilebreakcontinuereturn`,
  });

  // Category 12: Very long lines
  programs.push({
    name: 'long_line',
    category: 'pathological',
    source: `fn long() -> i32 { ${Array(200).fill('1 +').join(' ')} 0 }`,
  });

  // Category 13: Deeply nested parentheses
  programs.push({
    name: 'deep_parens',
    category: 'pathological',
    source: `fn parens() -> i32 { ${'('.repeat(50)}42${')'.repeat(50)} }`,
  });

  // Category 14: Repeated keywords
  programs.push({
    name: 'repeated_keywords',
    category: 'pathological',
    source: Array(50).fill('let x = 42;').join('\n'),
  });

  // Category 15: Empty and whitespace
  programs.push({
    name: 'empty',
    category: 'edge',
    source: '',
  });

  programs.push({
    name: 'whitespace_only',
    category: 'edge',
    source: '   \n\t\n  \n\t\t  \n',
  });

  programs.push({
    name: 'single_newline',
    category: 'edge',
    source: '\n',
  });

  // Category 16: All contextual identifiers
  programs.push({
    name: 'contextual_identifiers',
    category: 'keywords',
    source: `
actor MyActor {
    let count: i32;

    mailbox {
        overflow drop_new;
        overflow drop_old;
        overflow block;
        overflow fail;
        overflow coalesce;
        fallback default_handler;
    }

    receive fn tick(self, mut x: i32) -> i32 {
        self.count = self.count + x;
        self.count
    }
}

supervisor MySup {
    child actor: MyActor;
    restart permanent;
    window 60s;
    max_restarts 5;
}

#[export]
fn exported() -> i32 { 42 }

wire Config(json) {
    repeated items: string = 1;
}

wire Config2(yaml) {
    optional name: string = 1;
}
`,
  });

  // Category 17: Generics / complex type expressions
  programs.push({
    name: 'complex_types',
    category: 'types',
    source: `
fn generic_fn<T, U>(x: T, y: U) -> Result<T, U> where T: Send, U: Copy {
    Ok(x)
}

fn nested_generics() {
    let x: Vec<HashMap<string, Vec<Option<i32>>>> = Vec::new();
    let y: Result<Option<Vec<i32>>, string> = Ok(Some(Vec::new()));
}

type Generic<T> {
    value: T;
    items: Vec<T>;
}

trait Transform<T, U> {
    fn apply(self: Self, input: T) -> U;
}

impl Transform<i32, string> for Generic<i32> {
    fn apply(self: Generic<i32>, input: i32) -> string {
        f"{input}"
    }
}
`,
  });

  // Generate random variations to fill up to requested count
  const templates = [
    // Random function with various expressions
    (i) => `fn rand_${i}(x: i32) -> i32 {
    let a = x + ${Math.floor(Math.random() * 100)};
    let b = if a > ${Math.floor(Math.random() * 50)} { a * 2 } else { a - 1 };
    match b % 3 {
        0 => b + 1,
        1 => b * 2,
        _ => b,
    }
}
`,
    // Random actor
    (i) => `actor Fuzz${i} {
    let val: i32;
    receive fn set(n: i32) -> i32 { self.val = n; self.val }
    receive fn get() -> i32 { self.val }
}
`,
    // Random struct + impl
    (i) => `type Thing${i} { a: i32; b: string; }
impl Thing${i} {
    fn new(a: i32) -> Thing${i} { Thing${i} { a: a, b: f"thing{a}" } }
}
`,
    // Random generator
    (i) => `gen fn series_${i}() -> i32 {
    var i = 0;
    while i < ${Math.floor(Math.random() * 10) + 1} { yield i; i = i + 1; }
}
`,
    // Random bad input — spliced keywords
    (i) => {
      const kws = syntaxData.all_keywords;
      const shuffled = kws.sort(() => Math.random() - 0.5).slice(0, 8);
      return shuffled.join(' ') + ' { ' + shuffled.reverse().join('; ') + ' }';
    },
    // Random bad input — partial expressions
    (i) => `fn partial_${i}() { let x = ${Math.random() < 0.5 ? '' : '42 +'}`,
  ];

  while (programs.length < count) {
    const idx = programs.length;
    const template = templates[idx % templates.length];
    programs.push({
      name: `generated_${idx}`,
      category: idx % templates.length < 4 ? 'generated_valid' : 'generated_bad',
      source: template(idx),
    });
  }

  return programs;
}

// ── TextMate tokenizer ───────────────────────────────────────────────

async function loadTextMateGrammar() {
  const wasmPath = join(dirname(fileURLToPath(import.meta.url)), 'node_modules/vscode-oniguruma/release/onig.wasm');
  const wasmBin = readFileSync(wasmPath);
  await loadWASM(wasmBin.buffer);

  const tmGrammarRaw = readFileSync(TM_GRAMMAR_PATH, 'utf-8');
  const tmGrammar = JSON.parse(tmGrammarRaw);

  const registry = new Registry({
    onigLib: Promise.resolve({ createOnigScanner, createOnigString }),
    loadGrammar: async () => tmGrammar,
  });

  const grammar = await registry.loadGrammar('source.hew');
  if (!grammar) throw new Error('Failed to load TextMate grammar');
  return grammar;
}

function tokenizeWithTextMate(grammar, source) {
  const lines = source.split('\n');
  let ruleStack = INITIAL;
  const allTokens = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const result = grammar.tokenizeLine(line, ruleStack);
    ruleStack = result.ruleStack;

    for (const token of result.tokens) {
      allTokens.push({
        line: i + 1,
        startIndex: token.startIndex,
        endIndex: token.endIndex,
        scopes: token.scopes,
        text: line.slice(token.startIndex, token.endIndex),
      });
    }
  }

  return allTokens;
}

// ── tree-sitter parser ───────────────────────────────────────────────

function parseWithTreeSitter(source, timeout) {
  const tmpFile = join(__dirname, '.fuzz-tmp.hew');
  writeFileSync(tmpFile, source);
  try {
    const output = execFileSync(
      'npx',
      ['tree-sitter', 'parse', tmpFile, '--quiet'],
      { cwd: TREE_SITTER_DIR, timeout, encoding: 'utf-8', stdio: ['pipe', 'pipe', 'pipe'] }
    );
    const errorCount = (output.match(/\(ERROR\)/g) || []).length;
    const missingCount = (output.match(/\(MISSING\)/g) || []).length;
    return { success: true, errors: errorCount, missing: missingCount, output };
  } catch (err) {
    if (err.killed) {
      return { success: false, reason: 'timeout', errors: 0, missing: 0, output: '' };
    }
    // tree-sitter parse returns non-zero for parse errors but still produces output
    const output = (err.stdout || '') + (err.stderr || '');
    const errorCount = (output.match(/\(ERROR\)/g) || []).length;
    const missingCount = (output.match(/\(MISSING\)/g) || []).length;
    return { success: true, errors: errorCount, missing: missingCount, output };
  } finally {
    try { rmSync(tmpFile); } catch {}
  }
}

// ── Analysis ─────────────────────────────────────────────────────────

function analyseTextMateTokens(tokens, source) {
  const issues = [];

  // Check that known keywords get keyword scopes
  const allKws = new Set(syntaxData.all_keywords);
  const allTypes = new Set([
    ...syntaxData.types.integer,
    ...syntaxData.types.float,
    ...syntaxData.types.primitive,
    ...syntaxData.types.collections,
    ...syntaxData.types.option_result,
    ...syntaxData.types.concurrency,
    ...syntaxData.types.marker_traits,
    ...syntaxData.types.other,
  ]);

  const keywordsFound = new Set();
  const keywordsMissed = new Set();
  const typesFound = new Set();
  const typesMissed = new Set();

  let untokenizedLines = 0;
  const lines = source.split('\n');

  for (const token of tokens) {
    const text = token.text.trim();
    const scopeStr = token.scopes.join(' ');

    // Check keyword highlighting — keywords can be highlighted under various
    // scopes depending on context (keyword.*, constant.language.*, entity.name.*,
    // storage.*, variable.language.*). The key check is that they get ANY
    // non-source scope, meaning the grammar recognized them.
    if (allKws.has(text)) {
      const hasScope = scopeStr.includes('keyword') || scopeStr.includes('constant.language')
        || scopeStr.includes('entity.name') || scopeStr.includes('storage')
        || scopeStr.includes('variable.language');
      if (hasScope) {
        keywordsFound.add(text);
      } else {
        const inStringOrComment = scopeStr.includes('string') || scopeStr.includes('comment');
        if (!inStringOrComment) {
          keywordsMissed.add(text);
        }
      }
    }

    // Check type highlighting — types can appear under storage.type.*,
    // entity.name.type.*, constant.language.* (for Option/Result variants),
    // or support.type.*
    if (allTypes.has(text)) {
      const hasTypeScope = scopeStr.includes('storage.type') || scopeStr.includes('entity.name.type')
        || scopeStr.includes('constant.language') || scopeStr.includes('support.type');
      if (hasTypeScope) {
        typesFound.add(text);
      } else {
        const inStringOrComment = scopeStr.includes('string') || scopeStr.includes('comment');
        if (!inStringOrComment) {
          typesMissed.add(text);
        }
      }
    }
  }

  // Check for lines where everything is just source.hew (no highlighting at all)
  let lineTokenMap = {};
  for (const token of tokens) {
    if (!lineTokenMap[token.line]) lineTokenMap[token.line] = [];
    lineTokenMap[token.line].push(token);
  }

  for (let i = 1; i <= lines.length; i++) {
    const line = lines[i - 1];
    if (!line.trim()) continue; // blank lines are fine
    const lineTokens = lineTokenMap[i] || [];
    const allSourceOnly = lineTokens.every(t =>
      t.scopes.length === 1 && t.scopes[0] === 'source.hew'
    );
    if (allSourceOnly && line.trim().length > 0) {
      // Check if the line has any recognizable tokens (keywords, types, etc.)
      const words = line.trim().split(/\s+/);
      const hasKnownToken = words.some(w => allKws.has(w) || allTypes.has(w));
      if (hasKnownToken) {
        untokenizedLines++;
        issues.push(`Line ${i} has known tokens but no highlighting: ${line.trim().slice(0, 60)}`);
      }
    }
  }

  return { keywordsFound, keywordsMissed, typesFound, typesMissed, untokenizedLines, issues };
}

// ── Main ─────────────────────────────────────────────────────────────

async function main() {
  const opts = parseArgs();

  console.log(C.bold('\n  Hew Grammar Fuzzer\n'));

  // Load corpus
  let programs;
  if (opts.corpus) {
    const dir = resolve(opts.corpus);
    const files = readdirSync(dir).filter(f => f.endsWith('.hew'));
    programs = files.map(f => ({
      name: f.replace('.hew', ''),
      category: 'corpus',
      source: readFileSync(join(dir, f), 'utf-8'),
    }));
    console.log(`  Loaded ${programs.length} files from ${dir}\n`);
  } else {
    programs = generateCorpus(opts.generate);
    console.log(`  Generated ${programs.length} test programs\n`);
  }

  // Stats
  const stats = {
    total: programs.length,
    tmCrashes: 0,
    tmTimeouts: 0,
    tsCrashes: 0,
    tsTimeouts: 0,
    tsErrorPrograms: 0,
    allKeywordsFound: new Set(),
    allKeywordsMissed: new Set(),
    allTypesFound: new Set(),
    allTypesMissed: new Set(),
    untokenizedLines: 0,
    issues: [],
  };

  // Load TextMate grammar
  let grammar = null;
  if (!opts.treeSitterOnly) {
    process.stdout.write('  Loading TextMate grammar... ');
    try {
      grammar = await loadTextMateGrammar();
      console.log(C.green('OK'));
    } catch (err) {
      console.log(C.red(`FAILED: ${err.message}`));
      process.exit(1);
    }
  }

  // Check tree-sitter availability
  let tsAvailable = false;
  if (!opts.textmateOnly) {
    process.stdout.write('  Checking tree-sitter... ');
    try {
      execFileSync('npx', ['tree-sitter', '--version'], { cwd: TREE_SITTER_DIR, timeout: 10000, stdio: 'pipe' });
      tsAvailable = true;
      console.log(C.green('OK'));
    } catch {
      console.log(C.yellow('not available (skipping)'));
    }
  }

  console.log('');

  // Process each program
  for (let i = 0; i < programs.length; i++) {
    const prog = programs[i];
    const prefix = `  [${String(i + 1).padStart(String(programs.length).length)}/${programs.length}] ${prog.name}`;

    if (opts.verbose) {
      process.stdout.write(`${prefix} `);
    }

    // TextMate tokenization
    if (grammar) {
      try {
        const tokens = tokenizeWithTextMate(grammar, prog.source);
        const analysis = analyseTextMateTokens(tokens, prog.source);

        for (const kw of analysis.keywordsFound) stats.allKeywordsFound.add(kw);
        for (const kw of analysis.keywordsMissed) stats.allKeywordsMissed.add(kw);
        for (const t of analysis.typesFound) stats.allTypesFound.add(t);
        for (const t of analysis.typesMissed) stats.allTypesMissed.add(t);
        stats.untokenizedLines += analysis.untokenizedLines;

        for (const issue of analysis.issues) {
          stats.issues.push(`[${prog.name}] ${issue}`);
        }

        if (opts.verbose) process.stdout.write(C.green('TM:OK '));
      } catch (err) {
        stats.tmCrashes++;
        stats.issues.push(`[${prog.name}] TextMate CRASH: ${err.message}`);
        if (opts.verbose) process.stdout.write(C.red('TM:CRASH '));
      }
    }

    // tree-sitter parse
    if (tsAvailable) {
      const result = parseWithTreeSitter(prog.source, opts.timeout);
      if (!result.success) {
        if (result.reason === 'timeout') {
          stats.tsTimeouts++;
          stats.issues.push(`[${prog.name}] tree-sitter TIMEOUT`);
          if (opts.verbose) process.stdout.write(C.red('TS:TIMEOUT '));
        } else {
          stats.tsCrashes++;
          stats.issues.push(`[${prog.name}] tree-sitter CRASH`);
          if (opts.verbose) process.stdout.write(C.red('TS:CRASH '));
        }
      } else {
        if (result.errors > 0 && prog.category !== 'bad_input' && prog.category !== 'generated_bad') {
          stats.tsErrorPrograms++;
          if (opts.verbose) process.stdout.write(C.yellow(`TS:${result.errors}ERR `));
        } else {
          if (opts.verbose) process.stdout.write(C.green('TS:OK '));
        }
      }
    }

    if (opts.verbose) console.log('');
  }

  // ── Report ─────────────────────────────────────────────────────────

  console.log(C.bold('\n  ═══ Results ═══\n'));

  console.log(`  Programs tested:        ${stats.total}`);

  if (grammar) {
    console.log(`  TextMate crashes:       ${stats.tmCrashes === 0 ? C.green('0') : C.red(String(stats.tmCrashes))}`);
    console.log(`  Untokenized lines:      ${stats.untokenizedLines === 0 ? C.green('0') : C.yellow(String(stats.untokenizedLines))}`);
    console.log(`  Keywords found:         ${C.green(String(stats.allKeywordsFound.size))}/${syntaxData.all_keywords.length}`);
    // Only report keywords that were NEVER found highlighted anywhere
    const trulyMissedKw = [...stats.allKeywordsMissed].filter(k => !stats.allKeywordsFound.has(k));
    const neverSeen = syntaxData.all_keywords.filter(k => !stats.allKeywordsFound.has(k) && !stats.allKeywordsMissed.has(k));
    if (trulyMissedKw.length > 0) {
      console.log(`  Keywords unhighlighted:  ${C.yellow(trulyMissedKw.join(', '))}`);
    }
    if (neverSeen.length > 0) {
      console.log(`  Keywords not tested:    ${C.dim(neverSeen.join(', '))}`);
    }

    const totalTypes = Object.values(syntaxData.types).flat().length;
    console.log(`  Types found:            ${C.green(String(stats.allTypesFound.size))}/${totalTypes}`);
    const trulyMissedTy = [...stats.allTypesMissed].filter(t => !stats.allTypesFound.has(t));
    const neverSeenTy = Object.values(syntaxData.types).flat().filter(t => !stats.allTypesFound.has(t) && !stats.allTypesMissed.has(t));
    if (trulyMissedTy.length > 0) {
      console.log(`  Types unhighlighted:    ${C.yellow(trulyMissedTy.join(', '))}`);
    }
    if (neverSeenTy.length > 0) {
      console.log(`  Types not tested:       ${C.dim(neverSeenTy.join(', '))}`);
    }
  }

  if (tsAvailable) {
    console.log(`  tree-sitter crashes:    ${stats.tsCrashes === 0 ? C.green('0') : C.red(String(stats.tsCrashes))}`);
    console.log(`  tree-sitter timeouts:   ${stats.tsTimeouts === 0 ? C.green('0') : C.red(String(stats.tsTimeouts))}`);
    console.log(`  Valid programs w/errors: ${stats.tsErrorPrograms === 0 ? C.green('0') : C.yellow(String(stats.tsErrorPrograms))}`);
  }

  if (stats.issues.length > 0) {
    console.log(C.bold('\n  ─── Issues ───\n'));
    for (const issue of stats.issues.slice(0, 30)) {
      console.log(`  ${C.yellow('⚠')} ${issue}`);
    }
    if (stats.issues.length > 30) {
      console.log(C.dim(`  ... and ${stats.issues.length - 30} more`));
    }
  }

  // Exit code
  const failed = stats.tmCrashes > 0 || stats.tsCrashes > 0 || stats.tsTimeouts > 0;
  console.log('');
  if (failed) {
    console.log(C.red('  FAILED — grammar crashes or timeouts detected\n'));
    process.exit(1);
  } else {
    console.log(C.green('  PASSED — no crashes or timeouts\n'));
    process.exit(0);
  }
}

main().catch(err => {
  console.error(C.red(`Fatal: ${err.message}`));
  process.exit(1);
});
