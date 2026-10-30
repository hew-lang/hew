//! Structure-aware generator of small, valid, terminating Hew programs for the
//! native-versus-sandbox differential oracle (`scripts/fuzz/run-parity.py`).
//!
//! A program is a list of independent cases, each a function `case_N` called
//! from `main` in order. Every case carries a model of its standard output, so
//! the oracle also catches a defect that every engine shares. At most the last
//! case fails, and the model names the failure it expects.
//!
//! Programs avoid nondeterminism: no wall clock, randomness, addresses or
//! hash-map iteration, and actor output only through awaited replies.

use std::fmt::Write as _;

use arbitrary::{Result, Unstructured};

/// A rendered program and its model.
pub struct Program {
    pub source: String,
    pub stdout: String,
    /// The failure kind the last case raises, as the runtime names it.
    pub trap: Option<&'static str>,
}

struct Case {
    items: String,
    body: String,
    stdout: String,
    trap: Option<&'static str>,
}

/// Generate a program; `only` keeps a subset of the cases (for minimisation).
///
/// # Errors
/// Never in practice: an exhausted input yields the smallest choices.
pub fn generate(u: &mut Unstructured<'_>, only: Option<&[usize]>) -> Result<Program> {
    let count = u.int_in_range(1..=6usize)?;
    let mut cases = Vec::with_capacity(count + 1);
    for index in 0..count {
        cases.push(case(u, index, false)?);
    }
    if u.ratio(1u8, 4u8)? {
        cases.push(case(u, count, true)?);
    }
    let mut source = String::new();
    let mut main = String::from("fn main() {\n");
    let mut stdout = String::new();
    let mut trap = None;
    for (index, case) in cases.into_iter().enumerate() {
        if only.is_some_and(|keep| !keep.contains(&index)) {
            continue;
        }
        source.push_str(&case.items);
        let _ = writeln!(source, "fn case_{index}() {{\n{}}}\n", case.body);
        let _ = writeln!(main, "    case_{index}();");
        stdout.push_str(&case.stdout);
        trap = case.trap;
    }
    main.push_str("}\n");
    source.push_str(&main);
    Ok(Program {
        source,
        stdout,
        trap,
    })
}

fn case(u: &mut Unstructured<'_>, index: usize, failing: bool) -> Result<Case> {
    let mut case = Case {
        items: String::new(),
        body: String::new(),
        stdout: String::new(),
        trap: None,
    };
    let mut out = Out {
        body: &mut case.body,
        stdout: &mut case.stdout,
    };
    if failing {
        case.trap = Some(failing_case(u, index, &mut out, &mut case.items)?);
        return Ok(case);
    }
    match u.int_in_range(0..=13u8)? {
        0 | 1 => integers(u, &mut out, 4)?,
        11 => floats(u, &mut out)?,
        12 => results(u, index, &mut out, &mut case.items)?,
        13 => copies(u, index, &mut out, &mut case.items)?,
        2 => strings(u, &mut out)?,
        3 => collections(u, &mut out)?,
        4 => records(u, index, &mut out, &mut case.items)?,
        5 => enums(u, index, &mut out, &mut case.items)?,
        6 => closures(u, index, &mut out, &mut case.items)?,
        7 => generators(u, index, &mut out, &mut case.items)?,
        8 => actors(u, index, &mut out, &mut case.items)?,
        9 => supervisors(u, index, &mut out, &mut case.items)?,
        _ => timers(u, index, &mut out, &mut case.items)?,
    }
    Ok(case)
}

/// The statements of a case and the output they print.
struct Out<'a> {
    body: &'a mut String,
    stdout: &'a mut String,
}

impl Out<'_> {
    fn stmt(&mut self, line: &str) {
        let _ = writeln!(self.body, "    {line}");
    }

    /// `println(expr)` whose model prints `value`.
    fn print(&mut self, expr: &str, value: impl std::fmt::Display) {
        self.stmt(&format!("println({expr});"));
        let _ = writeln!(self.stdout, "{value}");
    }
}

// ---------------------------------------------------------------------------
// Integers
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum IntTy {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

const INT_TYS: [IntTy; 8] = [
    IntTy::I8,
    IntTy::I16,
    IntTy::I32,
    IntTy::I64,
    IntTy::U8,
    IntTy::U16,
    IntTy::U32,
    IntTy::U64,
];

impl IntTy {
    fn name(self) -> &'static str {
        match self {
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
        }
    }

    fn bits(self) -> u32 {
        match self {
            Self::I8 | Self::U8 => 8,
            Self::I16 | Self::U16 => 16,
            Self::I32 | Self::U32 => 32,
            Self::I64 | Self::U64 => 64,
        }
    }

    fn signed(self) -> bool {
        matches!(self, Self::I8 | Self::I16 | Self::I32 | Self::I64)
    }

    fn min(self) -> i128 {
        if self.signed() {
            -(1i128 << (self.bits() - 1))
        } else {
            0
        }
    }

    fn max(self) -> i128 {
        if self.signed() {
            (1i128 << (self.bits() - 1)) - 1
        } else {
            (1i128 << self.bits()) - 1
        }
    }

    fn wrap(self, value: i128) -> i128 {
        let modulus = 1i128 << self.bits();
        let low = value.rem_euclid(modulus);
        if self.signed() && low > self.max() {
            low - modulus
        } else {
            low
        }
    }

    fn checked(self, value: i128) -> std::result::Result<i128, &'static str> {
        if value < self.min() || value > self.max() {
            Err(OVERFLOW)
        } else {
            Ok(value)
        }
    }

    /// A literal spelling of `value`; the i64 minimum has none.
    fn literal(self, value: i128) -> String {
        if self == Self::I64 && value == self.min() {
            "(-9223372036854775807 - 1)".into()
        } else if value < 0 {
            format!("({value})")
        } else {
            value.to_string()
        }
    }

    fn pick_value(self, u: &mut Unstructured<'_>) -> Result<i128> {
        Ok(match u.int_in_range(0..=9u8)? {
            0 => 0,
            1 => 1,
            2 => self.max(),
            3 => self.min(),
            4 => self.max() - 1,
            5 if self.signed() => -1,
            6 if self.signed() => self.min() + 1,
            _ => {
                let small = i128::from(u.int_in_range(0..=300u16)?);
                let value = if self.signed() && u.arbitrary()? {
                    -small
                } else {
                    small
                };
                value.clamp(self.min(), self.max())
            }
        })
    }
}

const OVERFLOW: &str = "IntegerOverflow";
const DIV_ZERO: &str = "DivideByZero";
const MIN_DIV: &str = "SignedMinDivNegOne";

#[derive(Clone, Copy)]
enum BinOp {
    Add,
    Sub,
    Mul,
    WAdd,
    WSub,
    WMul,
    Div,
    Rem,
    And,
    Or,
    Xor,
}

#[derive(Clone, Copy)]
enum Method {
    WrappingAdd,
    WrappingSub,
    WrappingMul,
    SaturatingAdd,
    SaturatingSub,
    SaturatingMul,
    CheckedAdd(i128),
    CheckedSub(i128),
    CheckedMul(i128),
}

enum Expr {
    Var(usize),
    Lit(i128),
    Bin(BinOp, Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Method(Method, Box<Expr>, Box<Expr>),
    Shift(bool, Box<Expr>, u32),
}

type Eval = std::result::Result<i128, &'static str>;

impl Expr {
    fn gen(u: &mut Unstructured<'_>, ty: IntTy, vars: usize, depth: u8) -> Result<Self> {
        if depth == 0 || u.ratio(1u8, 3u8)? {
            return Ok(if vars > 0 && u.ratio(3u8, 4u8)? {
                Self::Var(u.int_in_range(0..=vars - 1)?)
            } else {
                Self::Lit(ty.pick_value(u)?)
            });
        }
        let left = Box::new(Self::gen(u, ty, vars, depth - 1)?);
        Ok(match u.int_in_range(0..=13u8)? {
            0 if ty.signed() => Self::Neg(left),
            1 => Self::Shift(u.arbitrary()?, left, u.int_in_range(0..=ty.bits() - 1)?),
            2 | 3 => {
                let right = Box::new(Self::gen(u, ty, vars, depth - 1)?);
                let fallback = ty.pick_value(u)?;
                let method = *u.choose(&[
                    Method::WrappingAdd,
                    Method::WrappingSub,
                    Method::WrappingMul,
                    Method::SaturatingAdd,
                    Method::SaturatingSub,
                    Method::SaturatingMul,
                    Method::CheckedAdd(fallback),
                    Method::CheckedSub(fallback),
                    Method::CheckedMul(fallback),
                ])?;
                Self::Method(method, left, right)
            }
            _ => {
                let right = Box::new(Self::gen(u, ty, vars, depth - 1)?);
                let op = *u.choose(&[
                    BinOp::Add,
                    BinOp::Sub,
                    BinOp::Mul,
                    BinOp::WAdd,
                    BinOp::WSub,
                    BinOp::WMul,
                    BinOp::Div,
                    BinOp::Rem,
                    BinOp::And,
                    BinOp::Or,
                    BinOp::Xor,
                ])?;
                Self::Bin(op, left, right)
            }
        })
    }

    fn eval(&self, ty: IntTy, vars: &[i128]) -> Eval {
        Ok(match self {
            Self::Var(index) => vars[*index],
            Self::Lit(value) => *value,
            Self::Neg(inner) => ty.checked(-inner.eval(ty, vars)?)?,
            Self::Shift(left, inner, amount) => {
                let value = inner.eval(ty, vars)?;
                if *left {
                    ty.wrap(value << amount)
                } else if ty.signed() {
                    value >> amount
                } else {
                    // Unsigned values are non-negative in the model.
                    value >> amount
                }
            }
            Self::Bin(op, a, b) => {
                let a = a.eval(ty, vars)?;
                let b = b.eval(ty, vars)?;
                match op {
                    BinOp::Add => ty.checked(a + b)?,
                    BinOp::Sub => ty.checked(a - b)?,
                    BinOp::Mul => ty.checked(a * b)?,
                    BinOp::WAdd => ty.wrap(a + b),
                    BinOp::WSub => ty.wrap(a - b),
                    BinOp::WMul => ty.wrap(a * b),
                    BinOp::Div | BinOp::Rem => {
                        if b == 0 {
                            return Err(DIV_ZERO);
                        }
                        if ty.signed() && a == ty.min() && b == -1 {
                            return Err(MIN_DIV);
                        }
                        if matches!(op, BinOp::Div) {
                            a / b
                        } else {
                            a % b
                        }
                    }
                    BinOp::And => ty.wrap(a & b),
                    BinOp::Or => ty.wrap(a | b),
                    BinOp::Xor => ty.wrap(a ^ b),
                }
            }
            Self::Method(method, a, b) => {
                let a = a.eval(ty, vars)?;
                let b = b.eval(ty, vars)?;
                let clamp = |value: i128| value.clamp(ty.min(), ty.max());
                let checked = |value: i128, fallback: i128| ty.checked(value).unwrap_or(fallback);
                match method {
                    Method::WrappingAdd => ty.wrap(a + b),
                    Method::WrappingSub => ty.wrap(a - b),
                    Method::WrappingMul => ty.wrap(a * b),
                    Method::SaturatingAdd => clamp(a + b),
                    Method::SaturatingSub => clamp(a - b),
                    Method::SaturatingMul => clamp(a * b),
                    Method::CheckedAdd(fallback) => checked(a + b, *fallback),
                    Method::CheckedSub(fallback) => checked(a - b, *fallback),
                    Method::CheckedMul(fallback) => checked(a * b, *fallback),
                }
            }
        })
    }

    /// The expression's spelling. Every literal becomes a typed binding in
    /// `lets`, so no operand's type depends on literal defaulting.
    fn render(&self, ty: IntTy, lets: &mut Lets<'_>) -> String {
        match self {
            Self::Var(index) => format!("v{index}"),
            Self::Lit(value) => lets.bind(ty, *value),
            Self::Neg(inner) => format!("(-{})", inner.render(ty, lets)),
            Self::Shift(left, inner, amount) => {
                let op = if *left { "<<" } else { ">>" };
                format!("({} {op} {amount})", inner.render(ty, lets))
            }
            Self::Bin(op, a, b) => {
                let op = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Mul => "*",
                    BinOp::WAdd => "&+",
                    BinOp::WSub => "&-",
                    BinOp::WMul => "&*",
                    BinOp::Div => "/",
                    BinOp::Rem => "%",
                    BinOp::And => "&",
                    BinOp::Or => "|",
                    BinOp::Xor => "^",
                };
                format!("({} {op} {})", a.render(ty, lets), b.render(ty, lets))
            }
            Self::Method(method, a, b) => {
                let (a, b) = (a.render(ty, lets), b.render(ty, lets));
                let call = |name: &str| format!("{a}.{name}({b})");
                let mut fallback = |name: &str, value: i128| {
                    format!(
                        "(match {a}.{name}({b}) {{ .Some(n) => n, .None => {} }})",
                        lets.bind(ty, value)
                    )
                };
                match method {
                    Method::WrappingAdd => call("wrapping_add"),
                    Method::WrappingSub => call("wrapping_sub"),
                    Method::WrappingMul => call("wrapping_mul"),
                    Method::SaturatingAdd => call("saturating_add"),
                    Method::SaturatingSub => call("saturating_sub"),
                    Method::SaturatingMul => call("saturating_mul"),
                    Method::CheckedAdd(value) => fallback("checked_add", *value),
                    Method::CheckedSub(value) => fallback("checked_sub", *value),
                    Method::CheckedMul(value) => fallback("checked_mul", *value),
                }
            }
        }
    }
}

/// Typed bindings for the literals of the statement being rendered.
struct Lets<'a> {
    out: &'a mut String,
    next: &'a mut usize,
}

impl Lets<'_> {
    fn bind(&mut self, ty: IntTy, value: i128) -> String {
        let name = format!("l{}", self.next);
        *self.next += 1;
        let _ = writeln!(
            self.out,
            "    let {name}: {} = {};",
            ty.name(),
            ty.literal(value)
        );
        name
    }
}

/// Spell `expr` as a typed binding and return its name.
fn bind_expr(out: &mut Out<'_>, next: &mut usize, ty: IntTy, expr: &Expr) -> String {
    let mut lets = String::new();
    let text = expr.render(
        ty,
        &mut Lets {
            out: &mut lets,
            next,
        },
    );
    out.body.push_str(&lets);
    let name = format!("r{next}");
    *next += 1;
    out.stmt(&format!("let {name}: {} = {text};", ty.name()));
    name
}

/// Typed integer variables and printed expressions over them. Expressions
/// that fail are replaced by their wrapping form in a non-failing case.
fn integers(u: &mut Unstructured<'_>, out: &mut Out<'_>, count: usize) -> Result<()> {
    let ty = *u.choose(&INT_TYS)?;
    let vars = int_vars(u, ty, out)?;
    let mut next = 0;
    for _ in 0..u.int_in_range(1..=count)? {
        let expr = Expr::gen(u, ty, vars.len(), 3)?;
        let Ok(value) = expr.eval(ty, &vars) else {
            continue;
        };
        match u.int_in_range(0..=5u8)? {
            0 => {
                let target = *u.choose(&INT_TYS)?;
                let name = bind_expr(out, &mut next, ty, &expr);
                out.print(&format!("{name} as {}", target.name()), target.wrap(value));
            }
            1 => {
                let other = Expr::gen(u, ty, vars.len(), 1)?;
                let Ok(right) = other.eval(ty, &vars) else {
                    continue;
                };
                let left = bind_expr(out, &mut next, ty, &expr);
                let right_name = bind_expr(out, &mut next, ty, &other);
                out.print(&format!("{left} < {right_name}"), value < right);
            }
            _ => {
                let name = bind_expr(out, &mut next, ty, &expr);
                out.print(&name, value);
            }
        }
    }
    Ok(())
}

fn int_vars(u: &mut Unstructured<'_>, ty: IntTy, out: &mut Out<'_>) -> Result<Vec<i128>> {
    let mut vars = Vec::new();
    for index in 0..u.int_in_range(1..=4usize)? {
        let value = ty.pick_value(u)?;
        out.stmt(&format!(
            "let v{index}: {} = {};",
            ty.name(),
            ty.literal(value)
        ));
        vars.push(value);
    }
    Ok(vars)
}

// ---------------------------------------------------------------------------
// Floats
// ---------------------------------------------------------------------------

/// C's `%g`: six significant digits, trailing zeros removed. Hew prints `f64`
/// this way on every engine.
fn format_g(value: f64) -> String {
    if value == 0.0 {
        return if value.is_sign_negative() {
            "-0".into()
        } else {
            "0".into()
        };
    }
    let scientific = format!("{value:.5e}");
    let (mantissa, exponent) = scientific.split_once('e').expect("exponent");
    let exponent: i32 = exponent.parse().expect("exponent value");
    let trim = |text: &str| {
        if text.contains('.') {
            text.trim_end_matches('0').trim_end_matches('.').to_string()
        } else {
            text.to_string()
        }
    };
    if !(-4..6).contains(&exponent) {
        let sign = if exponent < 0 { '-' } else { '+' };
        format!("{}e{sign}{:02}", trim(mantissa), exponent.abs())
    } else {
        let decimals = usize::try_from(5 - exponent).unwrap_or(0);
        trim(&format!("{value:.decimals$}"))
    }
}

const FLOATS: [f64; 10] = [
    0.0,
    0.1,
    0.5,
    2.5,
    -3.75,
    1e10,
    123_456.789,
    1e-5,
    -0.2,
    7.0,
];

fn float_literal(value: f64) -> String {
    let mut text = format!("{value}");
    if !text.contains('.') {
        text.push_str(".0");
    }
    if value < 0.0 {
        format!("({text})")
    } else {
        text
    }
}

fn floats(u: &mut Unstructured<'_>, out: &mut Out<'_>) -> Result<()> {
    let mut values = Vec::new();
    for index in 0..u.int_in_range(1..=3usize)? {
        let value = *u.choose(&FLOATS)?;
        out.stmt(&format!("let f{index}: f64 = {};", float_literal(value)));
        values.push(value);
    }
    for step in 0..u.int_in_range(1..=5usize)? {
        let a = u.int_in_range(0..=values.len() - 1)?;
        let b = u.int_in_range(0..=values.len() - 1)?;
        let (x, y) = (values[a], values[b]);
        let (expr, value) = match u.int_in_range(0..=6u8)? {
            0 => (format!("f{a} + f{b}"), x + y),
            1 => (format!("f{a} - f{b}"), x - y),
            2 => (format!("f{a} * f{b}"), x * y),
            3 if y != 0.0 => (format!("f{a} / f{b}"), x / y),
            4 => {
                // Float-to-integer conversion truncates toward zero.
                out.print(&format!("f{a} as i64"), x.trunc() as i64);
                continue;
            }
            5 => {
                out.print(&format!("f{a} < f{b}"), x < y);
                continue;
            }
            _ => (format!("f{a} * 3.0"), x * 3.0),
        };
        let next = values.len();
        out.stmt(&format!("let f{next} = {expr};"));
        values.push(value);
        if step % 2 == 0 {
            out.print(
                &format!("f\"<{{f{next}}}>\""),
                format!("<{}>", format_g(value)),
            );
        } else {
            out.print(&format!("f{next}"), format_g(value));
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Results and value copies
// ---------------------------------------------------------------------------

fn results(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<()> {
    let limit = u.int_in_range(0..=20i64)?;
    let _ = write!(
        items,
        "fn check{index}(n: i64) -> Result<i64, string> {{\n    if n > {limit} {{\n        \
         .Err(f\"too big {{n}}\")\n    }} else {{\n        .Ok(n * 2)\n    }}\n}}\n\n\
         fn sum{index}(a: i64, b: i64) -> i64 fails string {{\n    let x = check{index}(a)?;\n    \
         let y = check{index}(b)?;\n    x + y\n}}\n\n\
         fn half{index}(n: i64) -> Option<i64> {{\n    if n % 2 == 0 {{ .Some(n / 2) }} else {{ .None }}\n}}\n\n"
    );
    let check = |n: i64| {
        if n > limit {
            Err(format!("too big {n}"))
        } else {
            Ok(n * 2)
        }
    };
    for _ in 0..u.int_in_range(1..=5usize)? {
        let a = u.int_in_range(-5..=30i64)?;
        let b = u.int_in_range(-5..=30i64)?;
        let (al, bl) = (IntTy::I64.literal(a.into()), IntTy::I64.literal(b.into()));
        match u.int_in_range(0..=2u8)? {
            0 => out.print(
                &format!(
                    "match sum{index}({al}, {bl}) {{ .Ok(n) => f\"ok {{n}}\", .Err(e) => e }}"
                ),
                match check(a).and_then(|x| check(b).map(|y| x + y)) {
                    Ok(n) => format!("ok {n}"),
                    Err(e) => e,
                },
            ),
            1 => out.print(
                &format!("half{index}({al}) ?? -1"),
                if a % 2 == 0 { a / 2 } else { -1 },
            ),
            _ => out.print(&format!("check{index}({al}).is_ok()"), check(a).is_ok()),
        }
    }
    Ok(())
}

/// Collections and records are values: a copy changes independently of the
/// binding it came from.
fn copies(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<()> {
    let _ = write!(
        items,
        "type Cell{index} {{\n    id: i64;\n    tags: Vec<string>;\n}}\n\n"
    );
    let mut cells: Vec<(i64, Vec<String>)> = Vec::new();
    out.stmt(&format!("var cells: Vec<Cell{index}> = Vec.new();"));
    for id in 0..u.int_in_range(1..=3i64)? {
        let tag = (*u.choose(&WORDS)?).to_string();
        out.stmt(&format!(
            "cells.push(Cell{index} {{ id: {id}, tags: [{}] }});",
            quote(&tag)
        ));
        cells.push((id, vec![tag]));
    }
    let mut copy = cells.clone();
    out.stmt("var copy = cells.clone();");
    for _ in 0..u.int_in_range(1..=4usize)? {
        let at = u.int_in_range(0..=copy.len() - 1)?;
        match u.int_in_range(0..=2u8)? {
            0 => {
                let word = *u.choose(&WORDS)?;
                out.stmt(&format!("copy[{at}].tags.push({});", quote(word)));
                copy[at].1.push(word.to_string());
            }
            1 => {
                let id = u.int_in_range(10..=99i64)?;
                out.stmt(&format!("copy[{at}].id = {id};"));
                copy[at].0 = id;
            }
            _ => {
                out.stmt(&format!(
                    "copy.push(Cell{index} {{ id: {}, tags: [] }});",
                    copy.len()
                ));
                copy.push((copy.len() as i64, Vec::new()));
            }
        }
    }
    for (name, model) in [("cells", &cells), ("copy", &copy)] {
        out.print(&format!("{name}.len()"), model.len());
        for (at, (id, tags)) in model.iter().enumerate() {
            out.print(
                &format!("f\"{{{name}[{at}].id}} {{{name}[{at}].tags.len()}}\""),
                format!("{id} {}", tags.len()),
            );
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Strings
// ---------------------------------------------------------------------------

const WORDS: [&str; 12] = [
    "",
    "a",
    "abc",
    "héllo",
    "日本語",
    "🎉x",
    "straße",
    "ΣΑΣ",
    "İi",
    "a b c",
    "ǅemal",
    "naïve café",
];
const NEEDLES: [&str; 6] = ["a", "é", "本", "ss", " ", "🎉"];

fn quote(text: &str) -> String {
    let mut out = String::from("\"");
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '{' => out.push_str("\\{"),
            '}' => out.push_str("\\}"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn char_index(text: &str, byte: usize) -> usize {
    text[..byte].chars().count()
}

fn strings(u: &mut Unstructured<'_>, out: &mut Out<'_>) -> Result<()> {
    let mut values: Vec<String> = Vec::new();
    for index in 0..u.int_in_range(1..=3usize)? {
        let text = (*u.choose(&WORDS)?).to_string();
        out.stmt(&format!("let s{index} = {};", quote(&text)));
        values.push(text);
    }
    for _ in 0..u.int_in_range(1..=6usize)? {
        let index = u.int_in_range(0..=values.len() - 1)?;
        let s = &values[index].clone();
        let name = format!("s{index}");
        let needle = *u.choose(&NEEDLES)?;
        match u.int_in_range(0..=15u8)? {
            0 => out.print(&format!("{name}.len()"), s.chars().count()),
            1 => out.print(&format!("{name}.byte_len()"), s.len()),
            2 => out.print(&format!("{name}.to_upper()"), s.to_uppercase()),
            3 => out.print(&format!("{name}.to_lower()"), s.to_lowercase()),
            4 => out.print(
                &format!("{name}.contains({})", quote(needle)),
                s.contains(needle),
            ),
            5 => out.print(
                &format!("{name}.starts_with({})", quote(needle)),
                s.starts_with(needle),
            ),
            6 => out.print(
                &format!("{name}.replace({}, \"<>\")", quote(needle)),
                s.replace(needle, "<>"),
            ),
            7 => {
                let times = u.int_in_range(0..=3usize)?;
                out.print(&format!("{name}.repeat({times})"), s.repeat(times));
            }
            8 => out.print(
                &format!("{name}.count({})", quote(needle)),
                s.matches(needle).count(),
            ),
            9 => out.print(
                &format!(
                    "match {name}.find({}) {{ .Some(i) => i, .None => -1 }}",
                    quote(needle)
                ),
                s.find(needle).map_or(-1, |byte| char_index(s, byte) as i64),
            ),
            10 => {
                let at = u.int_in_range(0..=4usize)?;
                out.print(
                    &format!(
                        "match {name}.get({at}) {{ .Some(c) => f\"<{{c}}>\", .None => \"none\" }}"
                    ),
                    s.chars()
                        .nth(at)
                        .map_or("none".to_string(), |c| format!("<{c}>")),
                );
            }
            11 => {
                let other = u.int_in_range(0..=values.len() - 1)?;
                let joined = format!("{s}{}", values[other]);
                let next = values.len();
                out.stmt(&format!("let s{next} = {name} + s{other};"));
                values.push(joined);
            }
            12 => {
                let n = u.int_in_range(-5..=5i64)?;
                out.print(
                    &format!("f\"[{{{name}}}|{{{n}}}|{{{name}.len()}}]\""),
                    format!("[{s}|{n}|{}]", s.chars().count()),
                );
            }
            13 => out.print(
                &format!("{name}.split({}).len()", quote(needle)),
                s.split(needle).count(),
            ),
            14 => out.print(&format!("{name} == {}", quote(needle)), s == needle),
            _ => out.print(
                &format!("{name}.ends_with({})", quote(needle)),
                s.ends_with(needle),
            ),
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Collections
// ---------------------------------------------------------------------------

fn collections(u: &mut Unstructured<'_>, out: &mut Out<'_>) -> Result<()> {
    match u.int_in_range(0..=2u8)? {
        0 => {
            let mut model: Vec<i64> = Vec::new();
            out.stmt("var v: Vec<i64> = Vec.new();");
            for _ in 0..u.int_in_range(1..=10usize)? {
                match u.int_in_range(0..=6u8)? {
                    0..=2 => {
                        let n = u.int_in_range(-50..=50i64)?;
                        out.stmt(&format!("v.push({});", IntTy::I64.literal(n.into())));
                        model.push(n);
                    }
                    3 if !model.is_empty() => {
                        let value = model.pop().unwrap_or_default();
                        out.print("v.pop()", value);
                    }
                    4 if !model.is_empty() => {
                        let at = u.int_in_range(0..=model.len() - 1)?;
                        let n = u.int_in_range(-9..=9i64)?;
                        out.stmt(&format!("v[{at}] = {};", IntTy::I64.literal(n.into())));
                        model[at] = n;
                    }
                    5 => {
                        let n = u.int_in_range(-50..=50i64)?;
                        out.print(
                            &format!("v.contains({})", IntTy::I64.literal(n.into())),
                            model.contains(&n),
                        );
                    }
                    _ => out.print("v.len()", model.len()),
                }
            }
            out.stmt("var total = 0;");
            out.stmt("for i in 0..v.len() { total = total + v[i] * (i + 1); }");
            let total: i64 = model
                .iter()
                .enumerate()
                .map(|(i, n)| n * (i as i64 + 1))
                .sum();
            out.print("total", total);
        }
        1 => {
            let mut model: std::collections::BTreeMap<&str, i64> =
                std::collections::BTreeMap::new();
            out.stmt("var m: HashMap<string, i64> = HashMap.new();");
            for _ in 0..u.int_in_range(1..=10usize)? {
                let key = *u.choose(&WORDS)?;
                match u.int_in_range(0..=4u8)? {
                    0 | 1 => {
                        let n = u.int_in_range(-99..=99i64)?;
                        out.stmt(&format!(
                            "m.insert({}, {});",
                            quote(key),
                            IntTy::I64.literal(n.into())
                        ));
                        model.insert(key, n);
                    }
                    2 => {
                        out.stmt(&format!("m.remove({});", quote(key)));
                        model.remove(key);
                    }
                    3 => out.print(
                        &format!(
                            "match m.get({}) {{ .Some(n) => n, .None => -1000 }}",
                            quote(key)
                        ),
                        model.get(key).copied().unwrap_or(-1000),
                    ),
                    _ => out.print(
                        &format!("m.contains_key({})", quote(key)),
                        model.contains_key(key),
                    ),
                }
            }
            out.print("m.len()", model.len());
        }
        _ => {
            let mut model = std::collections::BTreeSet::new();
            out.stmt("var set: HashSet<i64> = HashSet.new();");
            for _ in 0..u.int_in_range(1..=10usize)? {
                let n = u.int_in_range(-5..=5i64)?;
                let lit = IntTy::I64.literal(n.into());
                match u.int_in_range(0..=2u8)? {
                    0 => {
                        out.stmt(&format!("set.insert({lit});"));
                        model.insert(n);
                    }
                    1 => {
                        out.stmt(&format!("set.remove({lit});"));
                        model.remove(&n);
                    }
                    _ => out.print(&format!("set.contains({lit})"), model.contains(&n)),
                }
            }
            out.print("set.len()", model.len());
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Records and enums
// ---------------------------------------------------------------------------

fn records(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<()> {
    let ty = format!("Rec{index}");
    let _ = write!(
        items,
        "type {ty} {{\n    count: i64;\n    label: string;\n    on: bool;\n}}\n\n\
         impl {ty} {{\n    fn score(self) -> i64 {{\n        if self.on {{ self.count * 2 + self.label.len() }} else {{ self.count - self.label.len() }}\n    }}\n}}\n\n"
    );
    let score = |count: i64, label: &str, on: bool| {
        let len = label.chars().count() as i64;
        if on {
            count * 2 + len
        } else {
            count - len
        }
    };
    let mut count = u.int_in_range(-100..=100i64)?;
    let mut label = (*u.choose(&WORDS)?).to_string();
    let mut on: bool = u.arbitrary()?;
    out.stmt(&format!(
        "var r = {ty} {{ count: {}, label: {}, on: {on} }};",
        IntTy::I64.literal(count.into()),
        quote(&label)
    ));
    for step in 0..u.int_in_range(1..=6usize)? {
        match u.int_in_range(0..=5u8)? {
            0 => out.print("r.score()", score(count, &label, on)),
            1 => {
                let n = u.int_in_range(-20..=20i64)?;
                out.stmt(&format!(
                    "r.count = r.count + {};",
                    IntTy::I64.literal(n.into())
                ));
                count += n;
            }
            2 => {
                // A copy is a separate value: changing it leaves `r` alone.
                let n = u.int_in_range(0..=20i64)?;
                out.stmt(&format!("var copy{step} = r;\n    copy{step}.count = {n};"));
                out.print(&format!("copy{step}.score()"), score(n, &label, on));
                out.print("r.count", count);
            }
            3 => {
                let word = *u.choose(&WORDS)?;
                out.stmt(&format!("r = {ty} {{ label: {}, ..r }};", quote(word)));
                label = word.to_string();
            }
            4 => {
                out.stmt("r.on = !r.on;");
                on = !on;
            }
            _ => {
                out.stmt(&format!(
                    "let {ty} {{ count: c{step}, label: l{step}, on: o{step} }} = r.clone();"
                ));
                out.print(
                    &format!("f\"{{c{step}}}/{{l{step}}}/{{o{step}}}\""),
                    format!("{count}/{label}/{on}"),
                );
            }
        }
    }
    Ok(())
}

#[derive(Clone)]
enum Variant {
    Empty,
    One(i64),
    Named(i64, String),
}

fn enums(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<()> {
    let ty = format!("Shape{index}");
    let limit = u.int_in_range(-5..=5i64)?;
    let _ = write!(
        items,
        "enum {ty} {{\n    Empty;\n    One(i64);\n    Named(i64, string);\n}}\n\n\
         fn classify{index}(s: {ty}) -> string {{\n    match s {{\n        \
         {ty}.Empty => \"empty\",\n        \
         {ty}.One(n) if n > {lim} => f\"big {{n}}\",\n        \
         {ty}.One(n) => f\"small {{n}}\",\n        \
         {ty}.Named(n, name) => f\"{{name}}:{{n * 2}}\",\n    }}\n}}\n\n",
        lim = IntTy::I64.literal(limit.into())
    );
    for _ in 0..u.int_in_range(1..=5usize)? {
        let variant = match u.int_in_range(0..=2u8)? {
            0 => Variant::Empty,
            1 => Variant::One(u.int_in_range(-10..=10i64)?),
            _ => Variant::Named(
                u.int_in_range(-10..=10i64)?,
                (*u.choose(&WORDS)?).to_string(),
            ),
        };
        let (expr, value) = match &variant {
            Variant::Empty => (format!("{ty}.Empty"), "empty".to_string()),
            Variant::One(n) => (
                format!("{ty}.One({})", IntTy::I64.literal((*n).into())),
                if *n > limit {
                    format!("big {n}")
                } else {
                    format!("small {n}")
                },
            ),
            Variant::Named(n, name) => (
                format!(
                    "{ty}.Named({}, {})",
                    IntTy::I64.literal((*n).into()),
                    quote(name)
                ),
                format!("{name}:{}", n * 2),
            ),
        };
        out.print(&format!("classify{index}({expr})"), value);
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Closures and generators
// ---------------------------------------------------------------------------

fn closures(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<()> {
    let k = u.int_in_range(-9..=9i64)?;
    let _ = write!(
        items,
        "fn adder{index}(k: i64) -> fn(i64) -> i64 {{\n    |x: i64| x + k\n}}\n\n\
         fn twice{index}(f: fn(i64) -> i64, x: i64) -> i64 {{\n    f(f(x))\n}}\n\n"
    );
    let lit = IntTy::I64.literal(k.into());
    out.stmt(&format!("let add = adder{index}({lit});"));
    let scale = u.int_in_range(1..=4i64)?;
    out.stmt(&format!("let scale = {scale};"));
    out.stmt("let mul = |x: i64| x * scale;");
    let word = *u.choose(&WORDS)?;
    out.stmt(&format!("let word = {};", quote(word)));
    out.stmt("let tag = |n: i64| f\"{word}#{n}\";");
    out.stmt("var counter = 0;");
    out.stmt("var bump = || { counter = counter + 1; counter };");
    let mut bumps = 0;
    for _ in 0..u.int_in_range(1..=6usize)? {
        let x = u.int_in_range(-20..=20i64)?;
        let xl = IntTy::I64.literal(x.into());
        match u.int_in_range(0..=4u8)? {
            0 => out.print(&format!("add({xl})"), x + k),
            1 => out.print(&format!("twice{index}(add, {xl})"), x + 2 * k),
            2 => out.print(&format!("twice{index}(mul, {xl})"), x * scale * scale),
            3 => out.print(&format!("tag({xl})"), format!("{word}#{x}")),
            _ => {
                bumps += 1;
                out.print("bump()", bumps);
            }
        }
    }
    // The closure owns its copy of `counter`; the binding keeps its value.
    out.print("counter", 0);
    Ok(())
}

fn generators(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<()> {
    let step = u.int_in_range(-3..=5i64)?;
    let _ = write!(
        items,
        "gen fn series{index}(n: i64, start: i64) -> i64 {{\n    var i = 0;\n    while i < n {{\n        \
         println(f\"gen {{i}}\");\n        yield start + i * {};\n        i = i + 1;\n    }}\n    \
         println(\"gen done\");\n}}\n\n",
        IntTy::I64.literal(step.into())
    );
    let n = u.int_in_range(0..=5i64)?;
    let start = u.int_in_range(-10..=10i64)?;
    let stop_at = if u.arbitrary()? {
        Some(u.int_in_range(0..=n)?)
    } else {
        None
    };
    out.stmt("var sum = 0;");
    out.stmt("var seen = 0;");
    let mut body = format!(
        "for x in series{index}({n}, {}) {{\n        println(f\"got {{x}}\");\n        sum = sum + x;\n        seen = seen + 1;",
        IntTy::I64.literal(start.into())
    );
    if let Some(stop) = stop_at {
        let _ = write!(body, "\n        if seen == {stop} {{ break; }}");
    }
    body.push_str("\n    }");
    out.stmt(&body);
    let mut sum = 0;
    let mut seen = 0;
    let mut finished = true;
    for i in 0..n {
        let _ = writeln!(out.stdout, "gen {i}");
        let x = start + i * step;
        let _ = writeln!(out.stdout, "got {x}");
        sum += x;
        seen += 1;
        if stop_at == Some(seen) {
            finished = false;
            break;
        }
    }
    if finished {
        let _ = writeln!(out.stdout, "gen done");
    }
    out.print("f\"{seen} {sum}\"", format!("{seen} {sum}"));
    Ok(())
}

// ---------------------------------------------------------------------------
// Actors, supervisors and timers
// ---------------------------------------------------------------------------

fn actor_decl(index: usize, items: &mut String) {
    let _ = write!(
        items,
        "actor Tally{index} {{\n    var total: i64;\n    var log: string = \"\";\n\
         \n    receive fn add(n: i64) -> i64 {{\n        total = total &+ n;\n        total\n    }}\n\
         \n    receive fn note(word: string) {{\n        log = log + word;\n    }}\n\
         \n    receive fn read() -> string {{\n        f\"{{total}}:{{log}}\"\n    }}\n\
         \n    receive fn fail() {{\n        panic(\"tally failed\");\n    }}\n}}\n\n"
    );
}

/// One ask or tell against a `Tally` handle named `handle`, with its model.
fn tally_message(
    u: &mut Unstructured<'_>,
    out: &mut Out<'_>,
    handle: &str,
    total: &mut i64,
    log: &mut String,
) -> Result<()> {
    match u.int_in_range(0..=3u8)? {
        0 => {
            let n = u.int_in_range(-100..=100i64)?;
            *total = total.wrapping_add(n);
            out.print(
                &format!(
                    "match {handle}.add({}) {{ .Ok(t) => t, .Err(_) => -1 }}",
                    IntTy::I64.literal(n.into())
                ),
                *total,
            );
        }
        1 => {
            let n = u.int_in_range(-100..=100i64)?;
            *total = total.wrapping_add(n);
            out.stmt(&format!(
                "let _ = {handle}.add({});",
                IntTy::I64.literal(n.into())
            ));
        }
        2 => {
            let word = *u.choose(&WORDS)?;
            log.push_str(word);
            out.stmt(&format!("let _ = {handle}.note({});", quote(word)));
        }
        _ => out.print(
            &format!("match {handle}.read() {{ .Ok(s) => s, .Err(_) => \"err\" }}"),
            format!("{total}:{log}"),
        ),
    }
    Ok(())
}

fn actors(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<()> {
    actor_decl(index, items);
    let count = u.int_in_range(1..=3usize)?;
    let mut models = Vec::new();
    for a in 0..count {
        let start = u.int_in_range(-50..=50i64)?;
        out.stmt(&format!(
            "let a{a} = spawn Tally{index} {{ total: {} }};",
            IntTy::I64.literal(start.into())
        ));
        models.push((start, String::new()));
    }
    for _ in 0..u.int_in_range(1..=8usize)? {
        let a = u.int_in_range(0..=count - 1)?;
        let (total, log) = &mut models[a];
        tally_message(u, out, &format!("a{a}"), total, log)?;
    }
    for a in 0..count {
        let (total, log) = &models[a];
        out.print(
            &format!("match a{a}.read() {{ .Ok(s) => s, .Err(_) => \"err\" }}"),
            format!("{total}:{log}"),
        );
    }
    Ok(())
}

fn supervisors(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<()> {
    actor_decl(index, items);
    let first = u.int_in_range(-50..=50i64)?;
    let second = u.int_in_range(-50..=50i64)?;
    let strategy = *u.choose(&["one_for_one", "one_for_all", "rest_for_one"])?;
    let _ = write!(
        items,
        "supervisor Pool{index} {{\n    strategy: {strategy};\n    intensity: 5 within 60s;\n    \
         child first: Tally{index} {{ total: {} }};\n    child second: Tally{index} {{ total: {} }};\n}}\n\n",
        IntTy::I64.literal(first.into()),
        IntTy::I64.literal(second.into()),
    );
    out.stmt(&format!("let pool = spawn Pool{index};"));
    out.stmt("let first = pool.first;");
    out.stmt("let second = pool.second;");
    let mut models = [(first, String::new()), (second, String::new())];
    let declared = [first, second];
    let names = ["first", "second"];
    for _ in 0..u.int_in_range(1..=6usize)? {
        let child = u.int_in_range(0..=1usize)?;
        if u.ratio(1u8, 4u8)? {
            // A crash restarts the child from its declaration. The strategy
            // decides which siblings restart with it.
            out.stmt(&format!("let _ = {}.fail();", names[child]));
            out.stmt(&format!("restarted({});", names[child]));
            let restarts: &[usize] = match (strategy, child) {
                ("one_for_one", _) | ("rest_for_one", 1) => &[child],
                _ => &[0, 1],
            };
            for &r in restarts {
                models[r] = (declared[r], String::new());
            }
        } else {
            let (total, log) = &mut models[child];
            tally_message(u, out, names[child], total, log)?;
        }
    }
    for child in 0..2 {
        let (total, log) = &models[child];
        out.print(
            &format!(
                "match {}.read() {{ .Ok(s) => s, .Err(_) => \"err\" }}",
                names[child]
            ),
            format!("{total}:{log}"),
        );
    }
    out.stmt("stop(pool);");
    out.stmt("stopped(pool);");
    Ok(())
}

fn timers(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<()> {
    let _ = write!(
        items,
        "fn delayed{index}(ms: i64, value: i64) -> i64 {{\n    sleep(ms * 1ms);\n    value * 10\n}}\n\n"
    );
    let count = u.int_in_range(1..=4usize)?;
    let mut values = Vec::new();
    for t in 0..count {
        let ms = u.int_in_range(0..=20i64)?;
        let value = u.int_in_range(-9..=9i64)?;
        out.stmt(&format!(
            "let t{t} = fork delayed{index}({ms}, {});",
            IntTy::I64.literal(value.into())
        ));
        values.push(value);
    }
    let ms = u.int_in_range(0..=10u8)?;
    out.stmt(&format!("sleep({ms}ms);"));
    out.print("\"tick\"", "tick");
    for (t, value) in values.iter().enumerate() {
        out.print(&format!("await t{t}"), value * 10);
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Failing cases
// ---------------------------------------------------------------------------

fn failing_case(
    u: &mut Unstructured<'_>,
    index: usize,
    out: &mut Out<'_>,
    items: &mut String,
) -> Result<&'static str> {
    match u.int_in_range(0..=4u8)? {
        0 => {
            // An arithmetic failure: regenerate until the model fails.
            let ty = *u.choose(&INT_TYS)?;
            let vars = int_vars(u, ty, out)?;
            for _ in 0..64 {
                let expr = Expr::gen(u, ty, vars.len(), 3)?;
                if let Err(trap) = expr.eval(ty, &vars) {
                    let name = bind_expr(out, &mut 0, ty, &expr);
                    out.stmt(&format!("println({name});"));
                    out.stmt("println(\"unreachable\");");
                    return Ok(trap);
                }
            }
            let max = ty.literal(ty.max());
            out.stmt(&format!("let top: {} = {max};", ty.name()));
            out.stmt("println(top + 1);");
            Ok(OVERFLOW)
        }
        1 => {
            let len = u.int_in_range(0..=4usize)?;
            let at = u.int_in_range(len..=len + 3)?;
            let elements = (0..len)
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            out.stmt(&format!("var v: Vec<i64> = [{elements}];"));
            out.print("v.len()", len);
            out.stmt(&format!("println(v[{at}]);"));
            Ok("IndexOutOfBounds")
        }
        2 => {
            let word = *u.choose(&WORDS)?;
            out.print("\"before\"", "before");
            out.stmt(&format!("panic({});", quote(word)));
            Ok("UserPanic")
        }
        3 => {
            let n = u.int_in_range(-5..=5i64)?;
            out.stmt(&format!(
                "let opt: Option<i64> = if {} > 0 {{ .Some(1) }} else {{ .None }};",
                IntTy::I64.literal(n.into())
            ));
            if n > 0 {
                out.print("opt.expect(\"present\")", 1);
                out.stmt("panic(\"after\");");
            } else {
                out.stmt("println(opt.expect(\"absent value\"));");
            }
            Ok("UserPanic")
        }
        _ => {
            // An actor's crash fails its awaiting caller; the program then fails.
            actor_decl(index, items);
            out.stmt(&format!("let a = spawn Tally{index} {{ total: 1 }};"));
            out.stmt("let _ = a.fail();");
            out.print(
                "match a.read() { .Ok(s) => s, .Err(_) => \"crashed\" }",
                "crashed",
            );
            out.stmt("panic(\"done\");");
            Ok("UserPanic")
        }
    }
}
