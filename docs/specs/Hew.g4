// ============================================================
//   Hew Programming Language — Formal Grammar (ANTLR4)
//   Version: 0.10.0-pre (in flux during v0.5 compiler refactor)
//
//   Converted from ISO 14977 EBNF (docs/specs/grammar.ebnf) and
//   validated against all examples/ programs.
//
//   Known ANTLR4 limitations:
//     - Interpolated strings (f"...{expr}...") use simplified lexer rules.
//       Full support requires ANTLR4 lexer modes.
//     - ">>" in nested generics (e.g. Vec<Vec<i32>>) is lexed
//       as a shift operator. Use a space: Vec<Vec<i32> >.
// ============================================================

grammar Hew;

// ----------------------------------------------------------------
//  Identifier helper — allows contextual keywords as identifiers.
//  ANTLR4 turns every string literal used in parser rules into
//  a keyword token.  The real Hew parser treats most of these as
//  contextual (they're keywords only in specific syntactic
//  positions).  This rule restores that flexibility.
//
//  Only tokens that the Rust lexer does NOT reserve as hard
//  keywords are listed here.  Primitive type names lex as
//  Identifier in the real lexer; `from` and `after` have explicit
//  parser workarounds; the remaining domain keywords are only
//  meaningful in specific syntactic positions and are not reserved.
//
//  Hard keywords (NOT valid as identifiers — omitted from this rule):
//    receive, default
// ----------------------------------------------------------------

ident
    : IDENT
    // Primitive type names (lex as Identifier in the real lexer)
    | 'i8' | 'i16' | 'i32' | 'i64' | 'isize'
    | 'u8' | 'u16' | 'u32' | 'u64' | 'usize'
    | 'f32' | 'f64'
    | 'bool' | 'string' | 'bytes'
    // Contextual keywords — lexer keywords accepted as identifiers by the parser
    | 'from' | 'after'
    | 'init' | 'child' | 'restart' | 'budget' | 'strategy'
    | 'permanent' | 'transient' | 'temporary'
    | 'one_for_one' | 'one_for_all' | 'rest_for_one'
    | 'wire' | 'optional' | 'deprecated' | 'reserved'
    | 'state' | 'event' | 'on' | 'when' | 'join'
    // Domain keywords that are NOT lexer keywords (always lex as Identifier)
    | 'mailbox' | 'overflow'
    | 'block' | 'fail' | 'coalesce' | 'fallback'
    | 'drop_new' | 'drop_old'
    | 'launch' | 'cancel' | 'is_cancelled'
    | 'list' | 'export' | 'repeated' | 'since'
    ;

// ----------------------------------------------------------------
//  Program structure
// ----------------------------------------------------------------

program
    : item* EOF
    ;

item
    : attribute* visibility?
      ( constDecl
      | typeDecl
      | typeAliasDecl
      | recordDecl
      | traitDecl
      | fnDecl
      | asyncFnDecl
      | genFnDecl
      | asyncGenFnDecl
      | actorDecl
      | supervisorDecl
      | wireDecl
      | wireStructDecl
      | machineDecl
      )
    | attribute*
      ( importDecl
      | implDecl
      | externBlock
      )
    ;

visibility
    : 'pub' ( '(' ( 'package' | 'super' ) ')' )?
    ;

// ----------------------------------------------------------------
//  Attributes
// ----------------------------------------------------------------

attribute
    : HASH_LBRACKET attrContent ']'
    ;

attrContent
    : ident ( '(' attrArgs ')' )?
    ;

attrArgs
    : attrArg ( ',' attrArg )*
    ;

attrArg
    : ident ( '=' ( STRING_LIT | ident | INTEGER_LIT ) )?
    | STRING_LIT
    | DURATION_LIT
    ;

// ----------------------------------------------------------------
//  Module system
// ----------------------------------------------------------------

importDecl
    : 'import' modulePath ( '::' importSpec )? ';'
    | 'import' STRING_LIT ';'              // File-path import: import "math_lib.hew";
    ;

modulePath
    : ident ( '::' ident )*
    ;

importSpec
    : ident
    | '{' importName ( ',' importName )* '}'
    | '*'
    ;

importName
    : ident ( 'as' ident )?
    ;

// ----------------------------------------------------------------
//  Constants and type declarations
// ----------------------------------------------------------------

constDecl
    : 'const' ident ':' type_ '=' expr ';'
    ;

typeDecl
    : 'type'   ident typeParams? whereClause? structBody
    | 'indirect'? 'enum'   ident typeParams? whereClause? enumBody
    ;

typeAliasDecl
    : 'type' ident '=' type_ ';'
    ;

recordDecl
    : 'record' ident typeParams? whereClause? recordBody
    ;

recordBody
    : recordNamedBody
    | recordTupleBody
    ;

recordNamedBody
    : '{' recordFieldDecl ( ',' recordFieldDecl )* ','? '}'
    ;

recordFieldDecl
    : ident ':' type_
    ;

recordTupleBody
    : '(' type_ ( ',' type_ )* ','? ')' ';'
    ;

wireDecl
    : 'wire' 'type' ident wireStructBody
    | 'wire' 'enum'   ident wireEnumBody
    ;

// #[wire] struct syntax — alternative wire struct declaration
wireStructDecl
    : 'struct' ident '{' wireStructItem* '}'
    ;

typeParams
    : '<' typeParam ( ',' typeParam )* '>'
    ;

typeParam
    : ident ( ':' traitBounds )?
    ;

traitBounds
    : traitBound ( '+' traitBound )*
    ;

traitBound
    : ident typeArgs?
    ;

// ----------------------------------------------------------------
//  Where clauses
// ----------------------------------------------------------------

whereClause
    : 'where' wherePredicate ( ',' wherePredicate )* ','?
    ;

wherePredicate
    : type_ ':' traitBounds
    ;

// ----------------------------------------------------------------
//  Type / wire bodies and fields
// ----------------------------------------------------------------

structBody
    : '{' structBodyItem* '}'
    ;

structBodyItem
    : structFieldDecl
    | fnDecl
    ;

enumBody
    : '{' variantDecl* '}'
    ;

wireStructBody
    : '{' wireStructItem* '}'
    ;

wireStructItem
    : wireFieldDecl
    | reservedDecl ';'?
    ;

wireEnumBody
    : '{' wireEnumItem* '}'
    ;

wireEnumItem
    : variantDecl
    | reservedDecl ';'?
    ;

structFieldDecl
    : attribute* ident ':' type_ ( ',' | ';' )?
    ;

actorFieldDecl
    : ( 'let' | 'var' ) ident ':' type_ ( '=' expr )? ( ',' | ';' )
    ;

wireFieldDecl
    : ident ':' wireType ( ( '@' | '=' ) INT_LIT )? wireAttr* ( ',' | ';' )?
    ;

wireAttr
    : 'optional'
    | 'deprecated'
    | 'repeated'
    | 'default' '(' expr ')'
    | 'since' INT_LIT                           // field version: since 2
    | 'json' '(' STRING_LIT ')'                 // per-field JSON key override
    | 'yaml' '(' STRING_LIT ')'                 // per-field YAML key override
    | reservedDecl
    ;

reservedDecl
    : 'reserved' '(' INT_LIT ( ',' INT_LIT )* ')'
    ;

variantDecl
    : ident ( '(' typeList ')' | '{' ( ident ':' type_ ( ',' | ';' )? )* '}' )? ( ',' | ';' )?
    ;

typeList
    : type_ ( ',' type_ )*
    ;

// ----------------------------------------------------------------
//  Traits
// ----------------------------------------------------------------

traitDecl
    : 'trait' ident typeParams? traitSuper? '{' traitItem* '}'
    ;

traitSuper
    : ':' traitBounds
    ;

traitItem
    : fnSig ( ';' | block )                    // Required or default method
    | associatedType
    ;

associatedType
    : 'type' ident ( ':' traitBounds )? ( '=' type_ )? ';'
    ;

fnSig
    : 'fn' ident typeParams?
      '(' params? ')'
      retType? whereClause?
    ;

// ----------------------------------------------------------------
//  Impl blocks
// ----------------------------------------------------------------

implDecl
    : 'impl' typeParams? traitBound 'for' type_ whereClause?
      '{' implBodyItem* '}'                         // Trait impl
    | 'impl' typeParams? type_ whereClause?
      '{' implBodyItem* '}'                         // Inherent impl
    ;

implBodyItem
    : fnDecl
    | 'type' ident '=' type_ ';'                    // Associated type impl
    ;

// ----------------------------------------------------------------
//  Actors
// ----------------------------------------------------------------

actorDecl
    : 'actor' ident traitSuper? '{'
        actorBodyItem*
      '}'
    ;

actorBodyItem
    : actorInit
    | mailboxDecl
    | actorFieldDecl
    | actorBareField                        // bare field: name: type;
    | receiveFnDecl
    | receiveGenFnDecl
    | fnDecl
    | genFnDecl
    ;

// Actor fields without let/var prefix (shorthand)
actorBareField
    : ident ':' type_ ( '=' expr )? ( ',' | ';' )
    ;

actorInit
    : 'init' '(' params? ')' block
    ;

mailboxDecl
    : 'mailbox' INT_LIT overflowPolicy? ';'
    ;

overflowPolicy
    : 'overflow' overflowKind
    ;

overflowKind
    : 'block'
    | 'drop_new'
    | 'drop_old'
    | 'fail'
    | 'coalesce' '(' ident ')' coalesceFallback?
    ;

coalesceFallback
    : 'fallback' overflowKind
    ;

receiveFnDecl
    : attribute* 'receive' 'fn' ident typeParams? '(' params? ')' retType? whereClause? block
    ;

receiveGenFnDecl
    : attribute* 'receive' 'gen' 'fn' ident typeParams? '(' params? ')' '->' type_ whereClause? block
    ;

// ----------------------------------------------------------------
//  Supervisors — supports both declarative and child-spec syntax
// ----------------------------------------------------------------

supervisorDecl
    : 'supervisor' ident '{' supervisorBody '}'
    ;

supervisorBody
    : ( supervisorField | childSpec )*
    ;

supervisorField
    : 'strategy' ':' ident ( ',' | ';' )?                       // strategy: one_for_one
    | 'intensity' ':' INT_LIT 'within' DURATION_LIT ( ',' | ';' )? // intensity: 5 within 60s
    ;

childSpec
    : ( 'child' | 'pool' ) ident ':' ident ( '(' args ')' )? childClause* ( ',' | ';' )?
    ;

childClause
    : 'restart' ':' ( 'permanent' | 'transient' | 'temporary' )
    | 'shutdown' ':' ( DURATION_LIT | 'brutal_kill' | 'infinity' )
    | 'wired_to' ':' '{' ( ident ( ':' ident )? ','? )* '}'
    ;

// ----------------------------------------------------------------
//  State Machines
// ----------------------------------------------------------------

machineDecl
    : 'machine' ident typeParams? '{' eventsHeader emitsHeader? stateDecl* transitionDecl* defaultArm? '}'
    ;

eventsHeader
    : 'events' '{' eventDecl* '}'
    ;

eventDecl
    : ident ( ';' | '{' ( ident ':' type_ ( ';' | ',' )? )* '}' ';'? )
    ;

emitsHeader
    : 'emits' '{' ( ident ';' )* '}'
    ;

stateDecl
    : 'state' ident ( '{' ( ident ':' type_ ( ';' | ',' )? )* ( 'entry' block )? ( 'exit' block )? compositeMember* '}' )? ';'?
    ;

compositeMember
    : 'initial'? stateDecl
    ;

transitionDecl
    : 'on' ident ( '(' ident ( ',' ident )* ')' )? ':' statePattern '=>' statePattern 'reenter'? ( 'when' expr )? ( block | '{' fieldInitList '}' | ';' )
    ;

defaultArm
    : 'default' '{' 'state' '}'
    ;

statePattern
    : ident
    | '_'
    ;

// ----------------------------------------------------------------
//  FFI / Extern declarations
// ----------------------------------------------------------------

externBlock
    : 'extern' STRING_LIT? '{' externFnDecl* '}'
    ;

externFnDecl
    : 'fn' ident '(' ( externParams ( '..' )? )? ')' retType? ';'
    ;

externParams
    : externParam ( ',' externParam )*
    ;

externParam
    : ident ':' type_
    ;

// ----------------------------------------------------------------
//  Functions
// ----------------------------------------------------------------

fnDecl
    : 'fn' ident typeParams? '(' params? ')' retType? whereClause? block
    ;

asyncFnDecl
    : 'async' 'fn' ident typeParams? '(' params? ')' retType? whereClause? block
    ;

genFnDecl
    : 'gen' 'fn' ident typeParams? '(' params? ')' '->' type_ whereClause? block
    ;

asyncGenFnDecl
    : 'async' 'gen' 'fn' ident typeParams? '(' params? ')' '->' type_ whereClause? block
    ;

params
    : param ( ',' param )*
    ;

param
    : 'var'? ident ':' type_
    ;

retType
    : '->' type_
    ;

// ----------------------------------------------------------------
//  Statements
// ----------------------------------------------------------------

block
    : '{' stmt* expr? '}'
    ;

stmt
    : letStmt
    | varStmt
    | assignStmt
    | ifStmt
    | ifLetStmt
    | matchStmt
    | loopStmt
    | forStmt
    | whileStmt
    | whileLetStmt
    | breakStmt
    | continueStmt
    | returnStmt
    | deferStmt
    | exprStmt
    | unsafeBlock
    | block                                 // Standalone block: { ... }
    | forkExpr                              // fork { ... } or fork child without trailing ;
    ;

letStmt
    : 'let' pattern ( ':' type_ )? ( '=' expr )? ';'
    ;

varStmt
    : 'var' ident ( ':' type_ )? ( '=' expr )? ';'
    ;

assignStmt
    : lvalue assignOp expr ';'
    ;

assignOp
    : '=' | '+=' | '-=' | '*=' | '/=' | '%='
    | '&=' | '|=' | '^=' | '<<=' | '>>='
    ;

lvalue
    : ident ( '.' ident | '[' expr ']' )*
    ;

ifStmt
    : 'if' expr block ( 'else' ( ifStmt | block ) )?
    ;

ifLetStmt
    : 'if' 'let' pattern '=' expr block ( 'else' block )?
    ;

matchStmt
    : 'match' expr '{' matchArm* '}'
    ;

matchArm
    : pattern guard? '=>' ( block | expr ','? )
    ;

guard
    : 'if' expr
    ;

loopStmt
    : ( LABEL ':' )? 'loop' block
    ;

forStmt
    : ( LABEL ':' )? 'for' 'await'? pattern 'in' expr block
    ;

whileStmt
    : ( LABEL ':' )? 'while' expr block
    ;

whileLetStmt
    : ( LABEL ':' )? 'while' 'let' pattern '=' expr block
    ;

breakStmt
    : 'break' LABEL? expr? ';'
    ;

continueStmt
    : 'continue' LABEL? ';'
    ;

returnStmt
    : 'return' expr? ';'
    ;

deferStmt
    : 'defer' expr ';'
    | 'defer' block
    ;

exprStmt
    : expr ';'
    ;

unsafeBlock
    : 'unsafe' block
    ;

// ----------------------------------------------------------------
//  Expressions  — precedence encoded by rule nesting
// ----------------------------------------------------------------

expr
    : unsafeExpr
    | timeoutExpr
    ;

unsafeExpr
    : 'unsafe' block
    ;

// Timeout combinator:  expr | after duration
timeoutExpr
    : rangeExpr ( '|' 'after' expr )?
    ;

rangeExpr
    : orExpr ( ( '..' | '..=' ) orExpr )?
    ;

orExpr
    : andExpr ( '||' andExpr )*
    ;

andExpr
    : eqExpr ( '&&' eqExpr )*
    ;

eqExpr
    : relExpr ( ( '==' | '!=' ) relExpr )*
    ;

relExpr
    : bitOrExpr ( ( '<' | '<=' | '>' | '>=' ) bitOrExpr )*
    ;

bitOrExpr
    : bitXorExpr ( '|' bitXorExpr )*
    ;

bitXorExpr
    : bitAndExpr ( '^' bitAndExpr )*
    ;

bitAndExpr
    : shiftExpr ( '&' shiftExpr )*
    ;

shiftExpr
    : addExpr ( ( '<<' | '>>' ) addExpr )*
    ;

// + also concatenates strings
addExpr
    : mulExpr ( ( '+' | '-' ) mulExpr )*
    ;

mulExpr
    : unaryExpr ( ( '*' | '/' | '%' ) unaryExpr )*
    ;

unaryExpr
    : ( '!' | '-' | '~' | 'await' ) unaryExpr
    | postfixExpr
    ;

postfixExpr
    : primary ( '?' | '.' ident | '.' INT_LIT | '::' typeArgs '(' args? ')' | '::' ident | '(' args? ')' | '[' expr ']' | 'as' type_ )*
    ;

primary
    : literal
    | INTERPOLATED_STRING
    | ident '{' fieldInitList '}'           // Struct init: Point { x: 1, y: 2 }
    | ident                                 // Plain identifier
    | 'this'                                // Actor self-reference
    | '{' expr ':' expr ( ',' expr ':' expr )* ','? '}'  // Map literal: {"key": val}
    | block                                 // Block expression: { ... }
    | '[' expr ';' expr ']'                 // Array repeat: [0; 256]
    | '[' exprList? ']'                     // Array literal
    | 'bytes' '[' exprList? ']'             // Byte array: bytes[0x41, 0x42]
    | '(' expr ( ',' exprList )? ')'        // Grouping or tuple
    | ifExpr
    | ifLetExpr
    | matchExpr
    | lambda
    | spawn
    | selectExpr
    | joinExpr
    | forkExpr                              // fork { ... } block or fork [name '='] expr child
    | cooperateExpr
    | yieldExpr
    ;

fieldInitList
    : fieldInit ( ',' fieldInit )* ','?
    ;

fieldInit
    : ident ':' expr
    ;

ifExpr
    : 'if' expr block ( 'else' ( ifExpr | block ) )?
    ;

ifLetExpr
    : 'if' 'let' pattern '=' expr block ( 'else' block )?
    ;

matchExpr
    : 'match' expr '{' matchArm* '}'
    ;

literal
    : INT_LIT
    | FLOAT_LIT
    | DURATION_LIT
    | STRING_LIT
    | BYTE_STRING_LIT
    | CHAR_LIT
    | REGEX_LIT
    | 'true'
    | 'false'
    ;

exprList
    : expr ( ',' expr )*
    ;

args
    : arg ( ',' arg )*
    ;

arg
    : ( ident ':' )? expr
    ;

// ----------------------------------------------------------------
//  Closures
// ----------------------------------------------------------------

lambda
    : 'move'? '|' lambdaParams? '|' expr
    | 'move'? '||' expr
    | 'move'? '|' lambdaParams? '|' retType block
    | typeParams '(' lambdaParams? ')' retType? '=>' ( expr | block )
    ;

lambdaParams
    : lambdaParam ( ',' lambdaParam )*
    ;

lambdaParam
    : ident ( ':' type_ )?
    ;

// ----------------------------------------------------------------
//  Actor operations
// ----------------------------------------------------------------

spawn
    : 'spawn' ( lambdaActor | actorSpawn )
    ;

actorSpawn
    : ident ( '.' ident )? typeArgs? ( '(' fieldInitList? ')' )?
    ;

lambdaActor
    : 'move'? '(' lambdaParams? ')' retType? '=>' ( expr | block )
    ;

// ----------------------------------------------------------------
//  Concurrency expressions
// ----------------------------------------------------------------

selectExpr
    : 'select' '{' selectArm* timeoutArm? '}'
    ;

selectArm
    : pattern 'from' expr '=>' expr ','?
    ;

timeoutArm
    : 'after' expr '=>' expr ','?
    ;

joinExpr
    : 'join' ( '{' exprList? ','? '}' | '(' exprList? ','? ')' )
    ;

// ----------------------------------------------------------------
//  Structured concurrency
// ----------------------------------------------------------------

// `fork` is dual-use. The block opens a structured-concurrency region;
// the child form (`fork name = expr` or bare `fork expr`) spawns a
// sibling task within an open block. Both share the keyword; the
// disambiguator is the token following `fork`. The two forms are
// folded into a single production here so the keyword is matched
// only once.
//
// Notes:
//   - `scope { ... }` is the structured-concurrency block (the lexical-
//     lifetime boundary). `fork name = expr` and bare `fork expr` are
//     only legal dynamically inside a `scope` block; outside one, the
//     checker emits `ForkOutsideScopeBlock`.
//   - Earlier drafts exposed `scope |s| { s.launch { } / s.spawn { } /
//     s.cancel() }`; that entire surface was removed in the 2026 edition
//     (HEW-SPEC §4.9 historical note).
scopeExpr
    : 'scope' block                         // structured-concurrency block
    ;
forkChild
    : 'fork' ident '=' expr                 // child binding: fork name = expr
    | 'fork' expr                           // bare child:    fork expr
    ;

cooperateExpr
    : 'cooperate'
    ;

yieldExpr
    : 'yield' expr?                             // yield with optional value
    ;

// ----------------------------------------------------------------
//  Types
//
//  Built-in generic types (Task<T>, Vec<T>, HashMap<K,V>, etc.)
//  are syntactically just ident typeArgs?, so they are folded
//  into the first alternative.  Listed in comments for reference:
//    Task<T>, ActorRef<T>, Actor<M,R>, Arc<T>, Rc<T>,
//    Weak<T>, Result<T,E>, Option<T>, Generator<Y>,
//    AsyncGenerator<Y>, Vec<T>, HashMap<K,V>
// ----------------------------------------------------------------

type_
    : primitiveType                         // i32, f64, bool, string, etc.
    | ident typeArgs?                       // Named type (includes all built-in generics)
    | '(' typeList? ')'                     // Tuple type / unit ()
    | '[' type_ ';' INT_LIT ']'             // Fixed-size array: [i32; 256]
    | '[' type_ ']'                         // Slice: [i32]
    | 'fn' '(' typeList? ')' retType?       // Function type
    | '*' 'mut' type_                       // Mutable raw pointer
    | '*' 'const' type_                     // Immutable raw pointer
    | 'dyn' traitBound                      // Trait object (single trait + optional type args)
    | 'dyn' '(' traitBounds ')'             // Multi-trait object: dyn (Trait1 + Trait2)
    | '_'                                   // Inferred type
    ;

// Primitive type names — listed explicitly because ANTLR4 turns
// string literals (from wireType etc.) into keyword tokens that
// no longer match IDENT.
primitiveType
    : 'i8'  | 'i16' | 'i32' | 'i64' | 'isize'
    | 'u8'  | 'u16' | 'u32' | 'u64' | 'usize'
    | 'f32' | 'f64'
    | 'bool' | 'string' | 'bytes' | 'duration'
    ;

typeArgs
    : '<' type_ ( ',' type_ )* '>'
    // NOTE: Nested generics like Vec<Vec<i32>> require a space before the
    // closing '>>' because the lexer produces a single GreaterGreater token.
    // The Grammarinator serializer should insert spaces around '>' in type
    // contexts.  The parser does not split '>>' into two '>' tokens.
    ;

// ----------------------------------------------------------------
//  Wire types (for wire struct/enum declarations)
// ----------------------------------------------------------------

wireType
    : 'u8'  | 'u16' | 'u32' | 'u64'
    | 'i8'  | 'i16' | 'i32' | 'i64'
    | 'f32' | 'f64'
    | 'bool' | 'bytes' | 'string'
    | ident
    ;

// ----------------------------------------------------------------
//  Pattern matching
// ----------------------------------------------------------------

pattern
    : pattern '|' pattern                       // Or-pattern
    | ident ( '::' ident )+ ( '(' patternList? ')' )?  // Qualified: Mod::Color::Red or Option::Some(x)
    | ident '(' patternList? ')'                // Constructor: Some(x)
    | ident '{' patternFieldList? '}'           // Struct: Point { x, y }
    | '(' patternList ')'                       // Tuple: (a, b, c)
    | literalPattern
    | '_'                                       // Wildcard
    | ident                                     // Binding
    ;

literalPattern
    : '-'? INT_LIT
    | '-'? FLOAT_LIT
    | STRING_LIT
    | CHAR_LIT
    | 'true'
    | 'false'
    ;

patternList
    : pattern ( ',' pattern )*
    ;

patternFieldList
    : patternField ( ',' patternField )*
    ;

patternField
    : ident ( ':' pattern )?
    ;

// ================================================================
//  LEXER RULES
// ================================================================

// ----------------------------------------------------------------
//  Multi-character operators and punctuation
//  (defined explicitly so longest-match works correctly)
// ----------------------------------------------------------------

HASH_LBRACKET : '#[' ;
ARROW         : '->' ;
FAT_ARROW     : '=>' ;
DOTDOT        : '..' ;
DOTDOTEQ      : '..=' ;
SHL           : '<<' ;
SHR           : '>>' ;
SHL_ASSIGN    : '<<=' ;
SHR_ASSIGN    : '>>=' ;
EQ            : '==' ;
NE            : '!=' ;
LE            : '<=' ;
GE            : '>=' ;
PLUS_ASSIGN   : '+=' ;
MINUS_ASSIGN  : '-=' ;
STAR_ASSIGN   : '*=' ;
SLASH_ASSIGN  : '/=' ;
PERCENT_ASSIGN: '%=' ;
AND_ASSIGN    : '&=' ;
OR_ASSIGN     : '|=' ;
XOR_ASSIGN    : '^=' ;
COLONCOLON    : '::' ;
LOGICAL_AND   : '&&' ;
LOGICAL_OR    : '||' ;

// ----------------------------------------------------------------
//  Labels (loop labels: @name)
// ----------------------------------------------------------------

LABEL
    : '@' [a-zA-Z_] [a-zA-Z0-9_]*
    ;

// ----------------------------------------------------------------
//  Duration literals  (must precede INT_LIT — longest match)
// ----------------------------------------------------------------

DURATION_LIT
    : [0-9] [0-9_]* ( 'ns' | 'us' | 'ms' | 's' | 'm' | 'h' )
    ;

// ----------------------------------------------------------------
//  Numeric literals
// ----------------------------------------------------------------

FLOAT_LIT
    : [0-9]+ '.' [0-9]+ ( [eE] [+\-]? [0-9]+ )?
    ;

INT_LIT
    : '0x' [0-9a-fA-F] [0-9a-fA-F_]*
    | '0X' [0-9a-fA-F] [0-9a-fA-F_]*
    | '0o' [0-7] [0-7_]*
    | '0O' [0-7] [0-7_]*
    | '0b' [01] [01_]*
    | '0B' [01] [01_]*
    | [0-9] [0-9_]*
    ;

// ----------------------------------------------------------------
//  String literals
// ----------------------------------------------------------------

REGEX_LIT
    : 're"' ( '\\' . | ~[\\"] )* '"'
    ;

INTERPOLATED_STRING
    : 'f"' ( ESC_SEQ | INTERP_BRACE | ~[\\"{] )* '"'
    ;

BYTE_STRING_LIT
    : 'b"' ( ESC_SEQ | ~[\\"] )* '"'          // Byte string
    ;

CHAR_LIT
    : '\'' ( ESC_SEQ | ~['\\] ) '\''           // Character literal
    ;

STRING_LIT
    : 'r"' ~["]* '"'                           // Raw string
    | '"' ( ESC_SEQ | ~[\\"] )* '"'            // Regular string
    ;

fragment INTERP_BRACE
    : '{' ( ESC_SEQ | INTERP_BRACE | ~[\\{}] )* '}'
    ;

fragment ESC_SEQ
    : '\\' [nrt\\"0]
    | '\\x' [0-9a-fA-F] [0-9a-fA-F]
    ;

// ----------------------------------------------------------------
//  Identifiers  (must come after all keyword-like tokens)
// ----------------------------------------------------------------

IDENT
    : [a-zA-Z_] [a-zA-Z0-9_]*
    ;

// ----------------------------------------------------------------
//  Comments  — doc comments are visible to parser rules;
//  regular comments go to hidden channel.
// ----------------------------------------------------------------

DOC_COMMENT
    : '///' ~[\r\n]* -> channel(HIDDEN)
    ;

INNER_DOC_COMMENT
    : '//!' ~[\r\n]* -> channel(HIDDEN)
    ;

LINE_COMMENT
    : '//' ~[\r\n]* -> channel(HIDDEN)
    ;

BLOCK_COMMENT
    : '/*' .*? '*/' -> channel(HIDDEN)
    ;

// ----------------------------------------------------------------
//  Whitespace
// ----------------------------------------------------------------

WS
    : [ \t\r\n]+ -> skip
    ;
