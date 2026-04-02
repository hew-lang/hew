// ============================================================
//   Hew Programming Language — Formal Grammar (ANTLR4)
//   Version: 0.10.0
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
    : 'pure'? fnSig ( ';' | block )            // Required or default method
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
    : 'pure'? fnDecl
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
    : attribute* 'pure'? 'receive' 'fn' ident typeParams? '(' params? ')' retType? whereClause? block
    ;

receiveGenFnDecl
    : attribute* 'pure'? 'receive' 'gen' 'fn' ident typeParams? '(' params? ')' '->' type_ whereClause? block
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
    : ident ':' supervisorFieldValue ( ',' | ';' )?
    ;

supervisorFieldValue
    : ident                                     // strategy: one_for_one
    | INT_LIT                                   // max_restarts: 5
    | DURATION_LIT                              // window: 10s
    | '[' ident ( ',' ident )* ']'              // children: [Worker, Logger]
    ;

childSpec
    : 'child' ident ':' ident ( '(' args ')' )? ( 'permanent' | 'transient' | 'temporary' )? ( ',' | ';' )?
    ;

restartSpec
    : 'restart' '(' ( 'permanent' | 'transient' | 'temporary' ) ')'
      ( 'budget' '(' INT_LIT ',' DURATION_LIT ')' )?
      ( 'strategy' '(' ( 'one_for_one' | 'one_for_all' | 'rest_for_one' ) ')' )?
    ;

// ----------------------------------------------------------------
//  State Machines
// ----------------------------------------------------------------

machineDecl
    : 'machine' ident '{' machineItem* '}'
    ;

machineItem
    : machineState
    | machineEvent
    | machineTransition
    | machineDefault
    ;

machineState
    : 'state' ident ( '{' ( ident ':' type_ ( ';' | ',' )? )* '}' )? ';'?
    ;

machineEvent
    : 'event' ident ( '{' ( ident ':' type_ ( ';' | ',' )? )* '}' )? ';'?
    ;

machineTransition
    : 'on' ident ':' statePattern '->' statePattern ( 'when' expr )? ( block | '{' fieldInitList '}' | ';' )
    ;

machineDefault
    : 'default' ( block | ';' )
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
    : 'pure'? 'fn' ident typeParams? '(' params? ')' retType? whereClause? block
    ;

asyncFnDecl
    : 'pure'? 'async' 'fn' ident typeParams? '(' params? ')' retType? whereClause? block
    ;

genFnDecl
    : 'pure'? 'gen' 'fn' ident typeParams? '(' params? ')' '->' type_ whereClause? block
    ;

asyncGenFnDecl
    : 'pure'? 'async' 'gen' 'fn' ident typeParams? '(' params? ')' '->' type_ whereClause? block
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
    | scope                                 // scope { ... } without trailing ;
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
    | sendExpr
    ;

unsafeExpr
    : 'unsafe' block
    ;

// Send:  actor <- message
sendExpr
    : timeoutExpr ( '<-' expr )?
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
    : relExpr ( ( '==' | '!=' | '=~' | '!~' ) relExpr )*
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
    | scope                                 // scope { } or scope |s| { ... }
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
//  Closures  — arrow syntax only (v0.6.0: pipe syntax |x| removed)
// ----------------------------------------------------------------

lambda
    : 'move'? typeParams? '(' lambdaParams? ')' retType? '=>' ( expr | block )
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
    : pattern ( '<-' | 'from' ) expr '=>' expr ','?
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

scope
    : 'scope' ( '|' ident '|' )? block
    ;

// Inside scope |s| { ... }, the binding supports:
//   s.launch { expr }      — via identifier desugaring in parser
//   s.spawn { expr }       — via identifier desugaring in parser
//   s.cancel()             — via identifier desugaring in parser

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
//    Task<T>, Scope, ActorRef<T>, Actor<M,R>, Arc<T>, Rc<T>,
//    Weak<T>, Result<T,E>, Option<T>, Generator<Y>,
//    AsyncGenerator<Y>, ActorStream<Y>, Vec<T>, HashMap<K,V>
// ----------------------------------------------------------------

type_
    : primitiveType                         // i32, f64, bool, string, etc.
    | ident typeArgs?                       // Named type (includes all built-in generics)
    | '(' typeList? ')'                     // Tuple type / unit ()
    | '[' type_ ';' INT_LIT ']'             // Fixed-size array: [i32; 256]
    | '[' type_ ']'                         // Slice: [i32]
    | 'fn' '(' typeList? ')' retType?       // Function type
    | '*' 'var' type_                       // Mutable raw pointer
    | '*' type_                             // Immutable raw pointer
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
SEND          : '<-' ;
DOTDOT        : '..' ;
DOTDOTEQ      : '..=' ;
SHL           : '<<' ;
SHR           : '>>' ;
SHL_ASSIGN    : '<<=' ;
SHR_ASSIGN    : '>>=' ;
EQ            : '==' ;
NE            : '!=' ;
MATCH_OP      : '=~' ;
NOT_MATCH_OP  : '!~' ;
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
