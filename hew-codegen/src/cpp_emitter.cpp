#include "hew/cpp_emitter.h"

#include <cctype>
#include <cstdint>
#include <iomanip>
#include <optional>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string_view>

namespace hew {
namespace {

template <class... Ts> struct Overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts> Overloaded(Ts...) -> Overloaded<Ts...>;

enum class ReturnKind { Unit, Value, Main };

std::string quoteString(std::string_view text) {
  std::ostringstream out;
  out << '"';
  for (unsigned char ch : text) {
    switch (ch) {
    case '\\':
      out << "\\\\";
      break;
    case '"':
      out << "\\\"";
      break;
    case '\n':
      out << "\\n";
      break;
    case '\r':
      out << "\\r";
      break;
    case '\t':
      out << "\\t";
      break;
    default:
      if (std::isprint(ch) != 0) {
        out << static_cast<char>(ch);
      } else {
        out << "\\x" << std::hex << std::setw(2) << std::setfill('0')
            << static_cast<int>(ch) << std::dec << std::setfill(' ');
      }
      break;
    }
  }
  out << '"';
  return out.str();
}

std::string quoteChar(char32_t value) {
  std::ostringstream out;
  out << "U'\\u" << std::hex << std::uppercase << std::setw(4)
      << std::setfill('0') << static_cast<std::uint32_t>(value) << "'";
  return out.str();
}

std::string spanSuffix(const ast::Span &span) {
  std::ostringstream out;
  out << " at span " << span.start << ".." << span.end;
  return out.str();
}

std::string sanitizeIdentifier(std::string_view name) {
  static const std::set<std::string> kKeywords = {
      "alignas",   "alignof",  "and",      "asm",       "auto",     "bitand",
      "bitor",     "bool",     "break",    "case",      "catch",    "char",
      "char8_t",   "char16_t", "char32_t", "class",     "compl",    "concept",
      "const",     "consteval","constexpr","constinit", "continue", "co_await",
      "co_return", "co_yield", "decltype", "default",   "delete",   "do",
      "double",    "else",     "enum",     "explicit",  "export",   "extern",
      "false",     "float",    "for",      "friend",    "goto",     "if",
      "inline",    "int",      "long",     "mutable",   "namespace","new",
      "noexcept",  "not",      "nullptr",  "operator",  "or",       "private",
      "protected", "public",   "register", "reinterpret_cast",      "requires",
      "return",    "short",    "signed",   "sizeof",    "static",   "struct",
      "switch",    "template", "this",     "throw",     "true",     "try",
      "typedef",   "typename", "union",    "unsigned",  "using",    "virtual",
      "void",      "volatile", "while",    "xor"};

  std::string out;
  out.reserve(name.size() + 8);
  for (size_t i = 0; i < name.size(); ++i) {
    const unsigned char ch = static_cast<unsigned char>(name[i]);
    if ((std::isalnum(ch) != 0) || ch == '_') {
      out.push_back(static_cast<char>(ch));
    } else {
      out.push_back('_');
    }
  }
  if (out.empty()) {
    out = "hew_ident";
  }
  if (std::isdigit(static_cast<unsigned char>(out.front())) != 0) {
    out.insert(out.begin(), '_');
  }
  if (kKeywords.contains(out)) {
    out.insert(0, "hew_");
  }
  return out;
}

class CppEmitter {
public:
  std::string emit(const ast::Program &program) {
    std::ostringstream out;
    emitPreamble(out);
    for (const auto &item : program.items) {
      emitItem(out, item);
    }
    return out.str();
  }

private:
  [[noreturn]] void unsupported(std::string_view what, const ast::Span &span) const {
    throw std::runtime_error("experimental C++ backend does not support " +
                             std::string(what) + spanSuffix(span));
  }

  static void emitLine(std::ostringstream &out, int indent, std::string_view text = {}) {
    out << std::string(static_cast<size_t>(indent * 2), ' ') << text << '\n';
  }

  static bool isUnitReturn(const std::optional<ast::Spanned<ast::TypeExpr>> &returnType) {
    if (!returnType.has_value()) {
      return true;
    }
    const auto *named = std::get_if<ast::TypeNamed>(&returnType->value.kind);
    return named != nullptr && named->name == "unit" && !named->type_args.has_value();
  }

  std::string emitType(const ast::Spanned<ast::TypeExpr> &ty) const {
    return std::visit(
        Overloaded{
            [&](const ast::TypeNamed &named) -> std::string {
              if (named.type_args.has_value()) {
                unsupported("generic types", ty.span);
              }
              if (named.name == "int" || named.name == "i64") {
                return "std::int64_t";
              }
              if (named.name == "i32") {
                return "std::int32_t";
              }
              if (named.name == "u64") {
                return "std::uint64_t";
              }
              if (named.name == "u32") {
                return "std::uint32_t";
              }
              if (named.name == "bool") {
                return "bool";
              }
              if (named.name == "f64") {
                return "double";
              }
              if (named.name == "string") {
                return "std::string";
              }
              if (named.name == "char") {
                return "char32_t";
              }
              if (named.name == "unit") {
                return "::hew::unit";
              }
              unsupported("type `" + named.name + "`", ty.span);
            },
            [&](const auto &) -> std::string {
              unsupported("this type expression", ty.span);
            }},
        ty.value.kind);
  }

  std::string emitLiteral(const ast::Literal &literal) const {
    return std::visit(
        Overloaded{
            [](const ast::LitInteger &value) -> std::string {
              return std::to_string(value.value);
            },
            [](const ast::LitFloat &value) -> std::string {
              std::ostringstream out;
              out << std::setprecision(17) << value.value;
              return out.str();
            },
            [](const ast::LitString &value) -> std::string {
              return "std::string(" + quoteString(value.value) + ")";
            },
            [](const ast::LitBool &value) -> std::string {
              return value.value ? "true" : "false";
            },
            [](const ast::LitChar &value) -> std::string { return quoteChar(value.value); },
            [](const ast::LitDuration &value) -> std::string {
              return std::to_string(value.value);
            }},
        literal);
  }

  std::string emitCallArg(const ast::CallArg &arg, const ast::Span &span) const {
    if (const auto *positional = std::get_if<ast::CallArgPositional>(&arg)) {
      return emitExpr(*positional->expr);
    }
    unsupported("named call arguments", span);
  }

  std::string emitExpr(const ast::Spanned<ast::Expr> &expr) const {
    return std::visit(
        Overloaded{
            [&](const ast::ExprLiteral &literal) { return emitLiteral(literal.lit); },
            [&](const ast::ExprIdentifier &ident) {
              return sanitizeIdentifier(ident.name);
            },
            [&](const ast::ExprUnary &unary) {
              std::string op;
              switch (unary.op) {
              case ast::UnaryOp::Not:
                op = "!";
                break;
              case ast::UnaryOp::Negate:
                op = "-";
                break;
              case ast::UnaryOp::BitNot:
                op = "~";
                break;
              }
              return "(" + op + emitExpr(*unary.operand) + ")";
            },
            [&](const ast::ExprBinary &binary) {
              std::string op;
              switch (binary.op) {
              case ast::BinaryOp::Add:
                op = "+";
                break;
              case ast::BinaryOp::Subtract:
                op = "-";
                break;
              case ast::BinaryOp::Multiply:
                op = "*";
                break;
              case ast::BinaryOp::Divide:
                op = "/";
                break;
              case ast::BinaryOp::Modulo:
                op = "%";
                break;
              case ast::BinaryOp::Equal:
                op = "==";
                break;
              case ast::BinaryOp::NotEqual:
                op = "!=";
                break;
              case ast::BinaryOp::Less:
                op = "<";
                break;
              case ast::BinaryOp::LessEqual:
                op = "<=";
                break;
              case ast::BinaryOp::Greater:
                op = ">";
                break;
              case ast::BinaryOp::GreaterEqual:
                op = ">=";
                break;
              case ast::BinaryOp::And:
                op = "&&";
                break;
              case ast::BinaryOp::Or:
                op = "||";
                break;
              case ast::BinaryOp::BitAnd:
                op = "&";
                break;
              case ast::BinaryOp::BitOr:
                op = "|";
                break;
              case ast::BinaryOp::BitXor:
                op = "^";
                break;
              case ast::BinaryOp::Shl:
                op = "<<";
                break;
              case ast::BinaryOp::Shr:
                op = ">>";
                break;
              default:
                unsupported("binary operator", expr.span);
              }
              return "(" + emitExpr(*binary.left) + " " + op + " " +
                     emitExpr(*binary.right) + ")";
            },
            [&](const ast::ExprCall &call) {
              if (call.type_args.has_value()) {
                unsupported("explicit type arguments", expr.span);
              }
              std::vector<std::string> args;
              args.reserve(call.args.size());
              for (const auto &arg : call.args) {
                args.push_back(emitCallArg(arg, expr.span));
              }
              if (const auto *ident = std::get_if<ast::ExprIdentifier>(&call.function->value.kind)) {
                if (ident->name == "println") {
                  if (args.size() != 1) {
                    unsupported("println with arity other than 1", expr.span);
                  }
                  return "::hew::builtin_println(" + args.front() + ")";
                }
                if (ident->name == "print") {
                  if (args.size() != 1) {
                    unsupported("print with arity other than 1", expr.span);
                  }
                  return "::hew::builtin_print(" + args.front() + ")";
                }
              }
              std::ostringstream out;
              out << emitExpr(*call.function) << "(";
              for (size_t i = 0; i < args.size(); ++i) {
                if (i != 0) {
                  out << ", ";
                }
                out << args[i];
              }
              out << ")";
              return out.str();
            },
            [&](const ast::ExprBlock &block) { return emitBlockExpr(block.block); },
            [&](const ast::ExprIf &ifExpr) { return emitIfExpr(ifExpr, expr.span); },
            [&](const ast::ExprInterpolatedString &interpolated) {
              return emitInterpolatedString(interpolated);
            },
            [&](const ast::ExprCast &cast) {
              return "static_cast<" + emitType(cast.ty) + ">(" + emitExpr(*cast.expr) + ")";
            },
            [&](const auto &) -> std::string {
              unsupported("this expression", expr.span);
            }},
        expr.value.kind);
  }

  std::string emitInterpolatedString(const ast::ExprInterpolatedString &interpolated) const {
    std::ostringstream out;
    out << "([&]() {\n";
    emitLine(out, 1, "std::ostringstream hew_ss;");
    for (const auto &part : interpolated.parts) {
      std::visit(
          Overloaded{
              [&](const ast::StringPartLiteral &literal) {
                emitLine(out, 1, "hew_ss << " + quoteString(literal.text) + ";");
              },
              [&](const ast::StringPartExpr &expr) {
                emitLine(out, 1,
                         "::hew::append_to_stream(hew_ss, " + emitExpr(*expr.expr) + ");");
              }},
          part);
    }
    emitLine(out, 1, "return hew_ss.str();");
    out << "}())";
    return out.str();
  }

  void emitStatement(std::ostringstream &out, int indent, const ast::Spanned<ast::Stmt> &stmt,
                     ReturnKind returnKind) const {
    std::visit(
        Overloaded{
            [&](const ast::StmtLet &letStmt) {
              const auto *pattern = std::get_if<ast::PatIdentifier>(&letStmt.pattern.value.kind);
              if (pattern == nullptr) {
                unsupported("destructuring let bindings", stmt.span);
              }
              const std::string name = sanitizeIdentifier(pattern->name);
              if (letStmt.value.has_value()) {
                if (letStmt.ty.has_value()) {
                  emitLine(out, indent,
                           "const " + emitType(*letStmt.ty) + " " + name + " = " +
                               emitExpr(*letStmt.value) + ";");
                } else {
                  emitLine(out, indent,
                           "const auto " + name + " = " + emitExpr(*letStmt.value) + ";");
                }
                return;
              }
              if (!letStmt.ty.has_value()) {
                unsupported("let bindings without an initializer or type", stmt.span);
              }
              emitLine(out, indent, "const " + emitType(*letStmt.ty) + " " + name + "{};");
            },
            [&](const ast::StmtVar &varStmt) {
              const std::string name = sanitizeIdentifier(varStmt.name);
              if (varStmt.value.has_value()) {
                if (varStmt.ty.has_value()) {
                  emitLine(out, indent,
                           emitType(*varStmt.ty) + " " + name + " = " +
                               emitExpr(*varStmt.value) + ";");
                } else {
                  emitLine(out, indent,
                           "auto " + name + " = " + emitExpr(*varStmt.value) + ";");
                }
                return;
              }
              if (!varStmt.ty.has_value()) {
                unsupported("var bindings without an initializer or type", stmt.span);
              }
              emitLine(out, indent, emitType(*varStmt.ty) + " " + name + "{};");
            },
            [&](const ast::StmtAssign &assign) {
              std::string op = "=";
              if (assign.op.has_value()) {
                switch (*assign.op) {
                case ast::CompoundAssignOp::Add:
                  op = "+=";
                  break;
                case ast::CompoundAssignOp::Subtract:
                  op = "-=";
                  break;
                case ast::CompoundAssignOp::Multiply:
                  op = "*=";
                  break;
                case ast::CompoundAssignOp::Divide:
                  op = "/=";
                  break;
                case ast::CompoundAssignOp::Modulo:
                  op = "%=";
                  break;
                case ast::CompoundAssignOp::BitAnd:
                  op = "&=";
                  break;
                case ast::CompoundAssignOp::BitOr:
                  op = "|=";
                  break;
                case ast::CompoundAssignOp::BitXor:
                  op = "^=";
                  break;
                case ast::CompoundAssignOp::Shl:
                  op = "<<=";
                  break;
                case ast::CompoundAssignOp::Shr:
                  op = ">>=";
                  break;
                }
              }
              emitLine(out, indent, emitExpr(assign.target) + " " + op + " " +
                                       emitExpr(assign.value) + ";");
            },
            [&](const ast::StmtIf &ifStmt) { emitIfStatement(out, indent, ifStmt, returnKind); },
            [&](const ast::StmtWhile &whileStmt) {
              if (whileStmt.label.has_value()) {
                unsupported("labelled while loops", stmt.span);
              }
              emitLine(out, indent, "while (" + emitExpr(whileStmt.condition) + ") {");
              emitStatementBlock(out, indent + 1, whileStmt.body, returnKind);
              emitLine(out, indent, "}");
            },
            [&](const ast::StmtFor &forStmt) { emitForStatement(out, indent, forStmt, returnKind); },
            [&](const ast::StmtBreak &breakStmt) {
              if (breakStmt.label.has_value() || breakStmt.value.has_value()) {
                unsupported("labelled or value-carrying breaks", stmt.span);
              }
              emitLine(out, indent, "break;");
            },
            [&](const ast::StmtContinue &continueStmt) {
              if (continueStmt.label.has_value()) {
                unsupported("labelled continue", stmt.span);
              }
              emitLine(out, indent, "continue;");
            },
            [&](const ast::StmtReturn &returnStmt) {
              if (returnStmt.value.has_value()) {
                emitLine(out, indent, "return " + emitExpr(*returnStmt.value) + ";");
                return;
              }
              switch (returnKind) {
              case ReturnKind::Main:
                emitLine(out, indent, "return 0;");
                break;
              case ReturnKind::Unit:
                emitLine(out, indent, "return ::hew::unit{};");
                break;
              case ReturnKind::Value:
                unsupported("bare return in a value-returning context", stmt.span);
              }
            },
            [&](const ast::StmtExpression &exprStmt) {
              emitLine(out, indent, emitExpr(exprStmt.expr) + ";");
            },
            [&](const auto &) { unsupported("this statement", stmt.span); }},
        stmt.value.kind);
  }

  void emitStatementBlock(std::ostringstream &out, int indent, const ast::Block &block,
                          ReturnKind returnKind) const {
    for (const auto &stmt : block.stmts) {
      emitStatement(out, indent, *stmt, returnKind);
    }
    if (block.trailing_expr != nullptr) {
      emitLine(out, indent, emitExpr(*block.trailing_expr) + ";");
    }
  }

  void emitFunctionBody(std::ostringstream &out, int indent, const ast::Block &block,
                        ReturnKind returnKind) const {
    for (const auto &stmt : block.stmts) {
      emitStatement(out, indent, *stmt, returnKind);
    }
    if (block.trailing_expr != nullptr) {
      if (returnKind == ReturnKind::Unit || returnKind == ReturnKind::Main) {
        emitLine(out, indent, emitExpr(*block.trailing_expr) + ";");
      } else {
        emitLine(out, indent, "return " + emitExpr(*block.trailing_expr) + ";");
      }
    }
    if (returnKind == ReturnKind::Main) {
      emitLine(out, indent, "return 0;");
    }
  }

  std::string emitBlockExpr(const ast::Block &block) const {
    std::ostringstream out;
    out << "([&]() {\n";
    const ReturnKind returnKind =
        block.trailing_expr != nullptr ? ReturnKind::Value : ReturnKind::Unit;
    for (const auto &stmt : block.stmts) {
      emitStatement(out, 1, *stmt, returnKind);
    }
    if (block.trailing_expr != nullptr) {
      emitLine(out, 1, "return " + emitExpr(*block.trailing_expr) + ";");
    } else {
      emitLine(out, 1, "return ::hew::unit{};");
    }
    out << "}())";
    return out.str();
  }

  std::string emitIfExpr(const ast::ExprIf &ifExpr, const ast::Span &span) const {
    std::ostringstream out;
    out << "([&]() {\n";
    emitLine(out, 1, "if (" + emitExpr(*ifExpr.condition) + ") {");
    emitExprBranch(out, 2, *ifExpr.then_block);
    emitLine(out, 1, "}");
    if (ifExpr.else_block.has_value() && ifExpr.else_block->get() != nullptr) {
      emitLine(out, 1, "else {");
      emitExprBranch(out, 2, *ifExpr.else_block->get());
      emitLine(out, 1, "}");
    } else {
      emitLine(out, 1, "return ::hew::unit{};");
    }
    out << "}())";
    return out.str();
  }

  void emitExprBranch(std::ostringstream &out, int indent,
                      const ast::Spanned<ast::Expr> &expr) const {
    if (const auto *block = std::get_if<ast::ExprBlock>(&expr.value.kind)) {
      for (const auto &stmt : block->block.stmts) {
        emitStatement(out, indent, *stmt,
                      block->block.trailing_expr != nullptr ? ReturnKind::Value : ReturnKind::Unit);
      }
      if (block->block.trailing_expr != nullptr) {
        emitLine(out, indent, "return " + emitExpr(*block->block.trailing_expr) + ";");
      } else {
        emitLine(out, indent, "return ::hew::unit{};");
      }
      return;
    }
    emitLine(out, indent, "return " + emitExpr(expr) + ";");
  }

  void emitIfStatement(std::ostringstream &out, int indent, const ast::StmtIf &ifStmt,
                       ReturnKind returnKind) const {
    emitLine(out, indent, "if (" + emitExpr(ifStmt.condition) + ") {");
    emitStatementBlock(out, indent + 1, ifStmt.then_block, returnKind);
    emitLine(out, indent, "}");
    if (!ifStmt.else_block.has_value()) {
      return;
    }
    emitLine(out, indent, "else {");
    if (ifStmt.else_block->is_if) {
      if (ifStmt.else_block->if_stmt == nullptr) {
        unsupported("malformed else-if block", ifStmt.condition.span);
      }
      emitStatement(out, indent + 1, *ifStmt.else_block->if_stmt, returnKind);
    } else if (ifStmt.else_block->block.has_value()) {
      emitStatementBlock(out, indent + 1, *ifStmt.else_block->block, returnKind);
    }
    emitLine(out, indent, "}");
  }

  void emitForStatement(std::ostringstream &out, int indent, const ast::StmtFor &forStmt,
                        ReturnKind returnKind) const {
    if (forStmt.label.has_value()) {
      unsupported("labelled for loops", forStmt.iterable.span);
    }
    if (forStmt.is_await) {
      unsupported("for await loops", forStmt.iterable.span);
    }
    const auto *pattern = std::get_if<ast::PatIdentifier>(&forStmt.pattern.value.kind);
    if (pattern == nullptr) {
      unsupported("non-identifier for-loop patterns", forStmt.pattern.span);
    }
    const auto *range = std::get_if<ast::ExprRange>(&forStmt.iterable.value.kind);
    if (range == nullptr) {
      unsupported("for loops over non-range iterables", forStmt.iterable.span);
    }
    const std::string name = sanitizeIdentifier(pattern->name);
    const std::string start =
        range->start.has_value() ? emitExpr(*range->start->get()) : "0";
    std::string header = "for (auto " + name + " = " + start + "; ";
    if (range->end.has_value()) {
      header += name + (range->inclusive ? " <= " : " < ") +
                emitExpr(*range->end->get());
    }
    header += "; ++" + name + ") {";
    emitLine(out, indent, header);
    emitStatementBlock(out, indent + 1, forStmt.body, returnKind);
    emitLine(out, indent, "}");
  }

  void emitItem(std::ostringstream &out, const ast::Spanned<ast::Item> &item) const {
    if (std::holds_alternative<ast::ImportDecl>(item.value.kind)) {
      return;
    }
    if (const auto *fn = std::get_if<ast::FnDecl>(&item.value.kind)) {
      emitFunction(out, *fn, item.span);
      emitLine(out, 0);
      return;
    }
    unsupported("this top-level item", item.span);
  }

  void emitFunction(std::ostringstream &out, const ast::FnDecl &fn, const ast::Span &span) const {
    if (fn.is_async) {
      unsupported("async functions", span);
    }
    if (fn.is_generator) {
      unsupported("generator functions", span);
    }
    if (fn.type_params.has_value()) {
      unsupported("generic functions", span);
    }
    if (fn.where_clause.has_value()) {
      unsupported("where clauses", span);
    }
    if (fn.name == "main") {
      if (!fn.params.empty()) {
        unsupported("main functions with parameters", span);
      }
      if (!isUnitReturn(fn.return_type)) {
        unsupported("non-unit main return types", span);
      }
      emitLine(out, 0, "int main() {");
      emitFunctionBody(out, 1, fn.body, ReturnKind::Main);
      emitLine(out, 0, "}");
      return;
    }

    std::ostringstream signature;
    signature << (isUnitReturn(fn.return_type) ? "::hew::unit" : emitType(*fn.return_type)) << " "
              << sanitizeIdentifier(fn.name) << "(";
    for (size_t i = 0; i < fn.params.size(); ++i) {
      const auto &param = fn.params[i];
      if (i != 0) {
        signature << ", ";
      }
      signature << emitType(param.ty) << " " << sanitizeIdentifier(param.name);
    }
    signature << ") {";
    emitLine(out, 0, signature.str());
    emitFunctionBody(out, 1, fn.body, isUnitReturn(fn.return_type) ? ReturnKind::Unit
                                                                   : ReturnKind::Value);
    emitLine(out, 0, "}");
  }

  void emitPreamble(std::ostringstream &out) const {
    emitLine(out, 0, "// Generated by hew-codegen --emit-cpp");
    emitLine(out, 0, "#include <cstdint>");
    emitLine(out, 0, "#include <iostream>");
    emitLine(out, 0, "#include <sstream>");
    emitLine(out, 0, "#include <string>");
    emitLine(out, 0);
    emitLine(out, 0, "namespace hew {");
    emitLine(out, 1, "struct unit {};");
    emitLine(out, 1);
    emitLine(out, 1, "inline void append_to_stream(std::ostringstream &out, bool value) {");
    emitLine(out, 2, R"(out << (value ? "true" : "false");)");
    emitLine(out, 1, "}");
    emitLine(out, 1);
    emitLine(out, 1,
             "inline void append_to_stream(std::ostringstream &out, const std::string &value) {");
    emitLine(out, 2, "out << value;");
    emitLine(out, 1, "}");
    emitLine(out, 1);
    emitLine(out, 1, "inline void append_to_stream(std::ostringstream &, unit) {}");
    emitLine(out, 1);
    emitLine(out, 1,
             "template <typename T> inline void append_to_stream(std::ostringstream &out, "
             "const T &value) {");
    emitLine(out, 2, "out << value;");
    emitLine(out, 1, "}");
    emitLine(out, 1);
    emitLine(out, 1,
             "template <typename T> inline unit builtin_print(const T &value) {");
    emitLine(out, 2, "std::ostringstream buffer;");
    emitLine(out, 2, "::hew::append_to_stream(buffer, value);");
    emitLine(out, 2, "std::cout << buffer.str();");
    emitLine(out, 2, "return {};");
    emitLine(out, 1, "}");
    emitLine(out, 1);
    emitLine(out, 1,
             "template <typename T> inline unit builtin_println(const T &value) {");
    emitLine(out, 2, "std::ostringstream buffer;");
    emitLine(out, 2, "::hew::append_to_stream(buffer, value);");
    emitLine(out, 2, R"(std::cout << buffer.str() << '\n';)");
    emitLine(out, 2, "return {};");
    emitLine(out, 1, "}");
    emitLine(out, 0, "}");
    emitLine(out, 0);
  }
};

} // namespace

std::string emitCppSource(const ast::Program &program) {
  return CppEmitter{}.emit(program);
}

} // namespace hew
