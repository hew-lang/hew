#include "hew/cpp_emitter.h"

#include <cassert>
#include <memory>
#include <string>

using namespace hew::ast;

namespace {

Span zeroSpan() { return Span{0, 0}; }

Spanned<TypeExpr> intType() {
  TypeExpr ty;
  ty.kind = TypeNamed{"int", std::nullopt};
  Spanned<TypeExpr> spanned;
  spanned.value = std::move(ty);
  spanned.span = zeroSpan();
  return spanned;
}

Spanned<Expr> intLiteral(int64_t value) {
  Expr expr;
  expr.kind = ExprLiteral{LitInteger{value}};
  expr.span = zeroSpan();
  Spanned<Expr> spanned;
  spanned.value = std::move(expr);
  spanned.span = zeroSpan();
  return spanned;
}

Spanned<Expr> identifier(std::string name) {
  Expr expr;
  expr.kind = ExprIdentifier{std::move(name)};
  expr.span = zeroSpan();
  Spanned<Expr> spanned;
  spanned.value = std::move(expr);
  spanned.span = zeroSpan();
  return spanned;
}

CallArg positionalArg(Spanned<Expr> expr) {
  return CallArgPositional{std::make_unique<Spanned<Expr>>(std::move(expr))};
}

Spanned<Expr> callExpr(std::string name, std::vector<CallArg> args) {
  Expr expr;
  expr.kind = ExprCall{std::make_unique<Spanned<Expr>>(identifier(std::move(name))),
                       std::nullopt, std::move(args), false};
  expr.span = zeroSpan();
  Spanned<Expr> spanned;
  spanned.value = std::move(expr);
  spanned.span = zeroSpan();
  return spanned;
}

Spanned<Expr> addExpr(Spanned<Expr> left, Spanned<Expr> right) {
  Expr expr;
  expr.kind =
      ExprBinary{std::make_unique<Spanned<Expr>>(std::move(left)), BinaryOp::Add,
                 std::make_unique<Spanned<Expr>>(std::move(right))};
  expr.span = zeroSpan();
  Spanned<Expr> spanned;
  spanned.value = std::move(expr);
  spanned.span = zeroSpan();
  return spanned;
}

Spanned<Stmt> exprStmt(Spanned<Expr> expr) {
  Stmt stmt;
  stmt.kind = StmtExpression{std::move(expr)};
  stmt.span = zeroSpan();
  Spanned<Stmt> spanned;
  spanned.value = std::move(stmt);
  spanned.span = zeroSpan();
  return spanned;
}

FnDecl makeAddFunction() {
  FnDecl fn{};
  fn.name = "add";
  fn.params.push_back(Param{"left", intType(), false});
  fn.params.push_back(Param{"right", intType(), false});
  fn.return_type = intType();
  fn.body.trailing_expr = std::make_unique<Spanned<Expr>>(
      addExpr(identifier("left"), identifier("right")));
  return fn;
}

FnDecl makeMainFunction() {
  FnDecl fn{};
  fn.name = "main";
  std::vector<CallArg> addArgs;
  addArgs.push_back(positionalArg(intLiteral(40)));
  addArgs.push_back(positionalArg(intLiteral(2)));
  std::vector<CallArg> printArgs;
  printArgs.push_back(positionalArg(callExpr("add", std::move(addArgs))));
  fn.body.stmts.push_back(
      std::make_unique<Spanned<Stmt>>(exprStmt(callExpr("println", std::move(printArgs)))));
  return fn;
}

} // namespace

int main() {
  Program program;
  Item addItem;
  addItem.kind = makeAddFunction();
  program.items.push_back(Spanned<Item>{std::move(addItem), zeroSpan()});

  Item mainItem;
  mainItem.kind = makeMainFunction();
  program.items.push_back(Spanned<Item>{std::move(mainItem), zeroSpan()});

  const auto generated = hew::emitCppSource(program);
  assert(generated.find("std::int64_t add(std::int64_t left, std::int64_t right)") !=
         std::string::npos);
  assert(generated.find("return (left + right);") != std::string::npos);
  assert(generated.find("int main()") != std::string::npos);
  assert(generated.find("::hew::builtin_println(add(40, 2));") != std::string::npos);
  return 0;
}
