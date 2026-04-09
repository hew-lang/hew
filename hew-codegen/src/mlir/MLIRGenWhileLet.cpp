//===- MLIRGenWhileLet.cpp - While-let codegen for Hew MLIRGen ------------===//
//
// While-let statement generation: generateWhileLetStmt
//
// while let Pattern = expr { body }
//
// Desugared to: loop { if let Pattern = expr { body } else { break } }
// We generate a loop-style scf::WhileOp (always-true before region) with the
// after region evaluating the scrutinee once, testing the pattern, and either
// binding variables + running the body or breaking.
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Diagnostics.h"

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringRef.h"

#include <string>

using namespace hew;
using namespace mlir;

// ============================================================================
// While-let statement generation
// ============================================================================

void MLIRGen::generateWhileLetStmt(const ast::StmtWhileLet &stmt) {
  auto location = currentLoc;

  if (!stmt.expr)
    return;

  const auto &pattern = stmt.pattern.value;
  auto *ctorPat = std::get_if<ast::PatConstructor>(&pattern.kind);
  if (!ctorPat) {
    emitError(location) << "while-let currently only supports constructor patterns";
    return;
  }

  const auto &ctorName = ctorPat->name;
  auto ctorVarIt = variantLookup.find(ctorName);
  if (ctorVarIt == variantLookup.end()) {
    emitError(location) << "unknown constructor '" << ctorName << "' in while-let pattern";
    return;
  }
  auto variantIndex = static_cast<int64_t>(ctorVarIt->second.second);

  auto lc = pushLoopControl(stmt.label, location);

  // Lowered as: loop { if tag_matches(expr) { bind; body } else { break } }
  // The "before" region just checks the activeFlag (same as `loop { }`).
  // The "after" region evaluates the scrutinee ONCE, tests the tag, and
  // either binds pattern variables + runs the body, or breaks.
  auto whileOp =
      mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

  // ── "before" region: check activeFlag ──
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);
  auto cond =
      mlir::memref::LoadOp::create(builder, location, lc.activeFlag, mlir::ValueRange{})
          .getResult();
  cond = andNotReturned(cond, location);
  mlir::scf::ConditionOp::create(builder, location, cond, mlir::ValueRange{});

  // ── "after" region: evaluate, match, bind, or break ──
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  auto falseVal = createIntConstant(builder, location, builder.getI1Type(), 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.continueFlag);

  // Evaluate the scrutinee once in this region.
  mlir::Value scrutinee = generateExpression(stmt.expr->value);
  if (!scrutinee) {
    // Expression failed — break immediately.
    mlir::memref::StoreOp::create(builder, location, falseVal, lc.activeFlag);
    ensureYieldTerminator(location);
    popLoopControl(lc, whileOp);
    return;
  }

  scrutinee = derefIndirectEnumScrutinee(scrutinee, stmt.expr->span, location);
  if (!scrutinee) {
    mlir::memref::StoreOp::create(builder, location, falseVal, lc.activeFlag);
    ensureYieldTerminator(location);
    popLoopControl(lc, whileOp);
    return;
  }
  mlir::Value tagMatch = emitTagEqualCondition(scrutinee, variantIndex, location);

  // Create scf.if: if tag matches, bind + body; else, break.
  auto ifOp = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, tagMatch,
                                       /*withElseRegion=*/true);

  // Then: pattern matched — bind variables and run the loop body.
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);
    pushDropScope();

    bindConstructorPatternVars(*ctorPat, scrutinee, location);
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(),
                                       lc.continueFlag, location);
    popDropScope();
  }
  ensureYieldTerminator(location);

  // Else: pattern did not match — break out of the loop.
  builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
  mlir::memref::StoreOp::create(builder, location, falseVal, lc.activeFlag);
  ensureYieldTerminator(location);

  // Continue after the if-op.
  builder.setInsertionPointAfter(ifOp);
  ensureYieldTerminator(location);

  popLoopControl(lc, whileOp);
}
