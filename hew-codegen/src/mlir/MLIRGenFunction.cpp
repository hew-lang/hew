//===- MLIRGenFunction.cpp - Function lowering for Hew MLIRGen ------------===//
//
// Split function-generation helpers for the MLIR generator.
//
//===----------------------------------------------------------------------===//

#include "hew/ast_helpers.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Location.h"
#include "mlir/IR/Value.h"

#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/StringRef.h"

#include <functional>
#include <unordered_set>

using namespace hew;
using namespace mlir;

namespace {
constexpr llvm::StringLiteral kHewDebugParamNameAttr = "hew.debug.param_name";
constexpr llvm::StringLiteral kHewExplicitDynParamAttr = "hew.explicit_dyn_param";

void setDebugParamNameAttrs(mlir::func::FuncOp funcOp, const std::vector<hew::ast::Param> &params,
                            mlir::Builder &builder) {
  for (size_t i = 0; i < params.size(); ++i)
    funcOp.setArgAttr(i, kHewDebugParamNameAttr, builder.getStringAttr(params[i].name));
}

void setExplicitDynParamAttrs(mlir::func::FuncOp funcOp, const std::vector<hew::ast::Param> &params,
                              mlir::Builder &builder) {
  for (size_t i = 0; i < params.size(); ++i) {
    if (std::holds_alternative<hew::ast::TypeTraitObject>(params[i].ty.value.kind)) {
      funcOp.setArgAttr(i, kHewExplicitDynParamAttr, builder.getBoolAttr(true));
    }
  }
}
} // namespace

bool MLIRGen::functionProducesValue(const ast::FnDecl &fn) const {
  if (!fn.return_type)
    return false;
  if (auto *named = std::get_if<ast::TypeNamed>(&fn.return_type->value.kind)) {
    return named->name != "()" && named->name != "unit" && named->name != "void" &&
           named->name != "Never";
  }
  return true;
}

void MLIRGen::bindFunctionParameters(const ast::FnDecl &fn, mlir::Block *entryBlock) {
  uint32_t paramIdx = 0;
  for (const auto &param : fn.params) {
    const auto &paramName = param.name;
    auto paramValue = entryBlock->getArgument(paramIdx);
    bindParam(param, paramValue);

    const auto &paramTy = param.ty.value;
    auto resolveAliasExpr = [this](llvm::StringRef name) { return resolveTypeAliasExpr(name); };
    auto handleStr = typeExprToHandleString(paramTy, knownHandleTypes, resolveAliasExpr);
    if (!handleStr.empty()) {
      handleVarTypes[paramName] = handleStr;
      if (auto bindingIdentity = resolveCurrentBindingIdentity(paramName))
        annotatedHandleTypes[bindingIdentity] = &paramTy;
    }
    if (auto streamInfo = streamHandleInfoFromTypeExpr(paramTy))
      rememberTrackedStreamHandleInfo(paramName, *streamInfo);
    auto actorName = typeExprToActorName(paramTy, resolveAliasExpr);
    if (!actorName.empty() && actorRegistry.count(actorName))
      actorVarTypes[paramName] = actorName;

    if (auto *traitObj = std::get_if<ast::TypeTraitObject>(&paramTy.kind)) {
      if (!traitObj->bounds.empty())
        dynTraitVarTypes[paramName] = traitObj->bounds[0].name;
    }
    ++paramIdx;
  }
}

void MLIRGen::collectExplicitReturnExcludeVars(const ast::Block &block, bool &hasNestedReturn) {
  std::function<void(const ast::Block &)> scanReturns;
  std::function<void(const ast::StmtIf &)> scanReturnsIf;
  scanReturnsIf = [&scanReturns, &scanReturnsIf](const ast::StmtIf &ifStmt) {
    scanReturns(ifStmt.then_block);
    if (ifStmt.else_block) {
      if (ifStmt.else_block->block)
        scanReturns(*ifStmt.else_block->block);
      if (ifStmt.else_block->if_stmt) {
        auto &nested = ifStmt.else_block->if_stmt->value;
        if (auto *nestedIf = std::get_if<ast::StmtIf>(&nested.kind))
          scanReturnsIf(*nestedIf);
      }
    }
  };
  scanReturns = [&scanReturns, &scanReturnsIf, &hasNestedReturn, this](const ast::Block &blk) {
    for (const auto &stmt : blk.stmts) {
      if (auto *retStmt = std::get_if<ast::StmtReturn>(&stmt->value.kind)) {
        hasNestedReturn = true;
        if (retStmt->value) {
          ExcludeSet tmp;
          collectExcludeVars(retStmt->value->value, tmp, 0);
          for (const auto &[name, d] : tmp)
            funcLevelEarlyReturnVarNames.insert(name);
        }
      } else if (auto *ifStmt = std::get_if<ast::StmtIf>(&stmt->value.kind)) {
        scanReturnsIf(*ifStmt);
      } else if (auto *forStmt = std::get_if<ast::StmtFor>(&stmt->value.kind)) {
        scanReturns(forStmt->body);
      } else if (auto *whileStmt = std::get_if<ast::StmtWhile>(&stmt->value.kind)) {
        scanReturns(whileStmt->body);
      }
    }
  };
  scanReturns(block);
}

void MLIRGen::queueFunctionParamDrops(const ast::FnDecl &fn, llvm::StringRef funcName) {
  auto isBorrowSemanticsDrop = [](const std::string &df) {
    return df == "hew_vec_free" || df == "hew_hashmap_free_impl" || df == "hew_hashset_free" ||
           df == "hew_rc_drop" || df == "hew_string_drop" || df.starts_with("__hew_drop_");
  };
  for (const auto &param : fn.params) {
    if (handleVarTypes.count(param.name) || lookupTrackedStreamHandleInfo(param.name))
      continue;
    auto dropFunc = dropFuncForType(param.ty.value);
    if (dropFunc.empty() || isBorrowSemanticsDrop(dropFunc))
      continue;
    if (dropFunc == "__auto_field_drop") {
      auto *named = std::get_if<ast::TypeNamed>(&param.ty.value.kind);
      if (!named || !isHandleBearingStruct(resolveTypeAlias(named->name)))
        continue;
    }
    if (dropFunc == funcName)
      continue;
    bool isUserDrop = false;
    if (auto *named = std::get_if<ast::TypeNamed>(&param.ty.value.kind)) {
      auto typeName = resolveTypeAlias(named->name);
      isUserDrop = userDropFuncs.count(typeName) > 0;
    }
    pendingFunctionParamDrops.push_back({param.name, dropFunc, isUserDrop});
  }
}

void MLIRGen::prepareFunctionDropAnalysis(const ast::FnDecl &fn, llvm::StringRef funcName,
                                          bool fnProducesValue, bool &hasNestedReturn) {
  funcLevelDropExcludeVars.clear();
  funcLevelDropScopeBase = dropScopes.size();
  collectExcludeVarsFromBlock(fn.body, funcLevelDropExcludeVars, 0, fnProducesValue);

  funcLevelReturnVarNames.clear();
  for (const auto &[name, depth] : funcLevelDropExcludeVars)
    funcLevelReturnVarNames.insert(name);
  resolveFunctionDropExclusionCandidates();

  funcLevelEarlyReturnVarNames.clear();
  hasNestedReturn = false;
  collectExplicitReturnExcludeVars(fn.body, hasNestedReturn);
  queueFunctionParamDrops(fn, funcName);
}

bool MLIRGen::validateFunctionReturnDrops(const ast::FnDecl &fn, bool fnProducesValue,
                                          mlir::Location location) {
  if (pendingFunctionParamDrops.empty() || !fnProducesValue)
    return true;

  std::unordered_set<std::string> droppedParamNames;
  for (const auto &pd : pendingFunctionParamDrops)
    droppedParamNames.insert(pd.name);

  std::function<bool(const ast::Expr &)> isFieldOfDroppedParam;
  std::function<bool(const ast::Block &)> blockValueYieldsFieldOfDroppedParam;
  std::function<bool(const ast::StmtIf &)> stmtIfValueYieldsFieldOfDroppedParam;
  std::function<bool(const ast::Block &, const std::optional<std::string> &, bool)>
      blockBreakValueYieldsFieldOfDroppedParam;
  std::function<bool(const ast::Expr &, const std::optional<std::string> &, bool)>
      exprBreakValueYieldsFieldOfDroppedParam;
  std::function<bool(const ast::StmtIf &, const std::optional<std::string> &, bool)>
      stmtIfBreakValueYieldsFieldOfDroppedParam;

  stmtIfValueYieldsFieldOfDroppedParam = [&](const ast::StmtIf &ifStmt) -> bool {
    if (blockValueYieldsFieldOfDroppedParam(ifStmt.then_block))
      return true;
    if (ifStmt.else_block) {
      if (ifStmt.else_block->block &&
          blockValueYieldsFieldOfDroppedParam(*ifStmt.else_block->block))
        return true;
      if (ifStmt.else_block->if_stmt) {
        if (auto *nested = std::get_if<ast::StmtIf>(&ifStmt.else_block->if_stmt->value.kind))
          if (stmtIfValueYieldsFieldOfDroppedParam(*nested))
            return true;
        if (auto *nestedIfLet =
                std::get_if<ast::StmtIfLet>(&ifStmt.else_block->if_stmt->value.kind)) {
          if (blockValueYieldsFieldOfDroppedParam(nestedIfLet->body))
            return true;
          if (nestedIfLet->else_body &&
              blockValueYieldsFieldOfDroppedParam(*nestedIfLet->else_body))
            return true;
        }
      }
    }
    return false;
  };
  stmtIfBreakValueYieldsFieldOfDroppedParam = [&](const ast::StmtIf &ifStmt,
                                                  const std::optional<std::string> &targetLabel,
                                                  bool checkUnlabeled) -> bool {
    if (blockBreakValueYieldsFieldOfDroppedParam(ifStmt.then_block, targetLabel, checkUnlabeled))
      return true;
    if (ifStmt.else_block) {
      if (ifStmt.else_block->block && blockBreakValueYieldsFieldOfDroppedParam(
                                          *ifStmt.else_block->block, targetLabel, checkUnlabeled))
        return true;
      if (ifStmt.else_block->if_stmt) {
        if (auto *nested = std::get_if<ast::StmtIf>(&ifStmt.else_block->if_stmt->value.kind))
          if (stmtIfBreakValueYieldsFieldOfDroppedParam(*nested, targetLabel, checkUnlabeled))
            return true;
        if (auto *nestedIfLet =
                std::get_if<ast::StmtIfLet>(&ifStmt.else_block->if_stmt->value.kind)) {
          if (blockBreakValueYieldsFieldOfDroppedParam(nestedIfLet->body, targetLabel,
                                                       checkUnlabeled))
            return true;
          if (nestedIfLet->else_body && blockBreakValueYieldsFieldOfDroppedParam(
                                            *nestedIfLet->else_body, targetLabel, checkUnlabeled))
            return true;
        }
      }
    }
    return false;
  };
  exprBreakValueYieldsFieldOfDroppedParam = [&](const ast::Expr &expr,
                                                const std::optional<std::string> &targetLabel,
                                                bool checkUnlabeled) -> bool {
    if (auto *blockE = std::get_if<ast::ExprBlock>(&expr.kind))
      return blockBreakValueYieldsFieldOfDroppedParam(blockE->block, targetLabel, checkUnlabeled);
    if (auto *unsafeE = std::get_if<ast::ExprUnsafe>(&expr.kind))
      return blockBreakValueYieldsFieldOfDroppedParam(unsafeE->block, targetLabel, checkUnlabeled);
    if (auto *scopeE = std::get_if<ast::ExprScope>(&expr.kind))
      return blockBreakValueYieldsFieldOfDroppedParam(scopeE->block, targetLabel, checkUnlabeled);
    if (auto *ifE = std::get_if<ast::ExprIf>(&expr.kind)) {
      if (ifE->then_block && exprBreakValueYieldsFieldOfDroppedParam(ifE->then_block->value,
                                                                     targetLabel, checkUnlabeled))
        return true;
      if (ifE->else_block && *ifE->else_block &&
          exprBreakValueYieldsFieldOfDroppedParam((*ifE->else_block)->value, targetLabel,
                                                  checkUnlabeled))
        return true;
      return false;
    }
    if (auto *ifLet = std::get_if<ast::ExprIfLet>(&expr.kind)) {
      if (blockBreakValueYieldsFieldOfDroppedParam(ifLet->body, targetLabel, checkUnlabeled))
        return true;
      if (ifLet->else_body &&
          blockBreakValueYieldsFieldOfDroppedParam(*ifLet->else_body, targetLabel, checkUnlabeled))
        return true;
      return false;
    }
    if (auto *matchE = std::get_if<ast::ExprMatch>(&expr.kind)) {
      for (const auto &arm : matchE->arms)
        if (arm.body &&
            exprBreakValueYieldsFieldOfDroppedParam(arm.body->value, targetLabel, checkUnlabeled))
          return true;
      return false;
    }
    return false;
  };
  blockBreakValueYieldsFieldOfDroppedParam = [&](const ast::Block &blk,
                                                 const std::optional<std::string> &targetLabel,
                                                 bool checkUnlabeled) -> bool {
    for (const auto &stmt : blk.stmts) {
      if (auto *brk = std::get_if<ast::StmtBreak>(&stmt->value.kind)) {
        if (brk->value) {
          bool targets_us = false;
          if (!brk->label && checkUnlabeled)
            targets_us = true;
          if (brk->label && targetLabel && *brk->label == *targetLabel)
            targets_us = true;
          if (targets_us && isFieldOfDroppedParam(brk->value->value))
            return true;
        }
        continue;
      }
      if (auto *ifStmt = std::get_if<ast::StmtIf>(&stmt->value.kind)) {
        if (stmtIfBreakValueYieldsFieldOfDroppedParam(*ifStmt, targetLabel, checkUnlabeled))
          return true;
        continue;
      }
      if (auto *ifLetStmt = std::get_if<ast::StmtIfLet>(&stmt->value.kind)) {
        if (blockBreakValueYieldsFieldOfDroppedParam(ifLetStmt->body, targetLabel, checkUnlabeled))
          return true;
        if (ifLetStmt->else_body && blockBreakValueYieldsFieldOfDroppedParam(
                                        *ifLetStmt->else_body, targetLabel, checkUnlabeled))
          return true;
        continue;
      }
      if (auto *matchStmt = std::get_if<ast::StmtMatch>(&stmt->value.kind)) {
        for (const auto &arm : matchStmt->arms)
          if (arm.body &&
              exprBreakValueYieldsFieldOfDroppedParam(arm.body->value, targetLabel, checkUnlabeled))
            return true;
        continue;
      }
      if (auto *exprStmt = std::get_if<ast::StmtExpression>(&stmt->value.kind)) {
        if (exprBreakValueYieldsFieldOfDroppedParam(exprStmt->expr.value, targetLabel,
                                                    checkUnlabeled))
          return true;
        continue;
      }
      if (auto *innerLoop = std::get_if<ast::StmtLoop>(&stmt->value.kind)) {
        const auto innerTarget =
            (innerLoop->label && targetLabel && *innerLoop->label == *targetLabel)
                ? std::optional<std::string>{}
                : targetLabel;
        if (blockBreakValueYieldsFieldOfDroppedParam(innerLoop->body, innerTarget, false))
          return true;
        continue;
      }
      if (auto *innerWhile = std::get_if<ast::StmtWhile>(&stmt->value.kind)) {
        const auto innerTarget =
            (innerWhile->label && targetLabel && *innerWhile->label == *targetLabel)
                ? std::optional<std::string>{}
                : targetLabel;
        if (blockBreakValueYieldsFieldOfDroppedParam(innerWhile->body, innerTarget, false))
          return true;
        continue;
      }
      if (auto *innerWhileLet = std::get_if<ast::StmtWhileLet>(&stmt->value.kind)) {
        const auto innerTarget =
            (innerWhileLet->label && targetLabel && *innerWhileLet->label == *targetLabel)
                ? std::optional<std::string>{}
                : targetLabel;
        if (blockBreakValueYieldsFieldOfDroppedParam(innerWhileLet->body, innerTarget, false))
          return true;
        continue;
      }
      if (auto *innerFor = std::get_if<ast::StmtFor>(&stmt->value.kind)) {
        const auto innerTarget =
            (innerFor->label && targetLabel && *innerFor->label == *targetLabel)
                ? std::optional<std::string>{}
                : targetLabel;
        if (blockBreakValueYieldsFieldOfDroppedParam(innerFor->body, innerTarget, false))
          return true;
        continue;
      }
    }
    return false;
  };
  blockValueYieldsFieldOfDroppedParam = [&](const ast::Block &blk) -> bool {
    if (blk.trailing_expr)
      return isFieldOfDroppedParam(blk.trailing_expr->value);
    if (!blk.stmts.empty()) {
      const auto &last = blk.stmts.back()->value;
      if (auto *exprStmt = std::get_if<ast::StmtExpression>(&last.kind))
        return isFieldOfDroppedParam(exprStmt->expr.value);
      if (auto *ifStmt = std::get_if<ast::StmtIf>(&last.kind))
        return stmtIfValueYieldsFieldOfDroppedParam(*ifStmt);
      if (auto *ifLetStmt = std::get_if<ast::StmtIfLet>(&last.kind)) {
        if (blockValueYieldsFieldOfDroppedParam(ifLetStmt->body))
          return true;
        if (ifLetStmt->else_body && blockValueYieldsFieldOfDroppedParam(*ifLetStmt->else_body))
          return true;
      }
      if (auto *matchStmt = std::get_if<ast::StmtMatch>(&last.kind)) {
        for (const auto &arm : matchStmt->arms)
          if (arm.body && isFieldOfDroppedParam(arm.body->value))
            return true;
      }
      if (auto *loopStmt = std::get_if<ast::StmtLoop>(&last.kind))
        return blockBreakValueYieldsFieldOfDroppedParam(loopStmt->body, loopStmt->label, true);
      if (auto *whileStmt = std::get_if<ast::StmtWhile>(&last.kind))
        return blockBreakValueYieldsFieldOfDroppedParam(whileStmt->body, whileStmt->label, true);
      if (auto *whileLetStmt = std::get_if<ast::StmtWhileLet>(&last.kind))
        return blockBreakValueYieldsFieldOfDroppedParam(whileLetStmt->body, whileLetStmt->label,
                                                        true);
      if (auto *forStmt = std::get_if<ast::StmtFor>(&last.kind))
        return blockBreakValueYieldsFieldOfDroppedParam(forStmt->body, forStmt->label, true);
    }
    return false;
  };
  isFieldOfDroppedParam = [&](const ast::Expr &expr) -> bool {
    return exprYieldsFieldMatching(expr, [&](const ast::ExprFieldAccess &fieldAccess) {
      auto *id = std::get_if<ast::ExprIdentifier>(&fieldAccess.object->value.kind);
      return id && droppedParamNames.count(id->name);
    });
  };

  if (blockValueYieldsFieldOfDroppedParam(fn.body)) {
    ++errorCount_;
    emitError(location) << "returning a field of an owned parameter is not yet supported "
                        << "(field-alias ownership tracking is required); "
                        << "return the whole parameter or clone the field instead";
    return false;
  }

  std::function<bool(const ast::Block &)> scanForFieldReturn;
  std::function<bool(const ast::StmtIf &)> scanForFieldReturnIf;
  scanForFieldReturnIf = [&](const ast::StmtIf &ifStmt) -> bool {
    if (scanForFieldReturn(ifStmt.then_block))
      return true;
    if (ifStmt.else_block) {
      if (ifStmt.else_block->block && scanForFieldReturn(*ifStmt.else_block->block))
        return true;
      if (ifStmt.else_block->if_stmt) {
        if (auto *nested = std::get_if<ast::StmtIf>(&ifStmt.else_block->if_stmt->value.kind))
          if (scanForFieldReturnIf(*nested))
            return true;
      }
    }
    return false;
  };
  scanForFieldReturn = [&](const ast::Block &blk) -> bool {
    for (const auto &stmt : blk.stmts) {
      if (auto *ret = std::get_if<ast::StmtReturn>(&stmt->value.kind)) {
        if (ret->value && isFieldOfDroppedParam(ret->value->value)) {
          ++errorCount_;
          emitError(location) << "returning a field of an owned parameter is not yet supported "
                              << "(field-alias ownership tracking is required); "
                              << "return the whole parameter or clone the field instead";
          return true;
        }
      } else if (auto *ifStmt = std::get_if<ast::StmtIf>(&stmt->value.kind)) {
        if (scanForFieldReturnIf(*ifStmt))
          return true;
      } else if (auto *forStmt = std::get_if<ast::StmtFor>(&stmt->value.kind)) {
        if (scanForFieldReturn(forStmt->body))
          return true;
      } else if (auto *whileStmt = std::get_if<ast::StmtWhile>(&stmt->value.kind)) {
        if (scanForFieldReturn(whileStmt->body))
          return true;
      }
    }
    return false;
  };
  return !scanForFieldReturn(fn.body);
}

mlir::func::FuncOp MLIRGen::generateFunction(const ast::FnDecl &fn, const std::string &nameOverride,
                                             std::optional<mlir::Location> fallbackLoc) {
  SymbolTableScopeT varScope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);

  auto typeLoc = [&](const ast::Spanned<ast::TypeExpr> &type) {
    return type.span.end > type.span.start ? loc(type.span) : fallbackLoc.value_or(currentLoc);
  };
  llvm::SmallVector<mlir::Type, 4> paramTypes;
  for (const auto &param : fn.params)
    paramTypes.push_back(convertType(param.ty.value, typeLoc(param.ty)));

  llvm::SmallVector<mlir::Type, 1> resultTypes;
  if (fn.return_type) {
    auto retTy = convertType(fn.return_type->value, typeLoc(*fn.return_type));
    if (!llvm::isa<mlir::NoneType>(retTy))
      resultTypes.push_back(retTy);
  }

  auto location = [&]() -> mlir::Location {
    if (fn.decl_span && fn.decl_span->end > fn.decl_span->start)
      return loc(*fn.decl_span);
    return fallbackLoc.value_or(currentLoc);
  }();
  std::string funcName =
      nameOverride.empty() ? mangleName(currentModulePath, "", fn.name) : nameOverride;
  bool isImplicitMainReturn = funcName == "main" && resultTypes.empty();
  auto funcType = builder.getFunctionType(paramTypes, resultTypes);

  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(funcName)) {
    if (existing.isDeclaration())
      existing.erase();
    else
      return existing;
  }

  auto savedIP = builder.saveInsertionPoint();
  auto restoreIP = llvm::scope_exit([&] { builder.restoreInsertionPoint(savedIP); });
  builder.setInsertionPointToEnd(module.getBody());
  auto funcOp = mlir::func::FuncOp::create(builder, location, funcName, funcType);
  setDebugParamNameAttrs(funcOp, fn.params, builder);
  setExplicitDynParamAttrs(funcOp, fn.params, builder);
  if (funcName != "main") {
    funcOp.setVisibility(ast::is_pub(fn.visibility) ? mlir::SymbolTable::Visibility::Public
                                                    : mlir::SymbolTable::Visibility::Private);
  }

  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  FunctionGenerationScope funcScope(*this, funcOp);
  currentFunctionReturnTypeExpr = fn.return_type ? &fn.return_type->value : nullptr;
  bindFunctionParameters(fn, entryBlock);
  initReturnFlagAndSlot(resultTypes, location);

  bool hasNestedReturn = false;
  bool fnProducesValue = functionProducesValue(fn);
  prepareFunctionDropAnalysis(fn, funcName, fnProducesValue, hasNestedReturn);
  if (!validateFunctionReturnDrops(fn, fnProducesValue, location))
    return nullptr;

  if (hasNestedReturn && returnFlag && !returnSlot)
    ensureReturnSlot(location);

  bool finalStmtNeedsStatementLowering = false;
  if (!fn.return_type && hasNestedReturn && !fn.body.trailing_expr && !fn.body.stmts.empty()) {
    const auto &lastStmt = fn.body.stmts.back()->value;
    finalStmtNeedsStatementLowering = std::holds_alternative<ast::StmtIf>(lastStmt.kind) ||
                                      std::holds_alternative<ast::StmtMatch>(lastStmt.kind);
  }
  bool bodyResultDiscarded = isImplicitMainReturn || (fn.return_type && resultTypes.empty()) ||
                             finalStmtNeedsStatementLowering;
  mlir::Value bodyValue = generateBlock(fn.body, /*statementPosition=*/bodyResultDiscarded,
                                        /*isFunctionBodyBlock=*/true);
  funcLevelDropExcludeVars.clear();
  funcLevelReturnVarNames.clear();
  funcLevelEarlyReturnVarNames.clear();

  if (!isImplicitMainReturn && resultTypes.empty() && bodyValue && bodyValue.getType() &&
      !llvm::isa<mlir::NoneType>(bodyValue.getType())) {
    resultTypes.push_back(bodyValue.getType());
    funcOp.setFunctionType(builder.getFunctionType(paramTypes, resultTypes));
  }

  auto *currentBlock = builder.getInsertionBlock();
  if (!currentBlock ||
      (!currentBlock->empty() && currentBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()))
    return funcOp;

  if (returnFlag && returnSlot && !resultTypes.empty()) {
    auto flagVal = mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
    auto selectOp = mlir::scf::IfOp::create(builder, location, resultTypes[0], flagVal,
                                            /*withElseRegion=*/true);

    builder.setInsertionPointToStart(&selectOp.getThenRegion().front());
    auto slotVal =
        mlir::memref::LoadOp::create(builder, location, returnSlot, mlir::ValueRange{}).getResult();
    slotVal = coerceTypeForSink(slotVal, resultTypes[0], location);
    mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{slotVal});

    builder.setInsertionPointToStart(&selectOp.getElseRegion().front());
    mlir::Value normalValue =
        bodyValue ? bodyValue : createDefaultValue(builder, location, resultTypes[0]);
    normalValue = coerceTypeForSink(normalValue, resultTypes[0], location);
    mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{normalValue});

    builder.setInsertionPointAfter(selectOp);
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{selectOp.getResult(0)});
    return funcOp;
  }

  if (bodyValue && !resultTypes.empty()) {
    if (!funcLevelDropExcludeValues.empty())
      emitDropsExcept(funcLevelDropExcludeValues);
    else
      emitAllDrops();
    auto coercedBody = coerceTypeForSink(bodyValue, resultTypes[0], location);
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{coercedBody});
    return funcOp;
  }

  emitAllDrops();
  if (isImplicitMainReturn) {
    resultTypes.push_back(builder.getI32Type());
    funcOp.setFunctionType(builder.getFunctionType(paramTypes, resultTypes));
    auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{zero});
  } else {
    mlir::func::ReturnOp::create(builder, location);
  }
  return funcOp;
}
