#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <sstream>
#include <fstream>
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringRef.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/RecursiveASTVisitor.h"

#include "main.hpp"

// no nested starttime and stoptime
const char* declstr =
    "int getint();int getch();int getarray(int[]);void putint(int);void "
    "putch(int);void putarray(int, int[]);void starttime();void stoptime();";
std::vector<llvm::StringRef> sysyLibFunc{"getint",    "getch",   "getarray",
                                         "putint",    "putch",   "putarray",
                                         "starttime", "stoptime"};
using namespace llvm;
using namespace clang;
using namespace clang::tooling;
using namespace clang::ast_matchers;

using sideEffectMap = DenseMap<FunctionDecl*, DenseSet<SideEffect> >;

static cl::opt<std::string> inputFile(cl::Required, cl::Positional,
                                      cl::desc("input file"));

auto& operator<<(decltype(errs()) o, const SideEffect& se) {
  o << "at " << se.line << ":" << se.col << ", ";
  if (isa<ParmVarDecl>(se.sideVar)) {
    o << "param " << se.sideVar->getName() << " in pos " << se.paramPos << "\n";
  } else {
    o << se.sideVar->getName() << "\n";
  }
  return o;
}

VarDecl* getDeclFromRef(DeclRefExpr* ref) {
  auto sidevar = dyn_cast<VarDecl>(ref->getDecl());
  assert(sidevar &&
         "decl of any valid sysy file should only contain varDecl and "
         "FunctionDecl\n");
  return sidevar;
}

DeclRefExpr* getArrayDeclFromArrayExpr(ArraySubscriptExpr* arraySubExpr) {
  auto base = arraySubExpr->getBase()->IgnoreImpCasts();
  if (auto subExpr = dyn_cast<ArraySubscriptExpr>(base)) {
    return getArrayDeclFromArrayExpr(subExpr);
  }
  return cast<DeclRefExpr>(base);
}

/// currentFunc is used to record the function that we are visiting
/// the function has side effect if it changes global var or memory
/// of pointer parameters or call other function with side effect
class FindSideEffectFuncVisitor
    : public RecursiveASTVisitor<FindSideEffectFuncVisitor> {
 public:
  explicit FindSideEffectFuncVisitor(ASTContext* Context, sideEffectMap& m)
      : Context(Context), sideEffectFuncs(m) {}

  bool TraverseFunctionDecl(FunctionDecl* funcDecl) {
    assert(!currentFunc && "currentFunc should be null!");
    currentFunc = funcDecl;
    RecursiveASTVisitor<FindSideEffectFuncVisitor>::TraverseFunctionDecl(
        funcDecl);
    currentFunc = nullptr;
    return true;
  }

  bool VisitCallExpr(CallExpr* callExpr) {
    auto funcDecl = callExpr->getDirectCallee();
    assert(funcDecl && "only direct function call in sysY");
    funcDecl->getName();
    if (std::find(sysyLibFunc.begin(), sysyLibFunc.end(),
                  funcDecl->getName()) != sysyLibFunc.end() ||
        !sideEffectFuncs.count(funcDecl)) {
      return true;
    }
    for (const auto& sideEff : sideEffectFuncs[funcDecl]) {
      auto sideVar = sideEff.sideVar;
      if (dyn_cast<ParmVarDecl>(sideVar)) {
        // callArg is a arraySubscriptExpr or DeclRefExpr because
        // there is no pointer arithmetics in sysY
        auto callArg = callExpr->getArg(sideEff.paramPos)->IgnoreImpCasts();
        if (isa<ArraySubscriptExpr>(callArg)) {
          callArg =
              getArrayDeclFromArrayExpr(cast<ArraySubscriptExpr>(callArg));
        }
        auto argDecl = getDeclFromRef(cast<DeclRefExpr>(callArg));
        decltype(SideEffect::paramPos) pos = ~0u;
        if (argDecl->isLocalVarDecl()) {
          return true;
        }
        if (isa<ParmVarDecl>(argDecl)) {  // also a parameter
          auto iter =
              std::find(currentFunc->param_begin(), currentFunc->param_end(),
                        cast<ParmVarDecl>(argDecl));
          assert(iter != currentFunc->param_end() && "unknown param decl");
          pos = (decltype(pos))std::distance(currentFunc->param_begin(), iter);
        } else {
          assert(argDecl->hasExternalStorage() &&
                 "only local, param and global var in sysY");
        }
        sideEffectFuncs[currentFunc].insert(
            SideEffect{sideEff.col, sideEff.line, argDecl, pos});

      } else {
        sideEffectFuncs[currentFunc].insert(sideEff);
      }
    }
    return true;
  }
  bool VisitBinaryOperator(BinaryOperator* binOp) {
    if (binOp->getOpcode() != BO_Assign || !currentFunc) return true;
    auto lhs = binOp->getLHS();
    auto fullLoc = Context->getFullLoc(lhs->getBeginLoc());
    auto lineNum = fullLoc.getLineNumber(), colNum = fullLoc.getColumnNumber();

    if (auto declRef = dyn_cast<DeclRefExpr>(lhs)) {
      auto sidevar = getDeclFromRef(declRef);
      if (sidevar->hasExternalStorage()) {  // sidevar is global
        sideEffectFuncs[currentFunc].insert(
            SideEffect{lineNum, colNum, sidevar});
      }
    } else if (auto arraySubExpr = dyn_cast<ArraySubscriptExpr>(
                   lhs)) {  // left operand is an array element
      auto declRef = getArrayDeclFromArrayExpr(arraySubExpr);
      auto arrayVar = getDeclFromRef(declRef);
      if (arrayVar->hasExternalStorage()) {
        sideEffectFuncs[currentFunc].insert(
            SideEffect{lineNum, colNum, arrayVar});
      } else if (auto paramDecl = dyn_cast<ParmVarDecl>(arrayVar)) {
        auto iter = std::find(currentFunc->param_begin(),
                              currentFunc->param_end(), paramDecl);
        assert(iter != currentFunc->param_end() && "unknown param decl");
        sideEffectFuncs[currentFunc].insert(SideEffect{
            lineNum, colNum, arrayVar,
            (uint32_t)std::distance(currentFunc->param_begin(), iter)});
      }
    } else {
      llvm_unreachable("only array element or variable can be lvalue in sysY");
    }
    return true;
  }

 private:
  friend class UBCheckConsumer;
  FunctionDecl* currentFunc = nullptr;
  sideEffectMap& sideEffectFuncs;
  ASTContext* Context;
};

class FindUBVisitor : public RecursiveASTVisitor<FindUBVisitor> {
 public:
  explicit FindUBVisitor(ASTContext* Context, sideEffectMap& m)
      : Context(Context), sideEffectFuncs(m) {}

 private:
  friend class UBCheckConsumer;
  sideEffectMap& sideEffectFuncs;
  ASTContext* Context;
};

class UBCheckConsumer : public clang::ASTConsumer {
 public:
  explicit UBCheckConsumer(ASTContext* Context)
      : effectMap(),
        Visitor(Context, effectMap),
        UBVisitor(Context, effectMap) {}

  virtual void HandleTranslationUnit(clang::ASTContext& Context) {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
#ifndef NDEBUG

    for (const auto& p : effectMap) {
      auto f = p.getFirst();
      errs() << "/*   Side Effect of func " << f->getName() << " start   */\n";
      for (auto& effects : p.getSecond()) {
        errs() << effects;
      }
      errs() << "/*   END   */\n";
    }
#endif
  }

 private:
  FindSideEffectFuncVisitor Visitor;
  FindUBVisitor UBVisitor;
  sideEffectMap effectMap;
};

class FindSideEffectFuncAction : public ASTFrontendAction {
 public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance& instance,
                                                 StringRef inputF) override {
    return std::make_unique<UBCheckConsumer>(&instance.getASTContext());
  }
};

int main(int argc, const char** argv) {
  cl::ParseCommandLineOptions(
      argc, argv, "a tool used to check sysY UB about order of evaluation");
  std::fstream file(inputFile, file.in);
  std::ostringstream code(declstr);
  if (!file) {
    errs() << "open input file failed\n";
    return 1;
  }
  code << file.rdbuf();
  runToolOnCode(std::make_unique<FindSideEffectFuncAction>(), code.str(),
                inputFile);
  return 0;
}
