#include <iostream>
#include <string>
#include <unordered_set>
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/DenseSet.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/RecursiveASTVisitor.h"

#include "main.hpp"

// no nested starttime and stoptime
const char* declstr =
    "int getint();int getch();int getarray(int[]);void putint(int);void "
    "putch(int);void putarray(int, int[]);void starttime();void stoptime();";
using namespace llvm;
using namespace clang;
using namespace clang::tooling;
using namespace clang::ast_matchers;

static cl::OptionCategory ToolCategory("Tool options");
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp("\nMore help text...\n");

#if 0
StatementMatcher funcMatcher =
    forStmt(hasLoopInit(declStmt(hasSingleDecl(
                varDecl(hasInitializer(integerLiteral(equals(0))))))))
        .bind("for");



class LoopPrinter : public MatchFinder::MatchCallback {
 public:
  virtual void run(const MatchFinder::MatchResult& Result) {
    const ForStmt* forstmt = Result.Nodes.getNodeAs<ForStmt>("for");
    if (forstmt) {
      errs() << "addr: " << forstmt << "\n";
      forstmt->dump();
  }}
};
#endif

VarDecl* getDeclFromRef(DeclRefExpr* ref) {
  auto sidevar = dyn_cast<VarDecl>(ref->getDecl());
  assert(sidevar &&
         "decl of any valid sysy file should only contain varDecl and "
         "FunctionDecl\n");
  return sidevar;
}

DeclRefExpr* getArrayDeclFromArrayExpr(ArraySubscriptExpr* arraySubExpr) {
  DeclRefExpr* arrayRef = nullptr;
  while(!arrayRef){
    auto base = arraySubExpr->getBase();
    if(auto castExpr = dyn_cast<ImplicitCastExpr>(base)){
      base = castExpr->getSubExpr();
    }
    if(auto subExpr = dyn_cast<ArraySubscriptExpr>(base)){
      return getArrayDeclFromArrayExpr(subExpr);
    }
    arrayRef = dyn_cast<DeclRefExpr>(base);
    
  }
}

/// currentFunc is used to record the function that we are visiting
/// the function has side effect if it changes global var or memory
/// of pointer parameters or call other function with side effect
class FindSideEffectFuncVisitor
    : public RecursiveASTVisitor<FindSideEffectFuncVisitor> {
 public:
  explicit FindSideEffectFuncVisitor(ASTContext* Context) : Context(Context) {}

  /*bool VisitCXXRecordDecl(CXXRecordDecl *Declaration) {
    if (Declaration->getQualifiedNameAsString() == "n::m::C") {
      FullSourceLoc FullLocation =
  Context->getFullLoc(Declaration->getBeginLoc()); if (FullLocation.isValid())
        llvm::outs() << "Found declaration at "
                     << FullLocation.getSpellingLineNumber() << ":"
                     << FullLocation.getSpellingColumnNumber() << "\n";
    }
    return true;
  }*/
  bool VisitFunctionDecl(FunctionDecl* funcDecl) {
    currentFunc = funcDecl;
    return true;
  }
  bool VisitBinaryOperator(BinaryOperator* binOp) {
    if (binOp->getOpcode() != BO_Assign || currentFunc) return true;
    auto lhs = binOp->getLHS();
    auto fullLoc = Context->getFullLoc(lhs->getBeginLoc());
    auto lineNum = fullLoc.getLineNumber(), colNum = fullLoc.getColumnNumber();

    if (auto declRef = dyn_cast<DeclRefExpr>(lhs)) {
      auto sidevar = getDeclFromRef(declRef);
      if (globalVar.count(sidevar)) {  // sidevar is global
        sideEffectFuncs[currentFunc].insert(
            SideEffect{lineNum, colNum, sidevar});
      }
    } else if (auto arraySubExpr = dyn_cast<ArraySubscriptExpr>(
                   lhs)) {  // left operand is an array element
      auto declRef = getArrayDeclFromArrayExpr(arraySubExpr);
      auto arrayVar = getDeclFromRef(declRef);
      if (globalVar.count(arrayVar) || dyn_cast<ParmVarDecl>(arrayVar)) {
        sideEffectFuncs[currentFunc].insert(
            SideEffect{lineNum, colNum, arrayVar});
      }
    } else {
      llvm_unreachable("only array element or variable can be lvalue in sysY");
    }
    return true;
  }

 private:
  friend class FindSideEffectFuncConsumer;
  FunctionDecl* currentFunc = nullptr;
  DenseMap<FunctionDecl*, DenseSet<SideEffect> > sideEffectFuncs;
  DenseSet<VarDecl*> globalVar;
  ASTContext* Context;
};

class FindSideEffectFuncConsumer : public clang::ASTConsumer {
 public:
  explicit FindSideEffectFuncConsumer(ASTContext* Context) : Visitor(Context) {}

  virtual void HandleTranslationUnit(clang::ASTContext& Context) {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

 private:
  FindSideEffectFuncVisitor Visitor;
};

int main(int argc, const char** argv) {
  auto e = CommonOptionsParser::create(argc, argv, ToolCategory);
  if (auto error = e.takeError()) {
    llvm::errs() << "Error: " << error << "\n";
    return 1;
  }
  auto optionParser = std::move(e.get());
  ClangTool CTool(optionParser.getCompilations(),
                  optionParser.getSourcePathList());

  // return CTool.run(newFrontendActionFactory(&Finder).get());
  return 0;
}
