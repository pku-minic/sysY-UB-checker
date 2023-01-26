#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <sstream>
#include <fstream>
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/AST/RecursiveASTVisitor.h"

#include "main.hpp"

/// no nested starttime and stoptime
/// actually, getint and getch also has side effect not
/// associated with variable(e.g., getint() / getint() ). To work
/// around this, I introduce a special global varibale __globalInput__
/// and let getarray, getch, getint write this variable. putint, putch,
/// putarray can't cause UB becouse their return value is void and will
/// not participate in expression.
const char* declstr =
    "int __globalInput__;int getint();int getch();int getarray(int[]);void "
    "putint(int);void "
    "putch(int);void putarray(int, int[]);void starttime();void stoptime();\n";
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
static cl::list<std::string> extraArgs("extra-arg", cl::value_desc("extra arg"),
                                       cl::desc("extra compiler args"));
static cl::opt<bool> verbose("verbose", cl::value_desc("verbose"),
                             cl::desc("output addition runtime info"));
static cl::opt<bool> initExprDump("init-list-expr-dump",
                                  cl::value_desc("list-expr-dump"),
                                  cl::desc("dump init list expr info"));
static cl::opt<bool> stmtRefDump(
    "stmt-ref-dump", cl::value_desc("stmt-ref-dump"),
    cl::desc("output declref graph for each stmt"));

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
  if (!sidevar) {
    auto n = ref->getDecl()->getName();
    errs() << n << "\n";
    assert(sidevar &&
           "decl of any valid sysy file should only contain varDecl and "
           "FunctionDecl\n");
  }
  return sidevar;
}

bool varIsGloal(VarDecl* decl) {
  auto linkType = decl->getFormalLinkage();
  assert((linkType == Linkage::ExternalLinkage ||
          linkType == Linkage::NoLinkage) &&
         "there should only be global or local var in sysY");
  return linkType == Linkage::ExternalLinkage;
}

auto getParamPos(FunctionDecl* f, ParmVarDecl* parm) {
  auto iter = std::find(f->param_begin(), f->param_end(), parm);
  assert(iter != f->param_end() && "can't find parameter pos");
  return (decltype(SideEffect::paramPos))std::distance(f->param_begin(), iter);
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
  explicit FindSideEffectFuncVisitor(ASTContext* Context, sideEffectMap& wMap,
                                     sideEffectMap& rMap,
                                     std::vector<FunctionDecl*>& funcs)
      : Context(Context),
        writeEffectFuncs(wMap),
        readEffectFuncs(rMap),
        allFuncs(funcs) {}

  bool TraverseFunctionDecl(FunctionDecl* funcDecl) {
    assert(globalInput && "globalInput should be visited first");
    assert(!currentFunc && "currentFunc should be null!");
    fakeArrayRef.clear();
    currentFunc = funcDecl;
    if (std::find(sysyLibFunc.begin(), sysyLibFunc.end(),
                  funcDecl->getName()) == sysyLibFunc.end())
      allFuncs.push_back(funcDecl);

    auto funcName = currentFunc->getName();
    if (funcName == "getarray") {
      writeEffectFuncs[currentFunc].insert(
          SideEffect{0, 0, currentFunc->getParamDecl(0), 0});
      writeEffectFuncs[currentFunc].insert(SideEffect{0, 0, globalInput});
    } else if (funcName == "getint" || funcName == "getch") {
      writeEffectFuncs[currentFunc].insert(SideEffect{0, 0, globalInput});
    } else {
      RecursiveASTVisitor<FindSideEffectFuncVisitor>::TraverseFunctionDecl(
          funcDecl);
      SmallVector<VarDecl*, 3> readVar;
      if (readEffectFuncs.count(currentFunc) &&
          writeEffectFuncs.count(currentFunc)) {
        for (auto& se : readEffectFuncs[currentFunc]) {
          if (writeEffectFuncs[currentFunc].count(se))
            readVar.push_back(se.sideVar);
        }
      }
      for (auto var : readVar) {
        readEffectFuncs[currentFunc].erase(SideEffect{0, 0, var});
      }
    }
    currentFunc = nullptr;
    return true;
  }
  bool VisitVarDecl(VarDecl* v) {  // debug only
    if (verbose) {
      auto t = v->getType().getTypePtr();
      errs() << v->getName() << ": class," << t->getTypeClassName() << ", "
             << v->getType().getAsString()
             << ", isCanonical: " << v->getType().isCanonical()
             << ", isPointer: " << t->isPointerType()
             << ", isArray: " << t->isArrayType()
             << ", declContext: " << v->getDeclContext()->getDeclKindName()
             << "\n";
    }
    if (v->getName() == "__globalInput__" && varIsGloal(v)) {
      globalInput = v;
    }
    return true;
  }
  bool VisitInitListExpr(InitListExpr* initExpr) {  // debug only
    if (initExprDump) {
      errs() << "is_semantic: " << initExpr->isSemanticForm() << "\n";
    }
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr* declRef);

  bool VisitCallExpr(CallExpr* callExpr) {
    auto funcDecl = callExpr->getDirectCallee();
    assert(funcDecl && "only direct function call in sysY");

    uint32_t tmpIndex = 0;
    for (auto expr : callExpr->arguments()) {  // get all fake array ref
      if (funcDecl->getParamDecl(tmpIndex++)->getType()->isPointerType()) {
        auto trueExpr = expr->IgnoreImpCasts();
        if (isa<ArraySubscriptExpr>(trueExpr)) {
          trueExpr =
              getArrayDeclFromArrayExpr(cast<ArraySubscriptExpr>(trueExpr));
        }
        assert(isa<DeclRefExpr>(trueExpr) &&
               "only ArraySubscriptExpr and DeclRefExpr can be as pointer "
               "argument");
        fakeArrayRef.insert(cast<DeclRefExpr>(trueExpr));
      }
    }
    if (funcDecl == currentFunc) return true;
    auto sideCheck = [=](sideEffectMap& m) {
      if (!m.count(funcDecl)) return;
      for (const auto& sideEff : m[funcDecl]) {
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
            return;
          }
          if (isa<ParmVarDecl>(argDecl)) {  // also a parameter
            pos = getParamPos(currentFunc, cast<ParmVarDecl>(argDecl));
            assert(argDecl->getType()->isPointerType() &&
                   "only array type can pass side effect in sysY");
          } else {
            assert(varIsGloal(argDecl) &&
                   "only local, param and global var in sysY");
          }
          assert(currentFunc);
          m[currentFunc].insert(
              SideEffect{sideEff.col, sideEff.line, argDecl, pos});

        } else {
          assert(currentFunc);
          m[currentFunc].insert(sideEff);
        }
      }
    };
    sideCheck(writeEffectFuncs);
    sideCheck(readEffectFuncs);
    return true;
  }
  bool VisitBinaryOperator(BinaryOperator* binOp);
  /*
    bool VisitDeclStmt(DeclStmt* decl) {
      errs() << "start\n";
      for (auto d : decl->children()) {
        d->dump();
      }
      errs() << "end\n";
      return true;
    }
  */
  /*
   bool VisitForStmt(ForStmt* forSt) {
     errs() << "start for\n";
     forSt->getInit()->dump();
     errs() << "\n";
     forSt->getCond()->dump();
     errs() << "\n";
     forSt->getInc()->dump();
     errs() << "\n";
     forSt->getBody();
     errs() << "\n";
     auto s = forSt->getConditionVariableDeclStmt();
     if (!s)
       errs() << "empty\n";
     else
       s->dump();
     errs() << "end\n";
     return true;
   }
 */
 private:
  friend class UBCheckConsumer;
  FunctionDecl* currentFunc = nullptr;
  sideEffectMap& writeEffectFuncs;
  sideEffectMap& readEffectFuncs;
  std::vector<FunctionDecl*>& allFuncs;
  ASTContext* Context;

  VarDecl* globalInput = nullptr;
  DenseSet<DeclRefExpr*> fakeArrayRef;
};

class FindUBVisitor : public RecursiveASTVisitor<FindUBVisitor> {
 public:
  using VarUsedRecord = DenseMap<DeclRefExpr*, std::vector<DeclRefExpr*> >;
  explicit FindUBVisitor(ASTContext* Context, sideEffectMap& wMap,
                         sideEffectMap& rMap, std::vector<FunctionDecl*>& funcs)
      : Context(Context),
        writeEffectFuncs(wMap),
        readEffectFuncs(rMap),
        allFuncs(funcs) {}
  bool VisitVarDecl(VarDecl* v) {
    if (v->getName() == "__globalInput__" && varIsGloal(v)) {
      globalInput = v;
    }
    return true;
  }

  bool TraverseCallExpr(CallExpr* callExp) {
    auto callRef = cast<DeclRefExpr>(
        callExp->getCallee()->IgnoreImpCasts());  // safe in sysY
    declRefCall.insert({callRef, callExp});
    VisitDeclRefExpr(callRef);
    callStack.push_back(callRef);

    auto funcDecl = callExp->getDirectCallee();
    assert(funcDecl && "only direct function call in sysY");
    uint32_t tmpIndex = 0;
    for (auto arg : callExp->arguments()) {
      if (funcDecl->getParamDecl(tmpIndex++)->getType()->isPointerType()) {
        auto trueExpr = arg->IgnoreImpCasts();
        if (isa<ArraySubscriptExpr>(trueExpr)) {
          trueExpr =
              getArrayDeclFromArrayExpr(cast<ArraySubscriptExpr>(trueExpr));
        }
        assert(isa<DeclRefExpr>(trueExpr) &&
               "only ArraySubscriptExpr and DeclRefExpr can be as pointer "
               "argument");
        fakeArrayRef.insert(cast<DeclRefExpr>(trueExpr));
      }
      TraverseStmt(arg);
    }
    callStack.pop_back();
    return true;
  }
  bool TraverseFunctionDecl(FunctionDecl* funcDecl) {
    // simplified version of RecursiveVisitor::TraverseFunctionDecl
    fakeArrayRef.clear();
    if (std::find(sysyLibFunc.begin(), sysyLibFunc.end(),
                  funcDecl->getName()) != sysyLibFunc.end())
      return true;
    for (auto parm : funcDecl->parameters()) {
      TraverseDecl(parm);
    }
    assert(funcDecl->getBody() && "no declaration in sysY");
    InternalVisitStmt(funcDecl->getBody());
    return true;
  }

  bool VisitBinaryOperator(BinaryOperator* binOp) {
    if (binOp->getOpcode() != BO_Assign) return true;
    if (isa<DeclRefExpr>(binOp->getLHS())) {
      lhsRef = cast<DeclRefExpr>(binOp->getLHS());
    } else {
      lhsRef =
          getArrayDeclFromArrayExpr(cast<ArraySubscriptExpr>(binOp->getLHS()));
    }
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr* refExp) {
    if (refExp == lhsRef) {
      assert(callStack.empty() && "lhsRef should be not as an arg");
      return true;
    }
    if (fakeArrayRef.count(refExp)) return true;
    usedRecord.insert({refExp, {}});
    if (!callStack.empty()) {
      assert(usedRecord.count(callStack.back()) &&
             "Expr in callStack should be in usedRecord");
      usedRecord[callStack.back()].push_back(refExp);
    } else {
      roots.push_back(refExp);
    }
    return true;
  }

 private:
  enum RWType : unsigned { READ, WRITE, NONE };
  enum Position : unsigned { START, END };
  enum Direction : unsigned { LINE, COLUMN };
  struct PosStruct {
    uint32_t line;
    uint32_t col;
    RWType rw;
  };
  void InternalVisitStmt(Stmt* s) {  // call this when s is a full expression
    // for, while and if stmt will have optional cond variable(declare variable
    // in condition expression), this will be nullptr in sysY but what is Init
    // Statement in IfStmt ?
    if (!s) return;
    resetStmtTraversal();
    switch (s->getStmtClass()) {
      case Stmt::DeclStmtClass:
      case Stmt::ForStmtClass:
      case Stmt::WhileStmtClass:
      case Stmt::CompoundStmtClass:
      case Stmt::IfStmtClass:
      case Stmt::BreakStmtClass:  // break and continue stmt don't have children
      case Stmt::ContinueStmtClass:
      case Stmt::ReturnStmtClass:  // optional ret expr
      case Stmt::NullStmtClass:    // ; is a null stmt
        InternalVisitComplexStmt(s);
        return;
      default:
        assert(isa<Expr>(s) && "unknown stmt class");
        TraverseStmt(s);
    }
    if (stmtRefDump) {
      errs() << "line " << getLineOrColumn(s, START, LINE, Context)
             << ", Stmt Start\n";
      graphDump(usedRecord, roots);
      errs() << "line " << getLineOrColumn(s, END, LINE, Context)
             << ", Stmt End\n";
    }
    DenseSet<VarDecl*> tmp;
    findUB(roots, getLineOrColumn(s, START, LINE, Context), tmp);
  }

  /// @brief core function to find UB by flatten trees, and split forest
  /// recursively
  /// @param stmtLine, line number of current statement
  /// @param forestRoots, forest roots
  void findUB(ArrayRef<DeclRefExpr*> forestRoots, uint32_t stmtLine,
              DenseSet<VarDecl*>& foundUBvars) {
    std::vector<DenseMap<VarDecl*, std::vector<PosStruct> > > usedVar;
    usedVar.resize(forestRoots.size());
    std::vector<VarDecl*> vars;
    std::vector<VarDecl*> UBvars;
    for (uint32_t i = 0, l = forestRoots.size(); i < l; ++i) {
      flattenTree(forestRoots[i], usedVar[i]);
      for (auto& p : usedVar[i]) {
        vars.push_back(p.getFirst());
      }
    }
    for (auto var : vars) {
      if (foundUBvars.count(var)) continue;
      int status[2]{0, 0};
      for (auto& map : usedVar) {
        if (map.count(var)) {
          auto f =
              std::find_if(map[var].begin(), map[var].end(),
                           [](const PosStruct& p) { return p.rw == WRITE; });
          status[f != map[var].end()]++;
          if (status[WRITE] >= 1 && status[WRITE] + status[READ] >= 2) {
            UBvars.push_back(var);
            foundUBvars.insert(var);
            break;
          }
        }
      }
    }
    for (auto var : UBvars) {
      errs() << "[WARNING] Possible Undefined Behavior in Statement line "
             << stmtLine << "\n";
      errs() << "VAR: " << var->getName() << " declaration at "
             << getLineOrColumn(var, START, LINE, Context) << ":"
             << getLineOrColumn(var, START, COLUMN, Context) << "\n";
      for (auto& map : usedVar) {
        if (map.count(var)) {
          for (const auto& p : map[var]) {
            errs() << "at " << p.line << ":" << p.col << ", "
                   << (p.rw == WRITE ? "write\n" : "read\n");
          }
        }
      }
      errs() << "[END WARNING]\n";
    }
    for (auto var : forestRoots) {
      assert(usedRecord.count(var) && "usedRecord should contain all refExpr");
      if (!usedRecord[var].empty()) {
        findUB(usedRecord[var], stmtLine, foundUBvars);
      }
    }
  }

  /// @brief  given a DeclRefExpr root, return all variables used by the tree
  /// @param root
  /// @param usedVar -> return value
  void flattenTree(DeclRefExpr* root,
                   DenseMap<VarDecl*, std::vector<PosStruct> >& usedVar) {
    auto decl = root->getDecl();
    if (isa<FunctionDecl>(decl)) {
      assert(declRefCall.count(root) && "we should store any callexpr");
      auto callExpr = declRefCall[root];
      auto helpFunc = [decl, callExpr, &usedVar](sideEffectMap& m, RWType t) {
        for (const auto& se : m[cast<FunctionDecl>(decl)]) {
          if (isa<ParmVarDecl>(se.sideVar)) {
            auto arg = callExpr->getArg(se.paramPos)->IgnoreImpCasts();
            auto arrayRef =
                isa<DeclRefExpr>(arg)
                    ? cast<DeclRefExpr>(arg)
                    : getArrayDeclFromArrayExpr(cast<ArraySubscriptExpr>(arg));
            usedVar[cast<VarDecl>(arrayRef->getDecl())].push_back(
                {se.line, se.col, t});
          } else {
            usedVar[se.sideVar].push_back({se.line, se.col, t});
          }
        }
      };
      helpFunc(readEffectFuncs, READ);
      helpFunc(writeEffectFuncs, WRITE);
      for (auto child : usedRecord[root]) {  // flatten recursively
        flattenTree(child, usedVar);
      }
    } else if (isa<VarDecl>(decl)) {
      usedVar[cast<VarDecl>(decl)].push_back(
          {getLineOrColumn(root, START, LINE, Context),
           getLineOrColumn(root, START, COLUMN, Context), READ});
      assert((usedRecord.count(root) || usedRecord[root].empty()) &&
             "VarRef don't have child");
    } else {
      llvm_unreachable("there should only be FunctionDecl and VarDecl in sysY");
    }
  }

  template <class T>
  static uint32_t getLineOrColumn(T* s, Position p, Direction d,
                                  ASTContext* context) {
    auto pos = p == START ? s->getBeginLoc() : s->getEndLoc();
    return d == LINE ? context->getFullLoc(pos).getLineNumber() - 1
                     : context->getFullLoc(pos).getColumnNumber();
  }

  void InternalVisitComplexStmt(Stmt* s) {
    for (auto subSt : s->children()) {
      // subSt will be init expr of decl varibale for DeclStmt
      InternalVisitStmt(subSt);
    }
  }

  void resetStmtTraversal() {
    usedRecord.clear();
    roots.clear();
    assert(callStack.empty() && "callStack should be empty after traversal");
    lhsRef = nullptr;
  }

  void graphDump(VarUsedRecord& record, std::vector<DeclRefExpr*> r) {
    DenseSet<DeclRefExpr*> used;
    for (auto& rf : r) {
      edgeDump(record, rf, used);
    }
  }
  void edgeDump(VarUsedRecord& record, DeclRefExpr* ref,
                DenseSet<DeclRefExpr*> used, uint32_t depth = 0) {
    if (used.count(ref)) return;
    errs() << std::string(depth, ' ') << ref->getDecl()->getName() << "\n";
    used.insert(ref);
    for (auto& sub : record[ref]) {
      edgeDump(record, sub, used, depth + 2);
    }
  }

  friend class UBCheckConsumer;
  friend class FindSideEffectFuncVisitor;
  sideEffectMap& writeEffectFuncs;
  sideEffectMap& readEffectFuncs;
  std::vector<FunctionDecl*>& allFuncs;
  ASTContext* Context;

  // used for single stmt traversal
  VarUsedRecord usedRecord;
  std::vector<DeclRefExpr*> roots;
  DenseMap<DeclRefExpr*, CallExpr*> declRefCall;
  std::vector<DeclRefExpr*> callStack;
  DeclRefExpr* lhsRef;

  VarDecl* globalInput = nullptr;
  DenseSet<DeclRefExpr*> fakeArrayRef;
};

bool FindSideEffectFuncVisitor::VisitDeclRefExpr(DeclRefExpr* declRef) {
  auto lineNum = FindUBVisitor::getLineOrColumn(declRef, FindUBVisitor::START,
                                                FindUBVisitor::LINE, Context);
  auto colNum = FindUBVisitor::getLineOrColumn(declRef, FindUBVisitor::START,
                                               FindUBVisitor::COLUMN, Context);
  if (isa<FunctionDecl>(declRef->getDecl()) || !currentFunc) return true;
  if (fakeArrayRef.count(declRef)) return true;
  auto decl = getDeclFromRef(declRef);
  /*auto funcname = currentFunc == nullptr ? "" : currentFunc->getName();
  auto varname = decl->getName();
  auto t = decl->getType();
  t.dump();*/
  if (varIsGloal(decl)) {
    readEffectFuncs[currentFunc].insert(SideEffect{lineNum, colNum, decl});
  } else if (isa<ParmVarDecl>(decl) &&
             decl->getType()->isPointerType()) {  // array parameter
    readEffectFuncs[currentFunc].insert(
        SideEffect{lineNum, colNum, decl,
                   getParamPos(currentFunc, cast<ParmVarDecl>(decl))});
  }
  return true;
}
bool FindSideEffectFuncVisitor::VisitBinaryOperator(BinaryOperator* binOp) {
  if (binOp->getOpcode() != BO_Assign || !currentFunc) return true;
  auto lhs = binOp->getLHS();
  auto lineNum = FindUBVisitor::getLineOrColumn(lhs, FindUBVisitor::START,
                                                FindUBVisitor::LINE, Context);
  auto colNum = FindUBVisitor::getLineOrColumn(lhs, FindUBVisitor::START,
                                               FindUBVisitor::COLUMN, Context);

  if (auto declRef = dyn_cast<DeclRefExpr>(lhs)) {
    auto sidevar = getDeclFromRef(declRef);
    if (varIsGloal(sidevar)) {  // sidevar is global
      writeEffectFuncs[currentFunc].insert(
          SideEffect{lineNum, colNum, sidevar});
    }
  } else if (auto arraySubExpr = dyn_cast<ArraySubscriptExpr>(
                 lhs)) {  // left operand is an array element
    auto declRef = getArrayDeclFromArrayExpr(arraySubExpr);
    auto arrayVar = getDeclFromRef(declRef);
    if (varIsGloal(arrayVar)) {
      writeEffectFuncs[currentFunc].insert(
          SideEffect{lineNum, colNum, arrayVar});
    } else if (auto paramDecl = dyn_cast<ParmVarDecl>(arrayVar)) {
      auto iter = std::find(currentFunc->param_begin(),
                            currentFunc->param_end(), paramDecl);
      assert(iter != currentFunc->param_end() && "unknown param decl");
      writeEffectFuncs[currentFunc].insert(SideEffect{
          lineNum, colNum, arrayVar,
          (uint32_t)std::distance(currentFunc->param_begin(), iter)});
    }
  } else {
    llvm_unreachable("only array element or variable can be lvalue in sysY");
  }
  return true;
}

class UBCheckConsumer : public clang::ASTConsumer {
 public:
  explicit UBCheckConsumer(ASTContext* Context)
      : writeEffectMap(),
        readEffectMap(),
        allFuncs(),
        Visitor(Context, writeEffectMap, readEffectMap, allFuncs),
        UBVisitor(Context, writeEffectMap, readEffectMap, allFuncs) {}

  virtual void HandleTranslationUnit(clang::ASTContext& Context) {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    if (verbose) {
      errs() << "all functions\n";
      for (auto func : allFuncs) {
        errs() << func->getName() << "\n";
      }
      errs() << "\n";
      for (const auto& p : writeEffectMap) {
        auto f = p.getFirst();
        errs() << "/*   Side Effect of func " << f->getName()
               << " start   */\n";
        for (auto& effects : p.getSecond()) {
          errs() << "w, ";
          errs() << effects;
        }
        if (readEffectMap.count(p.getFirst())) {
          for (auto& effects : readEffectMap[p.getFirst()]) {
            errs() << "r, ";
            errs() << effects;
          }
        }
        errs() << "/*   END   */\n";
      }
      for (const auto& p : readEffectMap) {
        auto f = p.getFirst();
        if (!writeEffectMap.count(f)) {
          errs() << "/*   Side Effect of func " << f->getName()
                 << " start   */\n";
          for (auto& effects : p.getSecond()) {
            errs() << "r, ";
            errs() << effects;
          }
          errs() << "/*   END   */\n";
        }
      }
    }
    UBVisitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

 private:
  FindSideEffectFuncVisitor Visitor;
  FindUBVisitor UBVisitor;
  std::vector<FunctionDecl*> allFuncs;
  VarDecl* globalInput;
  sideEffectMap writeEffectMap;
  sideEffectMap readEffectMap;
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
  std::ostringstream code;
  if (!file) {
    errs() << "open input file failed\n";
    return 1;
  }
  code << declstr;
  code << file.rdbuf();
  if (verbose) errs() << code.str();
  runToolOnCodeWithArgs(std::make_unique<FindSideEffectFuncAction>(),
                        code.str(), extraArgs, inputFile);
  return 0;
}
