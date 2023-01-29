#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <sstream>
#include <fstream>
#include <cstring>
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
#include "clang/Lex/Lexer.h"

#include "main.hpp"

/// no nested starttime and stoptime
/// actually, getint and getch also has side effect not
/// associated with variable(e.g., getint() / getint() ). To work
/// around this, I introduce a special global varibale __globalInput__
/// and let getarray, getch, getint write this variable. putint, putch,
/// putarray can't cause UB becouse their return value is void and will
/// not participate in expression.
// clang-format off
const char* declstr =
    "int __globalInput__; int getint(){__globalInput__ = __globalInput__ + 1;return 1;} "
    "int getch(){return getint();} int getarray(int a[]){a[0] = 1;return getint();}" 
    "void putint(int){} void putch(int){} void putarray(int, int[]){}"
    "void starttime(){} void stoptime(){}\n";
// clang-format on

using namespace llvm;
using namespace clang;
using namespace clang::tooling;
using namespace clang::ast_matchers;

using sideEffectMap = DenseMap<FunctionDecl*, DenseSet<DeclRefExpr*>>;

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
static cl::opt<bool> sideEffectDump(
    "side-effect-dump", cl::value_desc("side-effect-dump"),
    cl::desc("output side effect of each function"));

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

// https://stackoverflow.com/a/11154162/15570633
std::string getStmtString(Stmt* s, ASTContext* ctx) {
  auto start = s->getBeginLoc();
  auto& sm = ctx->getSourceManager();
  auto end = Lexer::getLocForEndOfToken(s->getEndLoc(), 0, sm, LangOptions());
  return std::string(sm.getCharacterData(start),
                     sm.getCharacterData(end) - sm.getCharacterData(start));
}

uint32_t getParamPos(ParmVarDecl* parm) {
  auto f = cast<FunctionDecl>(parm->getDeclContext());
  auto iter = std::find(f->param_begin(), f->param_end(), parm);
  assert(iter != f->param_end() && "can't find parameter pos");
  return (uint32_t)std::distance(f->param_begin(), iter);
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
                                     sideEffectMap& rMap)
      : Context(Context), writeEffectFuncs(wMap), readEffectFuncs(rMap) {}

  bool TraverseFunctionDecl(FunctionDecl* funcDecl) {
    assert(!currentFunc && "currentFunc should be null!");
    fakeArrayRef.clear();
    currentFunc = funcDecl;
    if (currentFunc->getName() == "main") main = currentFunc;

    RecursiveASTVisitor<FindSideEffectFuncVisitor>::TraverseFunctionDecl(
        funcDecl);
    if (sideEffectDump) dumpSideEffectMap();
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
    return true;
  }
  bool VisitInitListExpr(InitListExpr* initExpr) {  // debug only
    if (initExprDump) {
      errs() << "is_semantic: " << initExpr->isSemanticForm() << "\n";
    }
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr* declRef) {
    if (isa<FunctionDecl>(declRef->getDecl()) || !currentFunc) return true;
    if (fakeArrayRef.count(declRef)) return true;
    if (writeEffectFuncs.count(currentFunc) &&
        writeEffectFuncs[currentFunc].count(declRef))
      return true;
    auto decl = getDeclFromRef(declRef);

    if (varIsGloal(decl)) {
      readEffectFuncs[currentFunc].insert(declRef);
    } else if (isa<ParmVarDecl>(decl) &&
               decl->getType()->isPointerType()) {  // array parameter
      readEffectFuncs[currentFunc].insert(declRef);
    }
    return true;
  }

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
    auto n = currentFunc->getName();
    auto n2 = funcDecl->getName();
    auto n3 = getStmtString(callExpr, Context);
    if (funcDecl == currentFunc) return true;
    auto sideCheck = [=](sideEffectMap& m) {
      if (!m.count(funcDecl)) return;
      for (auto sideExpr : m[funcDecl]) {
        auto sideDecl = sideExpr->getDecl();
        if (isa<ParmVarDecl>(sideDecl)) {
          // callArg is a arraySubscriptExpr or DeclRefExpr because
          // there is no pointer arithmetics in sysY
          auto callArg =
              callExpr->getArg(getParamPos(cast<ParmVarDecl>(sideDecl)))
                  ->IgnoreImpCasts();
          if (isa<ArraySubscriptExpr>(callArg)) {
            callArg =
                getArrayDeclFromArrayExpr(cast<ArraySubscriptExpr>(callArg));
          }
          auto argDecl = getDeclFromRef(cast<DeclRefExpr>(callArg));
          if (argDecl->isLocalVarDecl()) {
            return;
          }
          if (isa<ParmVarDecl>(argDecl)) {  // also a parameter
            assert(argDecl->getType()->isPointerType() &&
                   "only array type can pass side effect in sysY");
          } else {
            assert(varIsGloal(argDecl) &&
                   "only local, param and global var in sysY");
          }
          assert(currentFunc);
          // implicitly remove repeat ref, should be safe?
          // e.g, reading callArg multiple times in callee causes callArg
          // inserted repeatedly
          m[currentFunc].insert(cast<DeclRefExpr>(callArg));
        } else {
          assert(currentFunc);
          // implicitly remove repeat ref, should be safe?
          m[currentFunc].insert(sideExpr);
        }
      }
    };
    sideCheck(writeEffectFuncs);
    sideCheck(readEffectFuncs);
    return true;
  }
  bool VisitBinaryOperator(BinaryOperator* binOp) {
    if (binOp->getOpcode() != BO_Assign || !currentFunc) return true;
    auto lhs = binOp->getLHS();

    if (auto declRef = dyn_cast<DeclRefExpr>(lhs)) {
      auto sidevar = getDeclFromRef(declRef);
      if (varIsGloal(sidevar)) {  // sidevar is global
        writeEffectFuncs[currentFunc].insert(declRef);
      }
    } else if (auto arraySubExpr = dyn_cast<ArraySubscriptExpr>(
                   lhs)) {  // left operand is an array element
      auto declRef = getArrayDeclFromArrayExpr(arraySubExpr);
      auto arrayVar = getDeclFromRef(declRef);
      if (varIsGloal(arrayVar)) {
        writeEffectFuncs[currentFunc].insert(declRef);
      } else if (auto paramDecl = dyn_cast<ParmVarDecl>(arrayVar)) {
        auto iter = std::find(currentFunc->param_begin(),
                              currentFunc->param_end(), paramDecl);
        assert(iter != currentFunc->param_end() && "unknown param decl");
        writeEffectFuncs[currentFunc].insert(declRef);
      }
    } else {
      llvm_unreachable("only array element or variable can be lvalue in sysY");
    }
    return true;
  }
  void dumpSideEffectMap();

 private:
  friend class UBCheckConsumer;
  FunctionDecl* currentFunc = nullptr;
  sideEffectMap& writeEffectFuncs;
  sideEffectMap& readEffectFuncs;
  FunctionDecl* main;
  ASTContext* Context;

  DenseSet<DeclRefExpr*> fakeArrayRef;
};

class FindUBVisitor : public RecursiveASTVisitor<FindUBVisitor> {
 public:
  enum Order : unsigned { ORDER, UNORDER };
  using ReachableGraph = DenseMap<VarDecl*, DenseSet<RefPair>>;
  using VarRefRecord = DenseMap<VarDecl*, std::vector<UniqueRef>>;
  explicit FindUBVisitor(ASTContext* Context, sideEffectMap& wMap,
                         sideEffectMap& rMap)
      : Context(Context), writeEffectFuncs(wMap), readEffectFuncs(rMap) {}

  void UBcheck(FunctionDecl* entryFunc) { TraverseFunctionDecl(entryFunc); }

  bool TraverseCallExpr(CallExpr* callExp) {
    TraverseCallee(callExp);
    auto funcDecl = callExp->getDirectCallee();

    auto callRef = cast<DeclRefExpr>(
        callExp->getCallee()->IgnoreImpCasts());  // safe in sysY
    declRefCall[callRef] = callExp;
    auto numArgs = callExp->getNumArgs();
    auto tmpRefs = new VarRefRecord[numArgs];
    VarRefRecord tmpOrgRef = std::move(refs);

    uint32_t tmpIndex = 0;
    for (auto arg : callExp->arguments()) {
      if (funcDecl->getParamDecl(tmpIndex)->getType()->isPointerType()) {
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
      tmpRefs[tmpIndex] = std::move(refs);
      ++tmpIndex;
    }

    VisitDeclRefExpr(callRef);

    for (auto& p : refs) {
      for (tmpIndex = 0; tmpIndex < numArgs; ++tmpIndex) {
        auto& map = tmpRefs[tmpIndex];
        if (map.count(p.getFirst())) {
          for (auto refExpr1 : map[p.getFirst()]) {
            for (auto refExpr2 : p.getSecond()) {
              reachGraph[p.getFirst()].insert({refExpr2, refExpr1});
            }
          }
        }
      }
    }
    for (tmpIndex = 0; tmpIndex < numArgs; ++tmpIndex) {  // merge refs
      for (auto& p : tmpRefs[tmpIndex]) {
        mergeVector(refs[p.getFirst()], p.getSecond());
      }
    }
    for (auto& p : tmpOrgRef) {
      mergeVector(refs[p.getFirst()], p.getSecond());
    }
    delete[] tmpRefs;
    return true;
  }
  bool TraverseFunctionDecl(FunctionDecl* funcDecl) {
    // simplified version of RecursiveVisitor::TraverseFunctionDecl
    fakeArrayRef.clear();
    for (auto parm : funcDecl->parameters()) {
      TraverseDecl(parm);
    }
    assert(funcDecl->getBody() && "no declaration in sysY");
    InternalVisitStmt(funcDecl->getBody());
    return true;
  }

  bool TraverseBinaryOperator(BinaryOperator* binOp) {
    if (binOp->getOpcode() == BO_Assign) {
      if (isa<DeclRefExpr>(binOp->getLHS())) {
        fakeArrayRef.insert(cast<DeclRefExpr>(binOp->getLHS()));
      } else {
        fakeArrayRef.insert(getArrayDeclFromArrayExpr(
            cast<ArraySubscriptExpr>(binOp->getLHS())));
      }
    }

    if (binOp->getOpcode() == BO_LAnd || binOp->getOpcode() == BO_LOr) {
      VarRefRecord tmpOrgRef = std::move(refs);
      TraverseStmt(binOp->getLHS());
      VarRefRecord tmprefs = std::move(refs);
      TraverseStmt(binOp->getRHS());
      for (const auto& p1 : refs) {
        if (tmprefs.count(p1.getFirst())) {
          for (auto pair1 : p1.getSecond()) {
            for (auto pair2 : tmprefs[p1.getFirst()]) {
              reachGraph[p1.getFirst()].insert({pair1, pair2});
            }
          }
        }
      }
      for (const auto& p : tmprefs) {  // merge refs
        mergeVector(refs[p.getFirst()], p.getSecond());
      }
      for (const auto& p : tmpOrgRef) {
        mergeVector(refs[p.getFirst()], p.getSecond());
      }
    } else {
      TraverseStmt(binOp->getLHS());
      TraverseStmt(binOp->getRHS());
    }
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr* refExp) {
    if (fakeArrayRef.count(refExp)) return true;
    auto decl = refExp->getDecl();
    if (isa<FunctionDecl>(decl)) {
      auto callExpr = declRefCall[refExp];
      decltype(refs) tmpref;
      assert(callExpr &&
             "should map any DeclRef of func type into its callExpr");
      auto helpFunc = [decl, callExpr, this, &tmpref](sideEffectMap& m,
                                                      RWType t) {
        for (auto varRef : m[cast<FunctionDecl>(decl)]) {
          auto varDecl = cast<VarDecl>(varRef->getDecl());
          if (isa<ParmVarDecl>(varDecl)) {
            auto arg = callExpr->getArg(getParamPos(cast<ParmVarDecl>(varDecl)))
                           ->IgnoreImpCasts();
            auto arrayRef =
                isa<DeclRefExpr>(arg)
                    ? cast<DeclRefExpr>(arg)
                    : getArrayDeclFromArrayExpr(cast<ArraySubscriptExpr>(arg));
            // arrayRef should be a fake arrayRef
            auto argDecl = cast<VarDecl>(arrayRef->getDecl());
            if (isa<ParmVarDecl>(argDecl)) {
              tmpref[getArgFromParm(cast<ParmVarDecl>(argDecl), callStack)]
                  .push_back({arrayRef, (refCnt += 2) | t});
            } else {
              assert(varIsGloal(argDecl) || argDecl->isLocalVarDecl());
              tmpref[argDecl].push_back({arrayRef, (refCnt += 2) | t});
            }
            assert(fakeArrayRef.count(arrayRef) &&
                   "array parameter should be a fake ref");
          } else {
            assert(varIsGloal(varDecl) &&
                   "only global and parameter have side effect");
            tmpref[varDecl].push_back({varRef, (refCnt += 2) | t});
          }
        }
      };
      helpFunc(writeEffectFuncs, WRITE);
      helpFunc(readEffectFuncs, READ);
      for (auto& p : tmpref) {
        auto& vec = p.getSecond();
        for (auto iter = vec.begin(); iter != vec.end(); ++iter) {
          for (auto iter2 = iter + 1; iter2 != vec.end(); ++iter2) {
            reachGraph[p.getFirst()].insert({*iter, *iter2});
          }
        }
        mergeVector(refs[p.getFirst()], p.getSecond());
      }

    } else {
      // one statement has at most one write which is thought as fake ref
      auto varDecl = cast<VarDecl>(refExp->getDecl());
      if (isa<ParmVarDecl>(varDecl) && varDecl->getType()->isPointerType()) {
        auto argDecl = getArgFromParm(cast<ParmVarDecl>(varDecl), callStack);
        refs[argDecl].push_back({refExp, (refCnt += 2) | READ});
      } else {
        refs[varDecl].push_back({refExp, (refCnt += 2) | READ});
      }
    }
    return true;
  }

 private:
  enum RWType : unsigned { READ, WRITE, NONE };
  enum Position : unsigned { START, END };
  enum Direction : unsigned { LINE, COLUMN };

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
      errs() << "[Stmt Start, line " << getLineOrColumn(s, START, LINE, Context)
             << "] " << getStmtString(s, Context) << "\n";
      graphDump();
      errs() << "[Stmt End, line " << getLineOrColumn(s, END, LINE, Context)
             << "]\n\n";
    }
    DenseSet<VarDecl*> tmp;
    findUB(getLineOrColumn(s, START, LINE, Context), s);
  }

  void TraverseCallee(CallExpr* call) {
    auto subFunc = call->getDirectCallee();
    // avoid recursive call
    if (!callStack.empty() && callStack.back()->getDirectCallee() == subFunc)
      return;
    assert(subFunc && "only direct function call in sysY");

    auto tmpFakeArrayRef = std::move(fakeArrayRef);
    auto tmpReachGraph = std::move(reachGraph);
    auto tmpRefs = std::move(refs);
    auto tmpDeclRefCall = std::move(declRefCall);

    callStack.push_back(call);
    TraverseFunctionDecl(subFunc);
    callStack.pop_back();

    fakeArrayRef = std::move(tmpFakeArrayRef);
    reachGraph = std::move(tmpReachGraph);
    refs = std::move(tmpRefs);
    declRefCall = std::move(tmpDeclRefCall);
  }

  /// @brief core function to find UB by flatten trees, and split forest
  /// recursively
  /// @param stmtLine, line number of current statement
  void findUB(uint32_t stmtLine, Stmt* st) {
    for (auto& p : refs) {
      auto var = p.getFirst();
      for (auto iter = p.getSecond().begin(); iter != p.getSecond().end();
           ++iter) {
        if ((iter->second & WRITE) != WRITE) continue;
        for (auto iter2 = p.getSecond().begin(); iter2 != p.getSecond().end();
             ++iter2) {
          if (*iter != *iter2 &&
              !reachGraph[p.getFirst()].count({*iter, *iter2}))
            goto FAIL;
        }
      }
      continue;
    FAIL:
      errs() << "[WARNING Possible Undefined Behavior in Statement line "
             << stmtLine << "] " << getStmtString(st, Context) << "\n";
      dumpCallStack();
      errs() << "VAR: " << var->getName() << " declaration at "
             << getLineOrColumn(var, START, LINE, Context) << ":"
             << getLineOrColumn(var, START, COLUMN, Context) << "\n";
      for (auto ref : refs[p.getFirst()]) {
        errs() << "at " << getLineOrColumn(ref.first, START, LINE, Context)
               << ":" << getLineOrColumn(ref.first, START, COLUMN, Context)
               << ", "
               << ((ref.second & WRITE) == WRITE ? "write\n" : "read\n");
      }
      errs() << "[END WARNING]\n";
    }
  }

  static VarDecl* getArgFromParm(ParmVarDecl* parmDecl,
                                 ArrayRef<CallExpr*> callStack) {
    auto argExpr =
        callStack.back()->getArg(getParamPos(parmDecl))->IgnoreImpCasts();
    if (isa<ArraySubscriptExpr>(argExpr)) {
      argExpr = getArrayDeclFromArrayExpr(cast<ArraySubscriptExpr>(argExpr));
    }
    auto argDecl = cast<VarDecl>(cast<DeclRefExpr>(argExpr)->getDecl());
    assert((argDecl->getType()->isPointerType() ||
            argDecl->getType()->isArrayType()) &&
           "only array type need to find orignial arg");
    if (isa<ParmVarDecl>(argDecl)) {
      assert(callStack.size() > 1 && "top function shouldn't have parameter");
      return getArgFromParm(cast<ParmVarDecl>(argDecl), callStack.drop_back());
    } else {
      assert(argDecl->isLocalVarDecl() || varIsGloal(argDecl));
      return argDecl;
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
    reachGraph.clear();
    refs.clear();
    declRefCall.clear();
  }

  void graphDump() {
    for (auto& p : refs) {
      auto map = dumpRefs(p.getFirst());
      errs() << "[REACHABLITIY]\n";
      bool hasOut = false;
      for (uint32_t i = 0, leng = map.size(); i < leng; ++i) {
        for (uint32_t j = i + 1; j < leng; ++j) {
          if (reachGraph.count(p.getFirst()) &&
              reachGraph[p.getFirst()].count({map[i], map[j]})) {
            hasOut = true;
            errs() << "[" << i << ", " << j << ", order]";
          }
        }
      }
      if (hasOut)
        errs() << "\n[END]\n";
      else
        errs() << "[END]\n";
    }
  }

  void dumpCallStack() {
    errs() << "[Call Stack]: ";
    if (callStack.empty()) {
      errs() << "Empty\n";
      return;
    }
    for (size_t len = callStack.size(), i = 0; i < len - 1; ++i) {
      errs() << getStmtString(callStack[i], Context) << " --> ";
    }
    errs() << getStmtString(callStack[callStack.size() - 1], Context) << "\n";
  }

  std::vector<UniqueRef> dumpRefs(VarDecl* v) {
    errs() << "[VAR]: " << v->getName() << "\n";
    uint32_t tmpIndex = 0;
    std::vector<UniqueRef> map;
    if (!refs.count(v)) return map;
    for (auto expr : refs[v]) {
      errs() << "[" << map.size() << "]";
      if ((expr.second & WRITE) == READ)
        errs() << "read at ";
      else if ((expr.second & WRITE) == WRITE)
        errs() << "write at ";
      else
        llvm_unreachable("SHOULD ONLY READ AND WRITE");
      errs() << getLineOrColumn(expr.first, START, LINE, Context) << ":"
             << getLineOrColumn(expr.first, START, COLUMN, Context) << "\n";
      map.push_back(expr);
    }
    return map;
  }

  template <class T>
  void mergeVector(std::vector<T>& v1, const std::vector<T>& v2) {
    auto refsSize = v1.size();
    v1.resize(refsSize + v2.size());
    std::memcpy(v1.data() + refsSize, v2.data(), v2.size() * sizeof(T));
  }

  friend class UBCheckConsumer;
  friend class FindSideEffectFuncVisitor;
  sideEffectMap& writeEffectFuncs;
  sideEffectMap& readEffectFuncs;
  ASTContext* Context;
  std::vector<CallExpr*> callStack;
  uint64_t refCnt = 0;

  // used for single stmt traversal
  DenseMap<DeclRefExpr*, CallExpr*> declRefCall;
  // if refpair in reachGraph, the ref pair is ordered
  ReachableGraph reachGraph;
  VarRefRecord refs;
  DenseSet<DeclRefExpr*> fakeArrayRef;
};

void FindSideEffectFuncVisitor::dumpSideEffectMap() {
  errs() << "[START] Function: " << currentFunc->getName() << "\n";
  auto helpFunc = [this](const char* s, sideEffectMap& m) {
    if (m.count(currentFunc)) {
      for (auto p : m[currentFunc]) {
        errs() << s << p->getDecl()->getName() << ", at "
               << FindUBVisitor::getLineOrColumn(p, FindUBVisitor::START,
                                                 FindUBVisitor::LINE, Context)
               << ":"
               << FindUBVisitor::getLineOrColumn(p, FindUBVisitor::START,
                                                 FindUBVisitor::COLUMN, Context)
               << "\n";
      }
    }
  };
  helpFunc("write ", writeEffectFuncs);
  helpFunc("read ", readEffectFuncs);
  errs() << "[END]\n";
}

class UBCheckConsumer : public clang::ASTConsumer {
 public:
  explicit UBCheckConsumer(ASTContext* Context)
      : writeEffectMap(),
        readEffectMap(),
        allFuncs(),
        Visitor(Context, writeEffectMap, readEffectMap),
        UBVisitor(Context, writeEffectMap, readEffectMap) {}

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
    UBVisitor.UBcheck(Visitor.main);
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
