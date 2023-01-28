#ifndef __main_HPP
#define __main_HPP

#include "llvm/ADT/DenseMap.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "llvm/Support/Casting.h"

// class VarDecl;
using namespace clang;
using UniqueRef = std::pair<DeclRefExpr*, uint64_t>;
struct RefPair {
  UniqueRef ref1;
  UniqueRef ref2;
  bool operator==(const RefPair &other) const {
    return (ref1 == other.ref1 && ref2 == other.ref2) ||
           (ref2 == other.ref1 && ref1 == other.ref2);
  }
  RefPair(UniqueRef r1, UniqueRef r2) : ref1(r1), ref2(r2) {}
};

template <>
struct llvm::DenseMapInfo<RefPair> {
  static inline RefPair getEmptyKey() {
    auto emptyKey = DenseMapInfo<UniqueRef>::getEmptyKey();
    return RefPair{emptyKey, emptyKey};
  }
  static inline RefPair getTombstoneKey() {
    auto tombKey = DenseMapInfo<UniqueRef>::getTombstoneKey();
    return RefPair{tombKey, tombKey};
  }
  static unsigned getHashValue(const RefPair &Val) {
    return DenseMapInfo<UniqueRef>::getHashValue(Val.ref1) +
           DenseMapInfo<UniqueRef>::getHashValue(Val.ref2);
  }
  static bool isEqual(const RefPair &LHS, const RefPair &RHS) {
    return LHS == RHS;
  }
};

#endif