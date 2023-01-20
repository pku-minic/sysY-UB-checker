#ifndef __main_HPP
#define __main_HPP

#include "llvm/ADT/DenseMap.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "llvm/Support/Casting.h"

// class VarDecl;

struct SideEffect {
  uint32_t line;
  uint32_t col;
  VarDecl *sideVar;  // pointer param or global bar
  bool operator==(const SideEffect &other) { return sideVar == other.sideVar; }
  SideEffect() = default;
  SideEffect(uint32_t l, uint32_t c, VarDecl *v)
      : line(l), col(c), sideVar(v) {}
};

template <>
struct llvm::DenseMapInfo<SideEffect> {
  static inline SideEffect getEmptyKey() { return SideEffect{0, 0, nullptr}; }
  static inline SideEffect getTombstoneKey() {
    return SideEffect{0, 0, (VarDecl *)~0ul};
  }
  static unsigned getHashValue(const SideEffect &Val) {
    uint64_t ptrVal = (uint64_t)Val.sideVar;
    return (ptrVal >> 4) ^ (ptrVal >> 9);
  }
  static bool isEqual(const SideEffect &LHS, const SideEffect &RHS) {
    return LHS.sideVar == RHS.sideVar;
  }
};

#endif