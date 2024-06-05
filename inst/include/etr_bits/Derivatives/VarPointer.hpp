#ifndef VAR_POINTER_ETR_H
#define VAR_POINTER_ETR_H

#include "../BinaryCalculations.hpp"
#include "../Core.hpp"
#include "../UnaryCalculations.hpp"

namespace etr {

template <typename T, int Idx, int TypeIdx, typename Trait> struct VarPointer {
  using TypeTrait = Trait;
  using CaseTrait = Trait;
  using Type = T;
  using RetType = T;
  static constexpr int I = Idx;
  static constexpr int TIdx = TypeIdx;

  T &AllVarsRef;
  VarPointer(T &AllVars_) : AllVarsRef(AllVars_) {}

  std::size_t size() const { return AllVarsRef.size(Idx, TypeIdx); }

  bool im() const { return AllVarsRef.im(Idx, TypeIdx); }

  std::size_t nr() const { return AllVarsRef.nr(Idx, TypeIdx); }

  std::size_t nc() const { return AllVarsRef.nc(Idx, TypeIdx); }

  void resize(std::size_t newSize) { AllVarsRef.resize(Idx, TIdx, newSize); }

  template <typename AV> static std::size_t getSize(AV &av) {
    if constexpr (TypeIdx == -1) {
      return av.varConstants[Idx].size();
    } else if constexpr (TypeIdx == 0) {
      return av.varBuffer[Idx].size();
    } else if constexpr (TypeIdx == 1) {
      return av.varBorrow[Idx].size();
    } else if constexpr (TypeIdx == 2) {
      return av.varBorrowSEXP[Idx].size();
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  template <typename AV>
  void setMatrix(AV &av, bool is, std::size_t rows, std::size_t cols) {
    if constexpr (TypeIdx == -1) {
      av.varConstants[Idx].d.mp.setMatrix(is, rows, cols);
    } else if constexpr (TypeIdx == 0) {
      av.varBuffer[Idx].d.mp.setMatrix(is, rows, cols);
    } else if constexpr (TypeIdx == 1) {
      av.varBorrow[Idx].d.mp.setMatrix(is, rows, cols);
    } else if constexpr (TypeIdx == 2) {
      av.varBorrowSEXP[Idx].d.mp.setMatrix(is, rows, cols);
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  auto get() const {
    if constexpr (TypeIdx == -1) {
      return AllVarsRef.varConstants[Idx];
    } else if constexpr (TypeIdx == 0) {
      return AllVarsRef.varBuffer[Idx];
    } else if constexpr (TypeIdx == 1) {
      return AllVarsRef.varBorrow[Idx];
    } else if constexpr (TypeIdx == 2) {
      return AllVarsRef.varBorrowSEXP[Idx];
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  auto getDerivObj() const {
    if constexpr (TypeIdx == -1) {
      static_assert(sizeof(T) == 0, "There is no deriv for constants");
    } else if constexpr (TypeIdx == 0) {
      return AllVarsRef.varBufferDerivs[Idx];
    } else if constexpr (TypeIdx == 1) {
      return AllVarsRef.varBorrowDerivs[Idx];
    } else if constexpr (TypeIdx == 2) {
      return AllVarsRef.varBorrowSEXPDerivs[Idx];
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  // TODO: change BaseType to the correct type
  // TODO: remove method
  template <typename AV> static auto getPtr(AV &av, BaseType **ptr) {
    if constexpr (TypeIdx == -1) {
      *ptr = av.varConstants[Idx].d.p;
    } else if constexpr (TypeIdx == 0) {
      *ptr = av.varBuffer[Idx].d.p;
    } else if constexpr (TypeIdx == 1) {
      *ptr = av.varBorrow[Idx].d.p;
    } else if constexpr (TypeIdx == 2) {
      *ptr = av.varBorrowSEXP[Idx].d.p;
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  template <typename AV> static auto getVal(AV &av, std::size_t VecIdx) {
    if constexpr (TypeIdx == -1) {
      return av.varConstants[Idx][VecIdx % av.varConstants[Idx].size()];
    } else if constexpr (TypeIdx == 0) {
      return av.varBuffer[Idx][VecIdx % av.varBuffer[Idx].size()];
    } else if constexpr (TypeIdx == 1) {
      return av.varBorrow[Idx][VecIdx % av.varBorrow[Idx].size()];
    } else if constexpr (TypeIdx == 2) {
      return av.varBorrowSEXP[Idx][VecIdx % av.varBorrowSEXP[Idx].size()];
    } else {
      ass(false, "Unknown variable index found");
    }
  }
  // TODO: check why operator[] onyl works with auto and not RetType
  auto operator[](std::size_t VecIdx) const {
    if constexpr (TypeIdx == -1) {
      return AllVarsRef.varConstants[Idx][VecIdx];
    } else if constexpr (TypeIdx == 0) {
      return AllVarsRef.varBuffer[Idx][VecIdx];
    } else if constexpr (TypeIdx == 1) {
      return AllVarsRef.varBorrow[Idx][VecIdx];
    } else if constexpr (TypeIdx == 2) {
      return AllVarsRef.varBorrowSEXP[Idx][VecIdx];
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  auto &operator[](std::size_t VecIdx) {
    if constexpr (TypeIdx == -1) {
      return AllVarsRef.varConstants[Idx][VecIdx];
    } else if constexpr (TypeIdx == 0) {
      return AllVarsRef.varBuffer[Idx][VecIdx];
    } else if constexpr (TypeIdx == 1) {
      return AllVarsRef.varBorrow[Idx][VecIdx];
    } else if constexpr (TypeIdx == 2) {
      return AllVarsRef.varBorrowSEXP[Idx][VecIdx];
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  template <typename AV> static auto getDeriv(AV &av, std::size_t VecIdx) {
    if constexpr (TypeIdx == -1) {
      return 0.0;
      // av.varConstants[Idx][VecIdx %
      //                         av.varConstants[Idx].size()]; // return 0.0;
    } else if constexpr (TypeIdx == 0) {
      if (av.varBufferDerivs[Idx].size() != av.varBuffer[Idx].size()) {
        av.varBufferDerivs[Idx].resize(av.varBuffer[Idx].size());
        if ((Idx == av.IndepVarIdx) && !av.DerivInit) {
          av.varBufferDerivs[Idx].fill(1.0);
          av.DerivInit = true;
        }
      }
      return av.varBufferDerivs[Idx][VecIdx % av.varBufferDerivs[Idx].size()];
    } else if constexpr (TypeIdx == 1) {
      if (av.varBorrowDerivs[Idx].size() != av.varBorrow[Idx].size()) {
        av.varBorrowDerivs[Idx].resize(av.varBorrow[Idx].size());
        if ((Idx == av.IndepVarIdx) && !av.DerivInit) {
          av.varBorrowDerivs[Idx].fill(1.0);
          av.DerivInit = true;
        }
      }
      return av.varBorrowDerivs[Idx][VecIdx % av.varBorrowDerivs[Idx].size()];
    } else if constexpr (TypeIdx == 2) {
      if (av.varBorrowSEXPDerivs[Idx].size() != av.varBorrowSEXP[Idx].size()) {
        av.varBorrowSEXPDerivs[Idx].resize(av.varBorrowSEXP[Idx].size());
        if ((Idx == av.IndepVarIdx) && !av.DerivInit) {
          av.varBorrowSEXPDerivs[Idx].fill(1.0);
          av.DerivInit = true;
        }
      }
      return av.varBorrowSEXPDerivs[Idx][VecIdx %
                                         av.varBorrowSEXPDerivs[Idx].size()];
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  // NOTE: TypeIdx -1 not implemented as for a constant the derivative can not
  // be set
  template <typename AV, typename Val>
  static auto setDeriv(AV &av, std::size_t VecIdx, Val v) {
    if constexpr (TypeIdx == 0) {
      av.varBufferDerivs[Idx][VecIdx] = v;
    } else if constexpr (TypeIdx == 1) {
      av.varBorrowDerivs[Idx][VecIdx] = v;
    } else if constexpr (TypeIdx == 2) {
      av.varBorrowSEXPDerivs[Idx][VecIdx] = v;
    } else {
      ass(false, "Unknown variable index found");
    }
  }

  // NOTE: TypeIdx -1 not implemented as for a constant the value cannot be
  // changed
  template <typename AV, typename Val>
  static auto setVal(AV &av, std::size_t VecIdx, Val v) {
    if constexpr (TypeIdx == 0) {
      av.varBuffer[Idx][VecIdx] = v;
    } else if constexpr (TypeIdx == 1) {
      av.varBorrow[Idx][VecIdx] = v;
    } else if constexpr (TypeIdx == 2) {
      av.varBorrowSEXP[Idx][VecIdx] = v;
    } else {
      ass(false, "Unknown variable index found");
    }
  }
};

} // namespace etr

#endif
