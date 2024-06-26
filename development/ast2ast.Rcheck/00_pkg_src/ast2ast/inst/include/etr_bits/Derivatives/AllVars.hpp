#ifndef ALLVARS_ETR_H
#define ALLVARS_ETR_H

#include "../BinaryCalculations.hpp"
#include "../Core.hpp"
#include "../UnaryCalculations.hpp"
#include "DerivTypes.hpp"

namespace etr {

template <class F, class... Args> inline F LoopVariadicT(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...);
  return f;
}

// TODO: the arguments passed to the function need own catzegories and have to
// be saved as pointer
template <int NBuffer_, int NBorrow_, int NBorrowSEXP_, int NConstants_>
struct AllVars {
  static constexpr int NBuffer = NBuffer_;
  static constexpr int NBorrow = NBorrow_;
  static constexpr int NBorrowSEXP = NBorrowSEXP_;
  static constexpr int NConstants = NConstants_;
  std::array<Vec<BaseType, Buffer<BaseType>>, NBuffer> varBuffer;
  std::array<Vec<BaseType, Borrow<BaseType>>, NBorrow> varBorrow;
  std::array<Vec<BaseType, BorrowSEXP<BaseType>>, NBorrowSEXP> varBorrowSEXP;
  std::array<Vec<BaseType, Buffer<BaseType>>, NConstants> varConstants;

  std::array<Buffer<BaseType>, NBuffer> varBufferDerivs;
  std::array<Buffer<BaseType>, NBorrow> varBorrowDerivs;
  std::array<Buffer<BaseType>, NBorrowSEXP> varBorrowSEXPDerivs;
  int IndepVarIdx;
  int IndepVarTypeIdx;
  bool DerivInit = false;

  AllVars(int IndepVarIdx_, int IndepVarTypeIdx_)
      : IndepVarIdx(IndepVarIdx_), IndepVarTypeIdx(IndepVarTypeIdx_) {}

  template <typename... Args> void initBuffer(Args &&...args) {
    int idx = 0;
    LoopVariadicT(
        [&](auto arg) {
          this->varBuffer[idx] = arg;
          idx++;
        },
        args...);
  }

  template <typename... Args> void initBorrow(Args &&...args) {
    int idx = 0;
    LoopVariadicT(
        [&](auto arg) {
          this->varBorrow[idx] = arg;
          idx++;
        },
        args...);
  }

  template <typename... Args> void initBorrowSEXP(Args &&...args) {
    int idx = 0;
    LoopVariadicT(
        [&](auto arg) {
          this->varBorrowSEXP[idx] = arg;
          idx++;
        },
        args...);
  }

  // TODO: check whether it is possible to use constexpr here
  std::size_t size(std::size_t Idx, int TypeIdx) const {
    if (TypeIdx == -1) {
      return varConstants[Idx].size();
    } else if (TypeIdx == 0) {
      return varBuffer[Idx].size();
    } else if (TypeIdx == 1) {
      return varBorrow[Idx].size();
    } else if (TypeIdx == 2) {
#ifdef STANDALONE_ETR
#else
      return varBorrowSEXP[Idx].size();
#endif
    }
    ass(false, "Unsupported type in derivative");
    return 0; // NOTE: just to please the compiler
  }

  bool im(std::size_t Idx, int TypeIdx) const {
    if (TypeIdx == -1) {
      return varConstants[Idx].im();
    } else if (TypeIdx == 0) {
      return varBuffer[Idx].im();
    } else if (TypeIdx == 1) {
      return varBorrow[Idx].im();
    } else if (TypeIdx == 2) {
#ifdef STANDALONE_ETR
#else
      return varBorrowSEXP[Idx].im();
#endif
    }
    ass(false, "Unsupported type in derivative");
    return 0; // NOTE: just to please the compiler
  }

  std::size_t nr(std::size_t Idx, int TypeIdx) const {
    if (TypeIdx == 0) {
      return varBuffer[Idx].nr();
    } else if (TypeIdx == 1) {
      return varBorrow[Idx].nr();
    } else if (TypeIdx == 2) {
#ifdef STANDALONE_ETR
#else
      return varBorrowSEXP[Idx].nr();
#endif
    }
    ass(false, "Unsupported type in derivative");
    return 0; // NOTE: just to please the compiler
  }

  std::size_t nc(std::size_t Idx, int TypeIdx) const {
    if (TypeIdx == 0) {
      return varBuffer[Idx].nc();
    } else if (TypeIdx == 1) {
      return varBorrow[Idx].nc();
    } else if (TypeIdx == 2) {
#ifdef STANDALONE_ETR
#else
      return varBorrowSEXP[Idx].nc();
#endif
    }
    ass(false, "Unsupported type in derivative");
    return 0; // NOTE: just to please the compiler
  }

  void resize(std::size_t Idx, int TypeIdx, std::size_t newSize) {
    if (TypeIdx == 0) {
      return varBuffer[Idx].resize(newSize);
    } else if (TypeIdx == 1) {
      return varBorrow[Idx].resize(newSize);
    } else if (TypeIdx == 2) {
#ifdef STANDALONE_ETR
#else
      return varBorrowSEXP[Idx].resize(newSize);
#endif
    }
  }

  void resizeDerivs(std::size_t Idx, int TypeIdx, std::size_t newSize) {
    if (TypeIdx == 0) {
      return varBufferDerivs[Idx].resize(newSize);
    } else if (TypeIdx == 1) {
      return varBorrowDerivs[Idx].resize(newSize);
    } else if (TypeIdx == 2) {
#ifdef STANDALONE_ETR
#else
      return varBorrowSEXPDerivs[Idx].resize(newSize);
#endif
    }
  }
};

} // namespace etr

#endif
