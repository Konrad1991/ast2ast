#ifndef DERIVS_H
#define DERIVS_H

#include "UtilsTraits.hpp"
#include "unaryCalculations.hpp"
#include "binaryCalculations.hpp"

/*
  Assume v.l and v.r are of type Buffer. 
  Than lDeriv and rDeriv are of type Vec<Buffer>.
  Thus, l, r, lDeriv and rDeriv are objects which exists. 
  Here, it is possible to use pointers to them instead of copying. 
  if e.g. v.l is of type Operation I will move the object. 

  Oruse dfdr and pass all expression created in R?
  --> Does not work as variables are removed which store old derivs
*/

// issue: add operation for bracket ()


namespace etr{

template <typename T>
struct ExtractTypeD;
template <typename T, typename R, typename Trait>
struct ExtractTypeD<Vec<T, R, Trait>> {
    using type = R;
};
template <typename T>
using ExtractedTypeD = typename ExtractTypeD<T>::type;

template<typename T>
constexpr T getL()  { return T(); }

template<typename T>
constexpr T getR() { return T(); }

/*
1. Construct a VarPointer Type
2. define size, im, nr and nc
3. Overload operator[]
4. operator+, operator* for VarPointer
	4.1 A BinaryType class instance has to be the result
5. walk funcion has to be updated. 
	5.1 check for IsVariableType, IsBinaryType, UnaryType
*/

template <class F, class... Args> inline F LoopVariadicT(F f, Args &&...args) {
  (f(std::forward<Args>(args)), ...);
  return f;
}
template<int NBuffer, int NBorrow, int NBorrowSEXP>
struct AllVars {
	std::array<Buffer<BaseType>*, NBuffer> varBuffer;
	std::array<Borrow<BaseType>*, NBorrow> varBorrow;
	std::array<BorrowSEXP<BaseType>*, NBorrowSEXP> varBorrowSEXP;
	std::array<Buffer<BaseType>, NBuffer + NBorrow + NBorrowSEXP> Derivs;

	template <typename ... Args>
	void initBuffer(Args &&...args) {
		int idx = 0;
                LoopVariadicT(
		      [&](auto arg) {
			this -> varBuffer[idx] = &arg; 
			idx++;
      		},
      		args...);
	}

	template <typename ... Args>
	void initBorrow(Args &&...args) {
		int idx = 0;
                LoopVariadicT(
		      [&](auto arg) {
			this -> varBorrow[idx] = &arg; 
			idx++;
      		},
      		args...);
	}

	template <typename ... Args>
	void initBorrowSEXP(Args &&...args) {
		int idx = 0;
                LoopVariadicT(
		      [&](auto arg) {
			this -> varBorrowSEXP[idx] = &arg; 
			idx++;
      		},
      		args...);
	}
	
	std::size_t size(size_t Idx) {
		if(Idx < NBuffer) {
	 		return varBuffer[Idx] -> size(); 
		} else if(Idx >= NBuffer && Idx < NBorrowSEXP) {
			return varBorrow[Idx - NBuffer] -> size();
		} else {
			return varBorrowSEXP[Idx - NBuffer - NBorrowSEXP] -> size();
		}
	}

	bool im(size_t Idx) {
		if(Idx < NBuffer) {
	 		return varBuffer[Idx] -> im(); 
		} else if(Idx >= NBuffer && Idx < NBorrowSEXP) {
			return varBorrow[Idx - NBuffer] -> im();
		} else {
			return varBorrowSEXP[Idx - NBuffer - NBorrowSEXP] -> im();
		}
	}

	std::size_t nr(size_t Idx) {
		if(Idx < NBuffer) {
	 		return varBuffer[Idx] -> nr(); 
		} else if(Idx >= NBuffer && Idx < NBorrowSEXP) {
			return varBorrow[Idx - NBuffer] -> nr();
		} else {
			return varBorrowSEXP[Idx - NBuffer - NBorrowSEXP] -> nr();
		}
	}

	std::size_t nc(size_t Idx) {
		if(Idx < NBuffer) {
	 		return varBuffer[Idx] -> nc(); 
		} else if(Idx >= NBuffer && Idx < NBorrowSEXP) {
			return varBorrow[Idx - NBuffer] -> nc();
		} else {
			return varBorrowSEXP[Idx - NBuffer - NBorrowSEXP] -> nc();
		}
	}

};

struct VarPointerTrait {};

template<typename T, int Idx, typename Trait = VarPointerTrait>
struct VarPointer {
	static constexpr int I = Idx;
	T& AllVarsRef;
	VarPointer(T& AllVars_) : AllVarsRef(AllVars_) {} 
	size_t size() {
		return AllVarsRef.size(Idx);
	}

	bool im() {
		return AllVarsRef.im(Idx);
	}

	size_t nr() {
		return AllVarsRef.nr(Idx);
	}
	
        size_t nc() {
		return AllVarsRef.nc(Idx);
	}
        

	
};

template<typename L, typename R, int IdxL, int IdxR>
auto operator+(const VarPointer<L, IdxL>& l, const VarPointer<R, IdxR>& r) {
		return; 
}



template<typename L, typename R, typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
struct QuarternyType {
  using typeTraitL = L;
  using typeTraitR = R;
  using typeTraitLDeriv = LDeriv;
  using typeTraitRDeriv = RDeriv;
  using TypeTrait = Trait;
  using VarTrait = std::tuple<L, R, RDeriv, RDeriv>;
};
template<typename L, typename R, typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
inline constexpr QuarternyType <L, R, LDeriv, RDeriv, Trait, OpTrait> produceQuarternyType() {
  return QuarternyType<L, R, LDeriv, RDeriv, Trait, OpTrait>();    
}

template<typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
struct BinaryType {
  using typeTraitLDeriv = LDeriv;
  using typeTraitRDeriv = RDeriv;
  using TypeTrait = Trait;
  using VarTrait = std::tuple<LDeriv, RDeriv>;
};
template<typename LDeriv, typename RDeriv, typename Trait, typename OpTrait>
inline constexpr BinaryType<LDeriv, RDeriv, Trait, OpTrait> produceBinaryType() {
  return BinaryType<LDeriv, RDeriv, Trait, OpTrait>();
}

template <typename I, typename Trait, typename OpTrait>
struct UnaryType {};
template <typename I, typename Trait, typename OpTrait>
inline constexpr UnaryType<I, Trait, OpTrait> produceUnaryType() {
  return UnaryType<I, Trait, OpTrait>();
}

template<typename T>
struct VariableType {
  using Type = T;
};
template<typename T>
inline constexpr VariableType<T> produceVariableType() {
  return VariableType<T>();
}


template<typename T>
requires IsVariable<T>
inline constexpr auto walkT() -> VariableType<T> {
  return VariableType<T>();
}

template <typename T>
requires IsMultiplication<T>
inline constexpr auto walkT()  {
    constexpr auto L = produceVariableType< typename T::typeTraitL>();
    constexpr auto R = produceVariableType< typename T::typeTraitR>();
    constexpr auto LDeriv = walkT<typename T::typeTraitL>();
    constexpr auto RDeriv = walkT<typename T::typeTraitR>();

    return produceQuarternyType< decltype( L ),
                                 decltype( R ),   
                                 decltype( LDeriv ),
                                 decltype( RDeriv ),
                                 QuarternaryTrait, TimesDerivTrait>();
}

template <typename T>
requires IsAddition<T>
inline constexpr auto walkT()  {
    constexpr auto LDeriv = walkT<typename T::typeTraitL>();
    constexpr auto RDeriv = walkT<typename T::typeTraitR>();
    return produceBinaryType< decltype( LDeriv ),
                              decltype( RDeriv ),
                              BinaryTrait, PlusDerivTrait>();
} 

template<typename T>
requires (IsVec<T> && !IsVariable<T>)
inline void walkT() {
  using tD = ExtractedTypeD<T>;
  printTAST<tD>();
  constexpr auto res = walkT<tD>();
  std::cout << "final result "  << std::endl;
  printTAST<decltype(res)>();
  return;
}


} // namespace etr

#endif
