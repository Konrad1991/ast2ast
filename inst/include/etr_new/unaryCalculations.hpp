#ifndef UNARYOPERATION
#define UNARYOPERATION

#include "UtilsTraits.hpp"

namespace etr {

template<typename I, UnaryFct f, typename Trait = UnaryTrait, typename CTrait = UnaryTrait>
struct UnaryOperation {
	using TypeTrait = Trait;
	using CaseTrait = Trait;
	const I& obj;
	MatrixParameter mp;
	UnaryOperation(const UnaryOperation& other) : obj(other.obj), mp(other.mp) {}
  UnaryOperation(const UnaryOperation&& other) : obj(other.obj), mp(other.mp) {}
	UnaryOperation(const I& obj_) : obj(obj_) {}
	template<typename IType, UnaryFct fOther, typename TraitOther>
	UnaryOperation(const UnaryOperation<IType, fOther, TraitOther> & other) : obj(other.obj) {}
	double operator[](size_t i) const {
    constexpr bool isDouble = std::is_same_v<I, double>;
    if constexpr (isDouble) {
      return f(obj);
    } else if constexpr(!isDouble) {
    	return f(obj[i % obj.size()]);
    }
  }
  size_t size() const {
  	constexpr bool isDouble = std::is_same_v<I, double>;
    if constexpr (isDouble) {
      return 1;
    } else if constexpr(!isDouble) {
    	return obj.size();
    }	
  } 
};

template<typename T>
auto sinus(const T& obj) -> Vec<double,
								UnaryOperation<decltype(convert(obj).d), Sinus, SinusTrait>, UnaryTrait> {
	constexpr bool isDouble = std::is_same_v<T, double>;
	if constexpr(!isDouble) {
		Vec<double, UnaryOperation<decltype(obj.d), Sinus, SinusTrait>, UnaryTrait> 
			ret(UnaryOperation<decltype(obj.d), Sinus, SinusTrait>(obj.d));
			return ret;
	} else if constexpr(isDouble) {
		Vec<double, UnaryOperation<double, Sinus, SinusTrait>> 
			ret(UnaryOperation<double, Sinus, SinusTrait>{obj});
			return ret;
	}
}


} // etr

#endif