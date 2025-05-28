#ifndef PRINT_ETR_HPP
#define PRINT_ETR_HPP

// TODO: fix the printing if NA and Inf

namespace etr {
#ifdef STANDALONE_ETR
#define PRINT_STREAM std::cout
#else
#define PRINT_STREAM Rcpp::Rcout
#endif

inline void print() { PRINT_STREAM << std::endl; }

template <typename T>
  requires isBID<T>
inline void print(const T &inp) {
  if constexpr (IS<T, bool>) {
    PRINT_STREAM << std::boolalpha << inp << " ";
  } else {
    PRINT_STREAM << inp << std::endl;
  }
  PRINT_STREAM << std::endl;
}

inline void print(const char *inp) {
  if constexpr (IS<T, bool>) {
    PRINT_STREAM << inp << " ";
  } else {
    PRINT_STREAM << inp << std::endl;
  }
  PRINT_STREAM << std::endl;
}

template <typename Object>
inline void print(const Object& obj) {
  PRINT_STREAM << std::boolalpha;
  if constexpr (IsVec<Object>) {
    for (std::size_t i = 0; i < obj.size(); i++)
      PRINT_STREAM << obj[i] << " ";
    PRINT_STREAM << std::endl;
  } else if constexpr(IsMat<Object>) {
    using RetType = typename ExtractDataType<Decayed<Object>>::RetType;
    Mat<RetType> res; res = obj;// copy/eval because of subsetted matrices
    for (std::size_t r = 0; r < res.nr(); r++) {
      for (std::size_t c = 0; c < res.nc(); c++) {
        PRINT_STREAM << res[c* res.nr() + r] << " ";
      }
      PRINT_STREAM << std::endl;
    }
  } else {
    ass<"unsupported object in print">(false);
  }

}

} // namespace etr

#endif
