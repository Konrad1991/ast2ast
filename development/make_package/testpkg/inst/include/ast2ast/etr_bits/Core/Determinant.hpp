#ifndef DETERMINANT_ETR_HPP
#define DETERMINANT_ETR_HPP

namespace etr {

template<typename A> requires IsArray<Decayed<A>>
inline auto determinant(const A& arr) {
  using T = typename ExtractDataType<Decayed<A>>::value_type;
  const auto& dim = dim_view(arr.get_dim());
  ass<"Error in determinant: argument is not a matrix">(dim.size() == 2);
  ass<"Matrix is not quadratic">(dim[0] == dim[1]);
  const int n = static_cast<int>(dim[0]);

  if constexpr (IS<T, ReverseDouble>) {
    if (n == 0) return ReverseDouble(1.0);
    int id = TAPE_INTERN.push_det(
      static_cast<std::size_t>(n), matmul_extract_ids(arr)
    );
    return ReverseDouble(from_tape_id_t{}, id, TAPE_INTERN.is_na[id]);
  }
  else if constexpr (IS<T, Dual>) {
    int info = 0;
    std::vector<double> a = matmul_extract_dvals(arr);
    for (double v : a) if (std::isnan(v)) ass<"det: missing or infinite values in matrix">(false);
    std::vector<int> ipiv(n);
    std::vector<double> Adot = matmul_extract_dot(arr);
    if (n == 0) return(Dual(1.0, 0.0));
    F77_CALL(dgetrf)(&n, &n, a.data(), &n, ipiv.data(), &info);
    ass<"Error in determinant: illegal argument to dgetrf">(info >= 0);
    if (info > 0) return Dual(0.0, 0.0); // singular: U has an exact zero on the diagonal
    double prod = 1.0;
    for (int i = 0; i < n; i++) prod *= a[i*n + i];
    int swaps = 0;
    for (std::size_t i = 0; i < ipiv.size(); i++) {
      if (ipiv[i] != static_cast<int>(i) + 1) swaps++;
    }
    const double res = (swaps % 2 ? -prod : prod);
    std::vector<double> Ainv(n*n, 0.0);
    for (int i = 0; i < n; i++) {
      Ainv[static_cast<std::size_t>(i*n + i)] = 1.0;
    }
    F77_CALL(dgetrs)("N", &n, &n, a.data(), &n, ipiv.data(), Ainv.data(), &n, &info FCONE);
    ass<"Error in determinant: illegal argument to dgetrs">(info == 0);
    double trace = 0.0;
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        trace += Ainv[j*n + i] * Adot[i*n + j];
      }
    }
    return Dual(res, res * trace);
  }
  else if constexpr (IS<T, Double>) {
    int info = 0;
    std::vector<double> a = matmul_extract_dvals(arr);
    for (double v : a) if (std::isnan(v)) ass<"det: missing or infinite values in matrix">(false);
    std::vector<int> ipiv(n);
    if (n == 0) return(Double(1.0));
    F77_CALL(dgetrf)(&n, &n, a.data(), &n, ipiv.data(), &info);
    ass<"Error in determinant: illegal argument to dgetrf">(info >= 0);
    if (info > 0) return Double(0.0); // singular: U has an exact zero on the diagonal
    double prod = 1.0;
    for (int i = 0; i < n; i++) prod *= a[i*n + i];
    int swaps = 0;
    for (std::size_t i = 0; i < ipiv.size(); i++) {
      if (ipiv[i] != static_cast<int>(i) + 1) swaps++;
    }
    const double res = (swaps % 2 ? -prod : prod);
    return Double(res);
  } else {
    ass<"You cannot caluclate the determinant as the entries are not of type double">(false);
    return Double(1.0); // please the compiler
  }
}

}

#endif
