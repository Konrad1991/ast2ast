#ifndef DERIVATIVES_ETR_HPP
#define DERIVATIVES_ETR_HPP

namespace etr {

template<std::size_t YIdx, std::size_t XIdx, typename F, typename... Args>
struct BoundCall {
  std::tuple<F, Args...> t;

  template<class FF, class... AA> BoundCall(FF&& f, AA&&... args) : t(std::forward<FF>(f), std::forward<AA>(args)...) {}

  decltype(auto) operator()() {
    return std::apply([](auto& f, auto&... args) -> decltype(auto) {
      return std::invoke(f, args...);
    }, t);
  }

  decltype(auto) x()       { return std::get<XIdx + 1>(t); }
  decltype(auto) y()       { return std::get<YIdx + 1>(t); }
  decltype(auto) x() const { return std::get<XIdx + 1>(t); }
  decltype(auto) y() const { return std::get<YIdx + 1>(t); }
};

template<std::size_t YIdx, std::size_t XIdx, typename F, typename... Args>
auto make_bound_call(F&& f, Args&&... args) {
  using FF = Decayed<F>;
  using BC = BoundCall<YIdx, XIdx, FF, Decayed<Args>...>;
  return BC(std::forward<F>(f), std::forward<Args>(args)...);
}

template<typename I> inline std::size_t calc_idx_seed(const I& idx) {
  using DecayedI = Decayed<I>;
  if constexpr (IsScalarLike<DecayedI>) {
    return static_cast<std::size_t>(get_val(idx)) - 1;
  } else if constexpr (IsArray<DecayedI>) {
    ass<"Size of idx in seed has to be 1L">(idx.size() == 1);
    return static_cast<std::size_t>(get_val(idx.get(0))) - 1;
  }
}

template<typename T, typename I> requires IsArray<T> inline void seed(T& obj, const I& idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  const std::size_t i = calc_idx_seed(idx);
  obj.d.set_dot(i, 1.0);
}
template<typename T, typename I> requires IsArray<T> inline void unseed(T& obj, const I& idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  const std::size_t i = calc_idx_seed(idx);
  obj.d.set_dot(i, 0.0);
}
template<typename T, typename I> requires IsDual<T> inline void seed(T& obj, const I& idx) {
  const std::size_t i = calc_idx_seed(idx);
  ass<"Wrong index in seed">(i == 0);
  obj.dot = 1.0;
}
template<typename T, typename I> requires IsDual<T> inline void unseed(T& obj, const I& idx) {
  const std::size_t i = calc_idx_seed(idx);
  ass<"Wrong index in seed">(i == 0);
  obj.dot = 0.0;
}

template<typename T> requires IsDual<T> inline auto get_dot(const T& obj) {
  Array<Double, Buffer<Double, RBufferTrait>> res(SI{1});
  res.dim = std::vector<std::size_t>{1};
  res.set(0, Double(obj.dot));
  return res;
}
template<typename T> requires IsArray<T> inline auto get_dot(const T& obj) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  Array<Double, Buffer<Double, RBufferTrait>> res(SI{obj.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, obj.d.get_dot(i));
  }
  res.dim = std::vector<std::size_t>{obj.size()};
  return res;
}

template<typename T> requires IsArray<T> void seed(T& obj, std::size_t idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  obj.d.set_dot(idx, 1.0);
}
template<typename T> requires IsArray<T> void unseed(T& obj, std::size_t idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  obj.d.set_dot(idx, 0.0);
}

template<std::size_t YIdx, std::size_t XIdx, typename J, typename Fun, typename... Args> requires (IsArray<J>)
void jacobian_forward(J& jac, const Fun& fct, Args&&... args) {
  auto bc = make_bound_call<YIdx, XIdx>(fct, args...);
  auto x = bc.x().get();
  auto y = bc.y().get();
  const std::size_t nrow = y.size();
  const std::size_t ncol = x.size();
  jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
  for (std::size_t i = 0; i < ncol; i++) {
    seed(x, i);
    bc();
    for (std::size_t j = 0; j < nrow; j++) {
      jac.set( i * nrow + j, y.d.get_dot(j));
    }
    unseed(x, i);
  }
}

template<typename Of, typename Wrt> inline auto deriv(const Of& of, const Wrt& w_inp) {
  using DecayedOf = Decayed<Of>;
  using DecayedWrt = Decayed<Wrt>;
  if constexpr (IsADType<DecayedOf> && IsADType<DecayedWrt>) {
    const auto w = wrt(w_inp);
    const auto d = derivatives(of, w);
    return d[0];
  }
  else if constexpr (IsArray<DecayedOf> && IsADType<DecayedWrt>) {
    using DataTypeOf = typename ExtractDataType<DecayedOf>::value_type;
    static_assert(IsADType<DataTypeOf>, "data type of of has to be Variable<Double>");
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{of.size()});
    for (std::size_t i = 0; i < res.size(); i++) {
      const auto w = wrt(w_inp);
      const auto d = derivatives(of.get(i), w);
      res.set(i, d[0]);
    }
    return res;
  }
  else if constexpr (IsADType<DecayedOf> && IsArray<DecayedWrt>) {
    using DataTypeWrt = typename ExtractDataType<DecayedWrt>::value_type;
    static_assert(IsADType<DataTypeWrt>, "data type of wrt has to be Variable<Double>");
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{w_inp.size()});
    for (std::size_t i = 0; i < res.size(); i++) {
      const auto w = wrt(w_inp.get(i));
      const auto d = derivatives(of, w);
      res.set(i, d[0]);
    }
    return res;
  }
  else if constexpr (IsArray<DecayedOf> && IsArray<DecayedWrt>) {
    using DataTypeWrt = typename ExtractDataType<DecayedWrt>::value_type;
    static_assert(IsADType<DataTypeWrt>, "data type of wrt has to be Variable<Double>");
    using DataTypeOf = typename ExtractDataType<DecayedOf>::value_type;
    static_assert(IsADType<DataTypeOf>, "data type of of has to be Variable<Double>");

    const std::size_t nrow = of.size();
    const std::size_t ncol = w_inp.size();
    Array<Double, Buffer<Double, RBufferTrait>> res = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
    for (std::size_t i = 0; i < ncol; i++) {
      const auto w = wrt(w_inp.get(i));
      for (std::size_t j = 0; j < nrow; j++) {
        const auto deriv = derivatives(of.get(j), w);
        res.set(i * nrow + j, deriv[0]);
      }
    }
    return res;
  }
}

template<std::size_t YIdx, std::size_t XIdx, typename J, typename Fun, typename... Args> requires (IsArray<J>)
void jacobian_backward(J& jac, const Fun& fct, Args&&... args) {
  auto bc = make_bound_call<YIdx, XIdx>(fct, args...);
  const auto x = bc.x().get();
  const auto y = bc.y().get();
  const std::size_t nrow = y.size();
  const std::size_t ncol = x.size();
  jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
  auto res = bc();
  for (std::size_t i = 0; i < ncol; i++) {
    const auto w = wrt(x.get(i));
    for (std::size_t j = 0; j < nrow; j++) {
      const auto deriv = derivatives(res.get(j), w);
      jac.set(i * nrow + j, deriv[0]);
    }
  }
}

} // namespace etr

#endif
