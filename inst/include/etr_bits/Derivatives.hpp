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
