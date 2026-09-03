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
// [[ / at on a Dual array yields a DualRef, not a Dual -- template deduction
// pins T to DualRef before its operator Dual() conversion could apply, so
// the IsDual<T> overload above never matches it. Convert explicitly instead.
inline auto get_dot(const DualRef& obj) {
  return get_dot(Dual(obj));
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

// deriv(of, wrt): reverse-mode Jacobian extraction from the live tape.
// Each TAPE_INTERN.reverse(out_id) zeros all adjoints, seeds adj[out_id]=1,
// and propagates — yielding one full row of the Jacobian (one output vs. all
// inputs) per sweep. The tape itself is built during ordinary evaluation
// before this call; deriv() does not push any new nodes.
//
// Required invariant: for an Array<ReverseDouble, ...> argument, .size() and
// .get(i) must be pure reads of buffer state — they must not push new nodes
// onto TAPE_INTERN. If they did, the tape would grow between successive
// reverse() calls, which would (a) shift node indices the previously cached
// .id values still refer to, and (b) make adj[] reads land in nodes that
// were not part of the computation we're differentiating. Today
// Buffer<ReverseDouble> stores std::vector<ReverseDouble> and get(i) returns a
// reference to the stored handle (a plain read), so the invariant holds. If a
// future Buffer specialization lazily materializes nodes on access, snapshot
// the ids into a local std::vector<int> before the reverse-sweep loops below.
template<typename Of, typename Wrt> inline auto deriv(const Of& of, const Wrt& w_inp) {
  using DecayedOf = Decayed<Of>;
  using DecayedWrt = Decayed<Wrt>;
  if constexpr (IsReverseDouble<DecayedOf> && IsReverseDouble<DecayedWrt>) {
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{1});
    res.dim = std::vector<std::size_t>{1};
    TAPE_INTERN.reverse(of.id);
    res.set(0, Double(TAPE_INTERN.adj[static_cast<std::size_t>(w_inp.id)]));
    return res;
  }
  else if constexpr (IsArray<DecayedOf> && IsReverseDouble<DecayedWrt>) {
    using DataTypeOf = typename ExtractDataType<DecayedOf>::value_type;
    static_assert(IsReverseDouble<DataTypeOf>, "data type of of has to be ReverseDouble");
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{of.size()});
    res.dim = std::vector<std::size_t>{of.size()};
    for (std::size_t j = 0; j < res.size(); j++) {
      TAPE_INTERN.reverse(of.get(j).id);
      res.set(j, Double(TAPE_INTERN.adj[static_cast<std::size_t>(w_inp.id)]));
    }
    return res;
  }
  else if constexpr (IsReverseDouble<DecayedOf> && IsArray<DecayedWrt>) {
    using DataTypeWrt = typename ExtractDataType<DecayedWrt>::value_type;
    static_assert(IsReverseDouble<DataTypeWrt>, "data type of wrt has to be ReverseDouble");
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{w_inp.size()});
    res.dim = w_inp.get_dim(); // keep wrt's shape: deriv(scalar, matrix) -> matrix
    TAPE_INTERN.reverse(of.id);
    for (std::size_t i = 0; i < res.size(); i++) {
      res.set(i, Double(TAPE_INTERN.adj[static_cast<std::size_t>(w_inp.get(i).id)]));
    }
    return res;
  }
  else if constexpr (IsArray<DecayedOf> && IsArray<DecayedWrt>) {
    using DataTypeWrt = typename ExtractDataType<DecayedWrt>::value_type;
    static_assert(IsReverseDouble<DataTypeWrt>, "data type of wrt has to be ReverseDouble");
    using DataTypeOf = typename ExtractDataType<DecayedOf>::value_type;
    static_assert(IsReverseDouble<DataTypeOf>, "data type of of has to be ReverseDouble");

    const std::size_t nrow = of.size();
    const std::size_t ncol = w_inp.size();
    Array<Double, Buffer<Double, RBufferTrait>> res = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
    for (std::size_t j = 0; j < nrow; j++) {
      TAPE_INTERN.reverse(of.get(j).id);
      for (std::size_t i = 0; i < ncol; i++) {
        res.set(i * nrow + j, Double(TAPE_INTERN.adj[static_cast<std::size_t>(w_inp.get(i).id)]));
      }
    }
    return res;
  }
}

template<std::size_t YIdx, std::size_t XIdx, typename J, typename Fun, typename... Args> requires (IsArray<J>)
void jacobian_backward(J& jac, const Fun& fct, Args&&... args) {
  auto bc = make_bound_call<YIdx, XIdx>(fct, args...);
  const auto x = bc.x().get();
  auto res = bc();
  const std::size_t nrow = res.size();
  const std::size_t ncol = x.size();
  jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
  for (std::size_t j = 0; j < nrow; j++) {
    TAPE_INTERN.reverse(res.get(j).id);
    for (std::size_t i = 0; i < ncol; i++) {
      jac.set(i * nrow + j, Double(TAPE_INTERN.adj[static_cast<std::size_t>(x.get(i).id)]));
    }
  }
}

// jacobian(f, x[, extra]): m-by-n Jacobian of f at x, column-major like R.
// Dispatch on x's scalar type -- forward mode for Dual, reverse mode for
// ReverseDouble. `extra` is threaded through to f unchanged (as in uniroot).
// A scalar-valued f is allowed too: the result is the 1-by-n gradient row
// (used by lbfgsb for the objective's gradient).
template<typename X, typename Call>
inline auto jacobian_impl(const X& x, Call&& call_f) {
  using XT = typename ExtractDataType<Decayed<X>>::value_type;
  const std::size_t ncol = x.size();

  if constexpr (IsReverseDouble<XT>) {
    Array<ReverseDouble, Buffer<ReverseDouble>> xr(SI{ncol});
    for (std::size_t i = 0; i < ncol; i++) xr.set(i, ReverseDouble::Var(get_val(x.get(i))));

    auto y = call_f(xr);
    // scalar-valued f -> one gradient row (1 x ncol); vector-valued f -> full m x n
    if constexpr (IsArray<Decayed<decltype(y)>>) {
      const std::size_t nrow = y.size();
      auto jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
      for (std::size_t j = 0; j < nrow; j++) {
        TAPE_INTERN.reverse(y.get(j).id);
        for (std::size_t i = 0; i < ncol; i++) {
          jac.set(i * nrow + j, Double(TAPE_INTERN.adj[static_cast<std::size_t>(xr.get(i).id)]));
        }
      }
      return jac;
    } else {
      auto jac = matrix(Double(0.0), Integer(1), Integer(static_cast<int>(ncol)));
      TAPE_INTERN.reverse(y.id);
      for (std::size_t i = 0; i < ncol; i++) {
        jac.set(i, Double(TAPE_INTERN.adj[static_cast<std::size_t>(xr.get(i).id)]));
      }
      return jac;
    }
  } else {
    Array<Dual, Buffer<Dual>> xd(SI{ncol});
    for (std::size_t i = 0; i < ncol; i++) xd.set(i, Dual(get_val(x.get(i)), 0.0));

    if constexpr (IsArray<Decayed<decltype(call_f(xd))>>) {
      // seed column 0 first to learn f's output length, then the rest
      seed(xd, std::size_t{0});
      auto y0 = call_f(xd);
      const std::size_t nrow = y0.size();
      auto jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
      for (std::size_t j = 0; j < nrow; j++) jac.set(j, y0.d.get_dot(j));
      unseed(xd, std::size_t{0});

      for (std::size_t i = 1; i < ncol; i++) {
        seed(xd, i);
        auto y = call_f(xd);
        for (std::size_t j = 0; j < nrow; j++) jac.set(i * nrow + j, y.d.get_dot(j));
        unseed(xd, i);
      }
      return jac;
    } else {
      auto jac = matrix(Double(0.0), Integer(1), Integer(static_cast<int>(ncol)));
      for (std::size_t i = 0; i < ncol; i++) {
        seed(xd, i);
        jac.set(i, Double(call_f(xd).dot));
        unseed(xd, i);
      }
      return jac;
    }
  }
}

template<typename F, typename X> requires (IsArray<Decayed<X>>)
inline auto jacobian(const F& f, const X& x) {
  return jacobian_impl(x, [&f](const auto& xv) { return f(xv); });
}

template<typename F, typename X, typename E> requires (IsArray<Decayed<X>>)
inline auto jacobian(const F& f, const X& x, const E& extra) {
  return jacobian_impl(x, [&f, &extra](const auto& xv) { return f(xv, extra); });
}

} // namespace etr

#endif
