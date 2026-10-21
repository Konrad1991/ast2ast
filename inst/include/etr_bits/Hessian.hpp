#ifndef HESSIAN_ETR_HPP
#define HESSIAN_ETR_HPP

namespace etr {

// Forward-over-reverse on the tape (development/Plans/hessian.md).
// f is recorded once; per direction v: tangent forward sweep (dot = J v),
// then a second-order reverse sweep (adj_dot = H v at the inputs).
// Only tape[start, end) is swept; older nodes (outer translation, constants)
// have dot = 0 and collect no adjoints.
struct HessianSweep {
  const ReverseTape& tape;
  std::size_t start;
  std::size_t end;
  std::vector<double> dot;
  std::vector<double> adj;
  std::vector<double> adj_dot;

  HessianSweep(const ReverseTape& t, std::size_t s)
    : tape(t), start(s), end(t.size()), dot(end - s), adj(end - s), adj_dot(end - s) {}

  inline bool local(int id) const { return id >= 0 && static_cast<std::size_t>(id) >= start; }
  inline double value(int id) const { return tape.val[static_cast<std::size_t>(id)]; }
  inline double tangent(int id) const { return local(id) ? dot[static_cast<std::size_t>(id) - start] : 0.0; }
  inline void add(int id, double g, double g_dot) {
    if (!local(id)) return;
    adj[static_cast<std::size_t>(id) - start] += g;
    adj_dot[static_cast<std::size_t>(id) - start] += g_dot;
  }

  // first and second derivative of the unary op at x (y = op(x))
  static inline void unary_derivatives(ROp op, double x, double y, double& d1, double& d2) {
    switch (op) {
      case ROp::Neg:   d1 = -1.0; d2 = 0.0; break;
      case ROp::Sin:   d1 = std::cos(x); d2 = -std::sin(x); break;
      case ROp::Cos:   d1 = -std::sin(x); d2 = -std::cos(x); break;
      case ROp::Tan: {
        const double c = std::cos(x);
        d1 = 1.0 / (c * c);
        d2 = 2.0 * y * d1;
        break;
      }
      case ROp::Asin: {
        const double s = 1.0 - x * x;
        d1 = 1.0 / std::sqrt(s);
        d2 = x / (s * std::sqrt(s));
        break;
      }
      case ROp::Acos: {
        const double s = 1.0 - x * x;
        d1 = -1.0 / std::sqrt(s);
        d2 = -x / (s * std::sqrt(s));
        break;
      }
      case ROp::Atan: {
        const double s = 1.0 + x * x;
        d1 = 1.0 / s;
        d2 = -2.0 * x / (s * s);
        break;
      }
      case ROp::Sinh:  d1 = std::cosh(x); d2 = y; break;
      case ROp::Cosh:  d1 = std::sinh(x); d2 = y; break;
      case ROp::Tanh:  d1 = 1.0 - y * y; d2 = -2.0 * y * d1; break;
      case ROp::Exp:   d1 = y; d2 = y; break;
      case ROp::Log:   d1 = 1.0 / x; d2 = -1.0 / (x * x); break;
      case ROp::Log10: d1 = 1.0 / (x * std::log(10.0)); d2 = -d1 / x; break;
      case ROp::Sqrt:  d1 = 0.5 / y; d2 = -0.25 / (y * y * y); break;
      case ROp::Abs:   d1 = x >= 0.0 ? 1.0 : -1.0; d2 = 0.0; break;
      default:         d1 = 0.0; d2 = 0.0;
    }
  }

  // partial derivatives of the binary op: first (da, db), second (daa, dab, dbb)
  static inline void binary_derivatives(ROp op, double a, double b, double c,
                                        double& da, double& db, double& daa, double& dab, double& dbb) {
    switch (op) {
      case ROp::Add: da = 1.0; db = 1.0; daa = dab = dbb = 0.0; break;
      case ROp::Sub: da = 1.0; db = -1.0; daa = dab = dbb = 0.0; break;
      case ROp::Mul: da = b; db = a; daa = 0.0; dab = 1.0; dbb = 0.0; break;
      case ROp::Div:
        da = 1.0 / b;
        db = -a / (b * b);
        daa = 0.0;
        dab = -1.0 / (b * b);
        dbb = 2.0 * a / (b * b * b);
        break;
      case ROp::Pow: {
        // as reverse(): base rule y * x^(y-1) (finite at x = 0), log terms only for x > 0
        da = b * std::pow(a, b - 1.0);
        daa = b * (b - 1.0) * std::pow(a, b - 2.0);
        if (a > 0.0) {
          const double l = std::log(a);
          db = c * l;
          dab = std::pow(a, b - 1.0) * (1.0 + b * l);
          dbb = c * l * l;
        } else {
          db = dab = dbb = 0.0;
        }
        break;
      }
      default: da = db = daa = dab = dbb = 0.0;
    }
  }

  static inline bool is_block(ROp op) {
    return op == ROp::MatMul || op == ROp::BlockSlot || op == ROp::Chol || op == ROp::Solve ||
           op == ROp::CrossProd || op == ROp::TriSolve || op == ROp::Det;
  }

  inline void forward(const std::vector<int>& x_ids, const double* v) {
    std::fill(dot.begin(), dot.end(), 0.0);
    for (std::size_t k = 0; k < x_ids.size(); k++) dot[static_cast<std::size_t>(x_ids[k]) - start] = v[k];
    for (std::size_t i = start; i < end; i++) {
      const ROp op = tape.op[i];
      if (op == ROp::Var || op == ROp::Const || tape.is_na[i]) continue;
      const int ai = tape.a[i];
      const int bi = tape.b[i];
      if (bi < 0) {
        double d1, d2;
        unary_derivatives(op, value(ai), tape.val[i], d1, d2);
        dot[i - start] = d1 * tangent(ai);
      } else {
        double da, db, daa, dab, dbb;
        binary_derivatives(op, value(ai), value(bi), tape.val[i], da, db, daa, dab, dbb);
        dot[i - start] = da * tangent(ai) + db * tangent(bi);
      }
    }
  }

  inline void reverse(int out) {
    std::fill(adj.begin(), adj.end(), 0.0);
    std::fill(adj_dot.begin(), adj_dot.end(), 0.0);
    add(out, 1.0, 0.0);
    for (std::size_t i = end; i-- > start;) {
      const ROp op = tape.op[i];
      if (op == ROp::Var || op == ROp::Const || tape.is_na[i]) continue;
      const double w = adj[i - start];
      const double w_dot = adj_dot[i - start];
      // NOTE: as reverse(): a node not reaching the output would add 0 * Inf = NaN
      if (w == 0.0 && w_dot == 0.0) continue;
      const int ai = tape.a[i];
      const int bi = tape.b[i];
      if (bi < 0) {
        double d1, d2;
        unary_derivatives(op, value(ai), tape.val[i], d1, d2);
        add(ai, w * d1, w_dot * d1 + w * d2 * tangent(ai));
      } else {
        double da, db, daa, dab, dbb;
        binary_derivatives(op, value(ai), value(bi), tape.val[i], da, db, daa, dab, dbb);
        const double ta = tangent(ai);
        const double tb = tangent(bi);
        add(ai, w * da, w_dot * da + w * (daa * ta + dab * tb));
        add(bi, w * db, w_dot * db + w * (dab * ta + dbb * tb));
      }
    }
  }
};

// records f(x) with ReverseDouble at the end of the tape, hands the sweep to body, rewinds
template<typename X, typename Call, typename Body>
inline auto with_recorded_scalar_f(const X& x, Call&& call_f, Body&& body) {
  auto& tape = TAPE_INTERN;
  const std::size_t tape_start = tape.size();
  const std::size_t blocks_start = tape.blocks.size();
  const std::size_t n = x.size();
  Array<ReverseDouble, Buffer<ReverseDouble>> xr(SI{n});
  for (std::size_t j = 0; j < n; j++) xr.set(j, ReverseDouble::Var(get_val(x.get(j))));
  const ReverseDouble y = call_f(xr);
  for (std::size_t i = tape_start; i < tape.size(); i++) {
    ass<"hessian: matrix operations (%*%, solve, chol, det, crossprod, backsolve) are not supported in hessian yet">(
      !HessianSweep::is_block(tape.op[i]));
  }
  std::vector<int> x_ids(n);
  for (std::size_t j = 0; j < n; j++) x_ids[j] = xr.get(j).id;
  HessianSweep sweep(tape, tape_start);
  auto res = body(sweep, x_ids, y);
  tape.rewind(tape_start, blocks_start);
  return res;
}

template<typename X, typename Call>
inline auto hessian_impl(const X& x, Call&& call_f) {
  const std::size_t n = x.size();
  return with_recorded_scalar_f(x, call_f, [n](HessianSweep& sweep, const std::vector<int>& x_ids, const ReverseDouble& y) {
    auto H = matrix(Double(0.0), Integer(static_cast<int>(n)), Integer(static_cast<int>(n)));
    if (y.isNA()) {
      for (std::size_t k = 0; k < n * n; k++) H.set(k, Double::NA());
      return H;
    }
    std::vector<double> e(n, 0.0);
    for (std::size_t j = 0; j < n; j++) {
      e[j] = 1.0;
      sweep.forward(x_ids, e.data());
      sweep.reverse(y.id);
      e[j] = 0.0;
      for (std::size_t i = 0; i < n; i++) {
        H.set(j * n + i, Double(sweep.adj_dot[static_cast<std::size_t>(x_ids[i]) - sweep.start]));
      }
    }
    return H;
  });
}

template<typename X, typename V, typename Call>
inline auto hvp_impl(const X& x, const V& v, Call&& call_f) {
  const std::size_t n = x.size();
  ass<"hvp: x and v differ in length">(v.size() == n);
  std::vector<double> dir(n);
  for (std::size_t j = 0; j < n; j++) {
    ass<"hvp: NA in v">(!get_scalar_val(v.get(j)).isNA());
    dir[j] = static_cast<double>(get_val(v.get(j)));
  }
  return with_recorded_scalar_f(x, call_f, [n, &dir](HessianSweep& sweep, const std::vector<int>& x_ids, const ReverseDouble& y) {
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{n});
    res.dim = Dim{n};
    if (y.isNA()) {
      for (std::size_t i = 0; i < n; i++) res.set(i, Double::NA());
      return res;
    }
    sweep.forward(x_ids, dir.data());
    sweep.reverse(y.id);
    for (std::size_t i = 0; i < n; i++) {
      res.set(i, Double(sweep.adj_dot[static_cast<std::size_t>(x_ids[i]) - sweep.start]));
    }
    return res;
  });
}

// extra converted to the type f takes inside the recording, so its constants are rewound too
template<typename F, typename E>
inline auto call_with_extra(const F& f, const E& extra) {
  return [&f, &extra](const auto& xv) {
    if constexpr (requires { &F::operator(); }) {
      typename SecondArgOf<F>::type e;
      e = extra;
      return f(xv, e);
    } else {
      return f(xv, extra);
    }
  };
}

template<typename F, typename X> requires (IsArray<Decayed<X>>)
inline auto hessian(const F& f, const X& x) {
  return hessian_impl(x, [&f](const auto& xv) { return f(xv); });
}

template<typename F, typename X, typename E> requires (IsArray<Decayed<X>>)
inline auto hessian(const F& f, const X& x, const E& extra) {
  return hessian_impl(x, call_with_extra(f, extra));
}

template<typename F, typename X, typename V> requires (IsArray<Decayed<X>> && IsArray<Decayed<V>>)
inline auto hvp(const F& f, const X& x, const V& v) {
  return hvp_impl(x, v, [&f](const auto& xv) { return f(xv); });
}

template<typename F, typename X, typename V, typename E> requires (IsArray<Decayed<X>> && IsArray<Decayed<V>>)
inline auto hvp(const F& f, const X& x, const V& v, const E& extra) {
  return hvp_impl(x, v, call_with_extra(f, extra));
}

} // namespace etr

#endif
