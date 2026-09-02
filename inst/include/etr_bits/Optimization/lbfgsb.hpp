#ifndef LBFGSB_ETR_HPP
#define LBFGSB_ETR_HPP

// lbfgsb(f, x, lower, upper, maxit, factr, pgtol, lmm): bound-constrained
// L-BFGS-B, calling R's own C routine (src/appl/lbfgsb.c) directly -- no SEXP
// round-trip per evaluation. f is a translated fn() taking a double vector and
// returning a scalar double. Gradient: etr::jacobian under derivative =
// "forward"/"reverse", central differences otherwise.
//
// The objective/gradient callbacks passed to R are plain C function pointers
// (optimfn/optimgr) -- no captures. The translated functor is reached through
// the void* ex slot instead.

#ifndef STANDALONE_ETR
#include <R_ext/Applic.h>

namespace etr {

struct lbfgsb_result {
  Array<Double, Buffer<Double>> par;
  etr::Double value;
  etr::Integer convergence;
  Array<Integer, Buffer<Integer>> counts;

  explicit lbfgsb_result(SEXP arg) :
    par(etr::checked_elt(arg, "par", 4, "lbfgsb_result")),
    value(etr::SEXP2Scalar<etr::Double>(etr::checked_elt(arg, "value", 4, "lbfgsb_result"))),
    convergence(etr::SEXP2Scalar<etr::Integer>(etr::checked_elt(arg, "convergence", 4, "lbfgsb_result"))),
    counts(etr::checked_elt(arg, "counts", 4, "lbfgsb_result")) {}

  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 4));
    SET_VECTOR_ELT(res, 0, etr::Cast(par));
    SET_VECTOR_ELT(res, 1, etr::Cast(value));
    SET_VECTOR_ELT(res, 2, etr::Cast(convergence));
    SET_VECTOR_ELT(res, 3, etr::Cast(counts));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(names, 0, Rf_mkChar("par"));
    SET_STRING_ELT(names, 1, Rf_mkChar("value"));
    SET_STRING_ELT(names, 2, Rf_mkChar("convergence"));
    SET_STRING_ELT(names, 3, Rf_mkChar("counts"));
    Rf_setAttrib(res, R_NamesSymbol, names);
    Rf_setAttrib(res, R_ClassSymbol, Rf_mkString("lbfgsb_result"));
    UNPROTECT(2);
    return res;
  }

  lbfgsb_result() = default;
  lbfgsb_result(const lbfgsb_result&) = default;
  lbfgsb_result(lbfgsb_result&&) noexcept = default;
  lbfgsb_result& operator=(const lbfgsb_result&) = default;
  lbfgsb_result& operator=(lbfgsb_result&&) noexcept = default;

  static lbfgsb_result NA() {
    lbfgsb_result r;
    r.value = Double::NA();
    r.convergence = Integer::NA();
    return r;
  }
};

inline void print(const lbfgsb_result& x) {
  PRINT_STREAM << "lbfgsb_result" << std::endl;
  PRINT_STREAM << "  par:";
  for (std::size_t i = 0; i < x.par.size(); i++) PRINT_STREAM << " " << x.par.get(i);
  PRINT_STREAM << std::endl;
  PRINT_STREAM << "  value: " << x.value << std::endl;
  PRINT_STREAM << "  convergence: " << x.convergence << std::endl;
}

// ex slot: just a pointer to the translated functor. R's lbfgsb tracks the
// evaluation counts itself.
template<typename S, typename A>
struct LbfgsbCtx {
  const std::function<S(A)>* loss;
};

// Rebuild the AD input vector from R's raw par[] each call. Under reverse mode
// the tape has to be cleared first -- lbfgsb calls this hundreds of times and
// each call re-records the objective, so without clear() the tape grows without
// bound and the cached node ids from earlier calls would go stale.
template<typename S, typename A>
inline A lbfgsb_make_x(int n, const double* par) {
  using VT = typename ExtractDataType<Decayed<A>>::value_type;
  A x(SI{static_cast<std::size_t>(n)});
  for (int i = 0; i < n; i++) {
    if constexpr (IsReverseDouble<VT>) {
      x.set(i, ReverseDouble::Var(par[i]));
    } else if constexpr (IsDual<VT>) {
      x.set(i, Dual(par[i], 0.0));
    } else {
      x.set(i, par[i]);
    }
    }
    return x;
}

template<typename S, typename A>
double lbfgsb_fn(int n, double* par, void* ex) {
  using VT = typename ExtractDataType<Decayed<A>>::value_type;
  auto* ctx = static_cast<LbfgsbCtx<S, A>*>(ex);
  if constexpr (IsReverseDouble<VT>) TAPE_INTERN.clear();
  return get_val((*ctx->loss)(lbfgsb_make_x<S, A>(n, par)));
}

template<typename S, typename A>
void lbfgsb_gr(int n, double* par, double* grad, void* ex) {
  using VT = typename ExtractDataType<Decayed<A>>::value_type;
  auto* ctx = static_cast<LbfgsbCtx<S, A>*>(ex);
  if constexpr (IsReverseDouble<VT> || IsDual<VT>) {
    if constexpr (IsReverseDouble<VT>) TAPE_INTERN.clear();
    A x = lbfgsb_make_x<S, A>(n, par);
    auto jac = jacobian(*ctx->loss, x);          // 1 x n (scalar-output jacobian)
    for (int i = 0; i < n; i++) grad[i] = get_val(jac.get(i));
  } else {
    A x = lbfgsb_make_x<S, A>(n, par);
    for (int i = 0; i < n; i++) {
      const double xi = par[i]; 
      const double h  = 1e-3 * std::max(std::abs(xi), 1.0);
      x.set(i, xi + h); const double fp = get_val((*ctx->loss)(x));
      x.set(i, xi - h); const double fm = get_val((*ctx->loss)(x));
      x.set(i, xi);
      grad[i] = (fp - fm) / (2.0 * h);
    }
  }
}

// lower/upper: length 1 (broadcast) or length(x); non-finite on a side => that
// side is unbounded (nbd code 0/1/2/3, as in R's lbfgsb).
template<typename B>
inline double lbfgsb_bound(const B& b, int i, int n, const char* which) {
  if constexpr (IsArray<Decayed<B>>) {
    const std::size_t m = b.size();
    ass(m == 1 || m == static_cast<std::size_t>(n),
        std::string("lbfgsb: '") + which + "' must have length 1 or length(x)");
    return get_val(b.get(m == 1 ? std::size_t{0} : static_cast<std::size_t>(i)));
  } else {
    return get_val(b);
  }
}

template<typename S, typename A, typename XV, typename LO, typename UP,
         typename MI, typename FA, typename PG, typename LM>
inline lbfgsb_result lbfgsb(const std::function<S(A)>& loss,
                            const XV& x0, const LO& lower, const UP& upper,
                            const MI& maxit, const FA& factr,
                            const PG& pgtol, const LM& lmm) {
  const int n = static_cast<int>(x0.size());
  ass<"lbfgsb: x must have length >= 1">(n >= 1);

  std::vector<double> xv(static_cast<std::size_t>(n));
  std::vector<double> lo(static_cast<std::size_t>(n));
  std::vector<double> up(static_cast<std::size_t>(n));
  std::vector<int> nbd(static_cast<std::size_t>(n));
  for (int i = 0; i < n; i++) {
    xv[static_cast<std::size_t>(i)] = get_val(x0.get(static_cast<std::size_t>(i)));
    const double l = lbfgsb_bound(lower, i, n, "lower");
    const double u = lbfgsb_bound(upper, i, n, "upper");
    lo[static_cast<std::size_t>(i)] = l;
    up[static_cast<std::size_t>(i)] = u;
    const bool lf = std::isfinite(l);
    const bool uf = std::isfinite(u);
    nbd[static_cast<std::size_t>(i)] = lf ? (uf ? 2 : 1) : (uf ? 3 : 0);
  }

  LbfgsbCtx<S, A> ctx{&loss};

  double Fmin = 0.0;
  int fail = 0, fncount = 0, grcount = 0;
  char msg[60] = "";

  ::lbfgsb(n, static_cast<int>(get_val(lmm)), xv.data(), lo.data(), up.data(),
           nbd.data(), &Fmin, &lbfgsb_fn<S, A>, &lbfgsb_gr<S, A>, &fail,
           static_cast<void*>(&ctx),
           static_cast<double>(get_val(factr)), static_cast<double>(get_val(pgtol)),
           &fncount, &grcount, static_cast<int>(get_val(maxit)),
           msg, 0, 10);
  msg[sizeof(msg) - 1] = '\0';

  if (fail != 0) {
    std::string m(msg);
    if (m.empty()) m = "lbfgsb did not converge";
    warn(false, m);
  }

  lbfgsb_result res;
  Array<Double, Buffer<Double>> par_out(SI{static_cast<std::size_t>(n)});
  par_out.dim = std::vector<std::size_t>{static_cast<std::size_t>(n)};
  for (int i = 0; i < n; i++) par_out.set(i, Double(xv[static_cast<std::size_t>(i)]));
  res.par = par_out;
  res.value = Double(Fmin);
  res.convergence = Integer(fail);

  Array<Integer, Buffer<Integer>> counts(SI{2});
  counts.dim = std::vector<std::size_t>{2};
  counts.set(0, Integer(fncount));
  counts.set(1, Integer(grcount));
  res.counts = counts;
  return res;
}

// lbfgsb(f, ..., data): f takes (x, data). Bind data into a one-arg loss and
// delegate -- lbfgsb_fn/lbfgsb_gr and the internal jacobian stay one-argument.
template<typename S, typename A, typename D, typename XV, typename LO, typename UP,
         typename MI, typename FA, typename PG, typename LM, typename DArg>
inline lbfgsb_result lbfgsb(const std::function<S(A, D)>& loss,
                            const XV& x0, const LO& lower, const UP& upper,
                            const MI& maxit, const FA& factr,
                            const PG& pgtol, const LM& lmm, const DArg& data) {
  std::function<S(A)> wrapped =
    [&loss, &data](const A& xv) -> S { return loss(xv, data); };
  return lbfgsb(wrapped, x0, lower, upper, maxit, factr, pgtol, lmm);
}

} // namespace etr

#endif // !STANDALONE_ETR
#endif
