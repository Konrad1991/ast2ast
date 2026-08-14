#ifndef UNIROOT_ETR_HPP
#define UNIROOT_ETR_HPP

// uniroot_plain(f, lower, upper): root of f in [lower, upper], via Brent's
// method (bisection + inverse quadratic interpolation, safeguarded).
// Requires f(lower) and f(upper) to have opposite sign. NA in either bound,
// or an NA returned by f at any evaluated point, poisons the whole result --
// same convention as chol/solve.
//
// Unlike Newton-Raphson, the current best guess is never allowed to leave
// the bracket -- that alone guarantees convergence without a derivative.
// Each iteration first tries a fast step: secant (like Newton, but the
// slope comes from two function values instead of an analytic derivative)
// once two points are available, inverse quadratic interpolation (fit a
// quadratic through the last three (x, f(x)) points and solve it for
// f == 0) once three are available. If that step would land outside the
// shrinking bracket, or isn't shrinking it fast enough, it's rejected and a
// plain bisection step is taken instead -- so the method converges at least
// as fast as bisection, typically faster.

namespace etr {

// This is a plain rewrite of R_zeroin2 (R's own src/library/stats/src/zeroin.c,
// the Forsythe/Malcolm/Moler netlib algorithm) -- not a call into R's
// non-API symbol, a fresh port of the same formulas.
inline Double uniroot_plain(
  Double ax,
  Double bx,
  Double fa,
  Double fb,
  const std::function<Double(Double)>& f,
  Double& Tol,
  Integer& Maxit) {

  const double EPSILON = std::numeric_limits<double>::epsilon();

  Double a = ax;
  Double b = bx;
  Double c = a;
  Double fc = fa;
  Double tol = Tol;
  int maxit = get_val(Maxit) + 1;

  /* First test if we have found a root at an endpoint */
  if (fa == Double(0.0)) {
    Tol = 0.0;
    Maxit = 0;
    return a;
  }
  if (fb == Double(0.0)) {
    Tol = 0.0;
    Maxit = 0;
    return b;
  }

  while(maxit--) {
    // Distance from the last but one to the last approximation
    Double prev_step = b - a;
    Double tol_act; // Actual tolerance
    Double p; // Interpolation step
    Double q; // lated in the from p /q; division operations is delayed until the last moment
    Double new_step; // step at this iteration

    // swap data for be to be the best approximation
    if (abs(fc) < abs(fb)) {
      a = b;
      b = c;
      c = a;
      fa = fb;
      fb = fc;
      fc = fa;
    }
    tol_act = Double(2.0) * EPSILON * abs(b) + tol / Double(2.0);
    new_step = (c - b) / Double(2.0);

    if (abs(new_step) <= tol_act || fb == Double(0.0)) {
      Maxit = Maxit - Integer(maxit);
      Tol = abs(c - b);
      return b; // Acceptable approximation is found
    }

    // Decide if the interpolation can be tried
    if (abs(prev_step) >= tol_act && // If prev_step was large enough
        abs(fa) > abs(fb) // and was in true direction
    ) { // interpolation may be tried
      Double t1;
      Double cb;
      Double t2;
      cb = c - b;
      if (a == c) { // If we have only two distinct points linear interpolation
        t1 = fb / fa;
        p = cb * t1;
        q = Double(1.0) - t1;
      } else { // quadratic inverse interpolation
        q = fa / fc;
        t1 = fb / fc;
        t2 = fb / fa;
        p = t2 * (cb * q * (q - t1) - (b - a)*(t1- Double(1.0)) );
        q = (q - Double(1.0)) * (t1 - Double(1.0)) * (t2 - Double(1.0));
      }

      // p was calculated with the opposite sign make p possitive and assign possible minus to q
      if (p > Double(0.0)) { 
        q = -q;
      } else {
        p = -p;
      }

      // If b + p/q falls in [b,c]
      if (p < (Double(0.75) * cb * q - abs(tol_act * q) / Double(2.0)) &&
          p < abs(prev_step*q/Double(2.0)) ) { // and isn't too large
        new_step = p / q; // it is accepted, if p/q is too large then the bisection procedure can reduce [b,c] range to more extent
      }
    }

    if (abs(new_step) < tol_act) { // Adjust the step to be not less than tolerance
      if (new_step > Double(0.0)) {
        new_step = tol_act;
      } else {
        new_step = -tol_act;
      }
    }
    // save the previous approximation
    a = b;
    fa = fb;
    b = b + new_step;
    fb = f(b); // do step to a new approximation

    if ( (fb > Double(0.0) && fc > Double(0.0)) ||
         (fb < Double(0.0) && fc < Double(0.0)) ) {
      // Adjust c for it to have a sign opposite to that of b
      c = a;
      fc = fa;
    }

  }

  // failed
  Tol = abs(c - b);
  Maxit = Integer(-1);
  return b;
}

struct uniroot_result{
  etr::Double root;
  etr::Double f_root;
  etr::Integer iter;
  etr::Double estim_prec;
  explicit uniroot_result(SEXP arg) :
    root(etr::SEXP2Scalar<etr::Double>(etr::checked_elt(arg, 0, 4, "uniroot_result"))),
    f_root(etr::SEXP2Scalar<etr::Double>(etr::checked_elt(arg, 1, 4, "uniroot_result"))),
    iter(etr::SEXP2Scalar<etr::Integer>(etr::checked_elt(arg, 2, 4, "uniroot_result"))),
    estim_prec(etr::SEXP2Scalar<etr::Double>(etr::checked_elt(arg, 3, 4, "uniroot_result"))) {}
  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 4));
    SET_VECTOR_ELT(res, 0, etr::Cast(root));
    SET_VECTOR_ELT(res, 1, etr::Cast(f_root));
    SET_VECTOR_ELT(res, 2, etr::Cast(iter));
    SET_VECTOR_ELT(res, 3, etr::Cast(estim_prec));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(names, 0, Rf_mkChar("root"));
    SET_STRING_ELT(names, 1, Rf_mkChar("f_root"));
    SET_STRING_ELT(names, 2, Rf_mkChar("iter"));
    SET_STRING_ELT(names, 3, Rf_mkChar("estim_prec"));
    Rf_setAttrib(res, R_NamesSymbol, names);
    Rf_setAttrib(res, R_ClassSymbol, Rf_mkString("uniroot_result"));
    UNPROTECT(2);
    return res;
  }
  uniroot_result() = default;
  uniroot_result(const uniroot_result&) = default;
  uniroot_result(uniroot_result&&) noexcept = default;
  uniroot_result& operator=(const uniroot_result&) = default;
  uniroot_result& operator=(uniroot_result&&) noexcept = default;

  static uniroot_result NA() {
    uniroot_result r;
    r.root = Double::NA();
    r.f_root = Double::NA();
    r.iter = Integer::NA();
    r.estim_prec = Double::NA();
    return r;
  }
};

template<typename B>
requires (IsArray<Decayed<B>>)
inline uniroot_result uniroot(const std::function<Double(Double)>& f, const B& boundaries, 
                      Double tol, Integer maxit) {
  uniroot_result res;
  // TODO: argument extendInt of uniroot
  Double lower = boundaries.get(0);
  Double upper = boundaries.get(1);

  if (lower.isNA() || upper.isNA()) return uniroot_result::NA();

  Double fa = f(lower);
  Double fb = f(upper);
  if (fa.isNA() || fb.isNA()) return uniroot_result::NA();
  // same check as R's own uniroot(): only error if same strict sign -- a
  // root exactly at one endpoint (product == 0) is fine, uniroot_plain
  // returns it immediately itself.
  ass<"uniroot: root not bracketed">(fa.val * fb.val <= 0.0);

  res.root = uniroot_plain(lower, upper, fa, fb, f, tol, maxit);
  res.f_root = f(res.root);
  res.iter = maxit;
  res.estim_prec = tol;
  return res;
}

} // namespace etr

#endif
