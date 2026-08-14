#ifndef NNLS_ETR_HPP
#define NNLS_ETR_HPP

// Direct port of the Lawson-Hanson NNLS algorithm (nnls.f, from "Solving
// Least Squares Problems", Lawson & Hanson 1974/1995 -- public domain,
// netlib.org/lawson-hanson/, see nnls.f next to this file). nnls isn't in
// base R at all (unlike uniroot); the CRAN `nnls` package just wraps this
// same Fortran.
//
// Templated over T (Double or Dual only -- see note at the bottom for why
// ReverseDouble is deliberately NOT here): every step is EXACT linear
// algebra (a Householder reflection or a Givens rotation), not an
// iterative approximation converging toward an answer the way Brent's
// root search is -- so running it directly with Dual arithmetic gives the
// exact forward derivative of x for the (value-determined) active set,
// not merely an approximation of one. All branch decisions (which column
// enters/leaves the active set, feasibility/degeneracy checks) compare on
// T's own comparison operators, which already read .val only, matching
// how solve()'s LU pivoting works.
//
// This is a first-pass, structurally faithful port -- variable names and
// control flow deliberately mirror nnls.f line-for-line (GOTOs become
// while/for loops with the same re-entry points) so it can be audited
// against the original. Not yet compiled/tested against nnls.f's own
// reference behavior -- check carefully before trusting it.

#include <vector>
#include <cstddef>

namespace etr {

namespace nnls_detail {

// G1: construct the Givens rotation (cterm, sterm) with
//   ( cterm sterm) (a)   (sig)
//   (-sterm cterm) (b) = (0  )
// sig = sqrt(a^2+b^2), computed via the larger-magnitude operand to avoid
// overflow (same trick as nnls.f's G1).
template<typename T>
inline void g1(T a, T b, T& cterm, T& sterm, T& sig) {
  if (abs(a) > abs(b)) {
    T xr = b / a;
    T yr = sqrt(T(1.0) + xr * xr);
    cterm = (a < T(0.0)) ? -(T(1.0) / yr) : (T(1.0) / yr); // sign(1/yr, a)
    sterm = cterm * xr;
    sig = abs(a) * yr;
    return;
  }
  if (!(b == T(0.0))) {
    T xr = a / b;
    T yr = sqrt(T(1.0) + xr * xr);
    sterm = (b < T(0.0)) ? -(T(1.0) / yr) : (T(1.0) / yr); // sign(1/yr, b)
    cterm = sterm * xr;
    sig = abs(b) * yr;
    return;
  }
  sig = T(0.0);
  cterm = T(0.0);
  sterm = T(1.0);
}

// "Procedure G2" (inlined in nnls.f, not a real subroutine there either):
// apply a Givens rotation to one pair of values.
template<typename T>
inline void g2_apply(T cc, T ss, T& x, T& y) {
  T temp = x;
  x = cc * temp + ss * y;
  y = -ss * temp + cc * y;
}

// H12, algorithm H1: construct a Householder reflection zeroing
// col0[l1..m] (1-indexed, inclusive) below the pivot row `lpivot`.
// col0 is a pointer to a column with an unused slot at index 0 (so
// col0[k] is Fortran row k directly, no off-by-one translation needed
// anywhere below) -- see nnls_core for how columns are laid out this way.
// Overwrites col0[lpivot] with the reflected value; col0[l1..m] keep
// their ORIGINAL values (h12_apply below needs them unchanged). Returns
// `up`, the scale h12_apply needs. An all-zero tail returns up=0 (no-op).
template<typename T>
inline T h12_construct(T* col0, int lpivot, int l1, int m) {
  T cl = abs(col0[lpivot]);
  for (int j = l1; j <= m; ++j) {
    if (abs(col0[j]) > cl) cl = abs(col0[j]);
  }
  if (cl == T(0.0)) return T(0.0); // all-zero tail: identity transform
  T clinv = T(1.0) / cl;
  T t0 = col0[lpivot] * clinv;
  T sm = t0 * t0;
  for (int j = l1; j <= m; ++j) {
    T t = col0[j] * clinv;
    sm = sm + t * t;
  }
  cl = cl * sqrt(sm);
  if (col0[lpivot] > T(0.0)) cl = -cl; // reflect away from the pivot's sign
  T up = col0[lpivot] - cl;
  col0[lpivot] = cl;
  return up;
}

// H12, algorithm H2: apply a transform built by h12_construct (same
// col0/up/lpivot/l1/m) to vector c0 (same "index 0 unused" convention;
// only indices lpivot and l1..m are touched).
template<typename T>
inline void h12_apply(const T* col0, T up, int lpivot, int l1, int m, T* c0) {
  T b = up * col0[lpivot];
  if (!(b < T(0.0))) return; // b must be < 0 for a genuine transform
  b = T(1.0) / b;
  T sm = c0[lpivot] * up;
  for (int i = l1; i <= m; ++i) sm = sm + c0[i] * col0[i];
  if (sm == T(0.0)) return; // already orthogonal to this vector
  sm = sm * b;
  c0[lpivot] = c0[lpivot] + sm * up;
  for (int i = l1; i <= m; ++i) c0[i] = c0[i] + sm * col0[i];
}

} // namespace nnls_detail

// nnls_core: solve min ||Ax-b||_2 s.t. x >= 0, via the Lawson-Hanson
// active-set algorithm.
//
// A: m x n, plain column-major (A[col*m + row], 0-indexed), NOT modified.
// b: length m, NOT modified (nnls.f overwrites both in place; this copies
//    into padded internal scratch instead, so the caller's data survives).
// x: length n, overwritten with the solution.
// w: length n, overwritten with the dual/gradient vector (w[i] == 0 for i
//    in the passive set, w[i] <= 0 for i in the active set at a solution).
// rnorm: overwritten with the residual norm ||Ax-b||.
// mode: 1 = success, 2 = bad dimensions (m<=0 or n<=0), 3 = iteration
//       count exceeded (3*n, same cap as nnls.f).
//
// Internally everything is 1-indexed with an unused slot at index/row/col
// 0 (A padded to an (m+1)-row stride per column) -- kept deliberately
// close to nnls.f's own indexing so this can be checked against it
// statement by statement, at the cost of one wasted row/col of storage.
template<typename T>
inline void nnls_core(const std::vector<T>& A, int m, int n, const std::vector<T>& b_in,
                       std::vector<T>& x, std::vector<T>& w, T& rnorm, int& mode) {
  using namespace nnls_detail;

  mode = 1;
  if (m <= 0 || n <= 0) { mode = 2; return; }

  // Padded 1-indexed scratch copies: Ap column j lives at
  // &Ap[j*(m+1)], with Ap[j*(m+1) + row] == A(row,j) for row = 1..m.
  std::vector<T> Ap(static_cast<std::size_t>(n + 1) * static_cast<std::size_t>(m + 1), T(0.0));
  auto Aref = [&](int row, int col) -> T& { return Ap[static_cast<std::size_t>(col) * (m + 1) + row]; };
  for (int col = 1; col <= n; ++col) {
    for (int row = 1; row <= m; ++row) {
      Aref(row, col) = A[static_cast<std::size_t>(col - 1) * m + (row - 1)];
    }
  }

  std::vector<T> b(m + 1, T(0.0));
  for (int i = 1; i <= m; ++i) b[i] = b_in[i - 1];

  x.assign(n, T(0.0));
  w.assign(n, T(0.0));
  std::vector<T> xp(n + 1, T(0.0));   // padded 1-indexed working copy of x
  std::vector<T> wp(n + 1, T(0.0));   // padded 1-indexed working copy of w
  std::vector<T> zz(m + 1, T(0.0));
  std::vector<int> index(n + 1, 0);

  int iter = 0;
  const int itmax = 3 * n;

  for (int i = 1; i <= n; ++i) index[i] = i;

  int iz2 = n;
  int iz1 = 1;
  int nsetp = 0;
  int npp1 = 1;

  // Solves the NSETP x NSETP upper-triangular system formed by the
  // passive set's Householder-reduced columns, in place in zz() -- the
  // "internal subroutine" at label 400 in nnls.f, reached there via a
  // computed GOTO (RTNKEY); here it's just called directly.
  auto solve_triangular = [&]() {
    for (int l = 1; l <= nsetp; ++l) {
      int ip = nsetp + 1 - l;
      if (l != 1) {
        for (int ii = 1; ii <= ip; ++ii) {
          zz[ii] = zz[ii] - Aref(ii, index[ip + 1]) * zz[ip + 1];
        }
      }
      int jjt = index[ip];
      zz[ip] = zz[ip] / Aref(ip, jjt);
    }
  };

  for (;;) { // ****** MAIN LOOP ******
    if (iz1 > iz2 || nsetp >= m) break; // -> TERMINATION

    // dual (negative gradient) vector w() for the active set
    for (int izc = iz1; izc <= iz2; ++izc) {
      int jc = index[izc];
      T sm = T(0.0);
      for (int l = npp1; l <= m; ++l) sm = sm + Aref(l, jc) * b[l];
      wp[jc] = sm;
    }

    int j = 0, iz = 0;
    T up = T(0.0);
    bool have_candidate = false;
    for (;;) { // candidate selection: find largest w(j), reject/retry, or give up
      T wmax = T(0.0);
      int izmax = 0;
      for (int izc = iz1; izc <= iz2; ++izc) {
        int jc = index[izc];
        if (wp[jc] > wmax) { wmax = wp[jc]; izmax = izc; }
      }
      if (!(wmax > T(0.0))) break; // Kuhn-Tucker conditions satisfied

      iz = izmax;
      j = index[iz];

      T asave = Aref(npp1, j);
      up = h12_construct(&Ap[static_cast<std::size_t>(j) * (m + 1)], npp1, npp1 + 1, m);
      T unorm = T(0.0);
      if (nsetp != 0) {
        for (int l = 1; l <= nsetp; ++l) unorm = unorm + Aref(l, j) * Aref(l, j);
      }
      unorm = sqrt(unorm);
      // DIFF(x,y) in nnls.f is just x - y (a machine-precision test helper,
      // kept explicit here for the same reason it's a separate call there)
      T diff = (unorm + abs(Aref(npp1, j)) * T(0.01)) - unorm;
      if (diff > T(0.0)) {
        for (int l = 1; l <= m; ++l) zz[l] = b[l];
        h12_apply(&Ap[static_cast<std::size_t>(j) * (m + 1)], up, npp1, npp1 + 1, m, zz.data());
        T ztest = zz[npp1] / Aref(npp1, j);
        if (ztest > T(0.0)) { have_candidate = true; break; }
      }
      // reject j: restore, zero its dual value, try the next-largest w(j)
      Aref(npp1, j) = asave;
      wp[j] = T(0.0);
    }
    if (!have_candidate) break; // -> TERMINATION

    for (int l = 1; l <= m; ++l) b[l] = zz[l];

    index[iz] = index[iz1];
    index[iz1] = j;
    iz1 = iz1 + 1;
    int piv = npp1; // pivot row shared by both H12 applications below
    nsetp = piv;
    npp1 = piv + 1;

    if (iz1 <= iz2) {
      for (int jz = iz1; jz <= iz2; ++jz) {
        int jj = index[jz];
        h12_apply(&Ap[static_cast<std::size_t>(j) * (m + 1)], up, piv, piv + 1, m,
                  &Ap[static_cast<std::size_t>(jj) * (m + 1)]);
      }
    }
    if (nsetp != m) {
      for (int l = npp1; l <= m; ++l) Aref(l, j) = T(0.0);
    }
    wp[j] = T(0.0);

    // zz already holds the transformed b from the acceptance step above
    solve_triangular();

    // ****** SECONDARY LOOP ******
    for (;;) {
      ++iter;
      if (iter > itmax) { mode = 3; goto termination; }

      T alpha = T(2.0);
      int jj = 0;
      for (int ip = 1; ip <= nsetp; ++ip) {
        int l = index[ip];
        if (!(zz[ip] > T(0.0))) {
          T t = -xp[l] / (zz[ip] - xp[l]);
          if (alpha > t) { alpha = t; jj = ip; }
        }
      }
      if (alpha == T(2.0)) {
        // all new coefficients are feasible -- accept and go back to the main loop
        for (int ip = 1; ip <= nsetp; ++ip) xp[index[ip]] = zz[ip];
        break;
      }

      for (int ip = 1; ip <= nsetp; ++ip) {
        int l = index[ip];
        xp[l] = xp[l] + alpha * (zz[ip] - xp[l]);
      }

      int i = index[jj];
      for (;;) { // move coeff i from set P to set Z, retry until the rest is feasible
        xp[i] = T(0.0);
        if (jj != nsetp) {
          int start = jj + 1;
          for (int jcol = start; jcol <= nsetp; ++jcol) {
            int ii = index[jcol];
            index[jcol - 1] = ii;
            T cc, ss, sig;
            g1(Aref(jcol - 1, ii), Aref(jcol, ii), cc, ss, sig);
            Aref(jcol - 1, ii) = sig;
            Aref(jcol, ii) = T(0.0);
            for (int l = 1; l <= n; ++l) {
              if (l != ii) g2_apply(cc, ss, Aref(jcol - 1, l), Aref(jcol, l));
            }
            g2_apply(cc, ss, b[jcol - 1], b[jcol]);
          }
        }
        npp1 = nsetp;
        nsetp = nsetp - 1;
        iz1 = iz1 - 1;
        index[iz1] = i;

        bool all_feasible = true;
        for (int jjc = 1; jjc <= nsetp; ++jjc) {
          int ic = index[jjc];
          if (!(xp[ic] > T(0.0))) { jj = jjc; i = ic; all_feasible = false; break; }
        }
        if (all_feasible) break;
      }

      for (int l = 1; l <= m; ++l) zz[l] = b[l];
      solve_triangular();
    }
  }

termination:
  T sm = T(0.0);
  if (npp1 <= m) {
    for (int i = npp1; i <= m; ++i) sm = sm + b[i] * b[i];
  } else {
    for (int j = 1; j <= n; ++j) wp[j] = T(0.0);
  }
  rnorm = sqrt(sm);

  for (int i = 1; i <= n; ++i) x[i - 1] = xp[i];
  for (int j = 1; j <= n; ++j) w[j - 1] = wp[j];
}

// nnls(A, b): DSL-facing wrapper, Double only for now (Dual/ReverseDouble
// are separate follow-ups, see TODOs below). A is m x n, b length m.
// Returns x (length n) as an etr::Array<Double,...>.
template<typename MA, typename MB>
requires (IsArray<Decayed<MA>> && IsArray<Decayed<MB>>)
inline auto nnls(const MA& Ain, const MB& bin) {
  const auto& dim = Ain.get_dim();
  ass<"nnls: A has to be a matrix">(dim.size() == 2);
  const int m = static_cast<int>(dim[0]);
  const int n = static_cast<int>(dim[1]);
  ass<"nnls: b must have as many entries as A has rows">(static_cast<int>(bin.size()) == m);

  std::vector<double> araw = matmul_extract_dvals(Ain);
  std::vector<double> braw = matmul_extract_dvals(bin);
  std::vector<Double> A(araw.size());
  for (std::size_t i = 0; i < araw.size(); ++i) A[i] = Double(araw[i]);
  std::vector<Double> b(braw.size());
  for (std::size_t i = 0; i < braw.size(); ++i) b[i] = Double(braw[i]);

  std::vector<Double> x, w;
  Double rnorm;
  int mode;
  nnls_core(A, m, n, b, x, w, rnorm, mode);
  ass<"nnls: bad dimensions">(mode != 2);
  ass<"nnls: iteration count exceeded">(mode != 3);

  Array<Double, Buffer<Double, RBufferTrait>> res(SI{static_cast<std::size_t>(n)});
  res.dim = std::vector<std::size_t>{static_cast<std::size_t>(n)};
  for (std::size_t i = 0; i < x.size(); ++i) res.set(i, x[i]);
  return res;
}

// TODO: ReverseDouble is deliberately not templated here. Unlike Dual,
// ReverseDouble builds a tape, and unrolling every Householder reflection
// and Givens rotation across up to 3*n outer steps would push a tape node
// per elementary op -- versus solve()'s single BlockOp per call with its
// hand-derived, efficient backward pass. The reverse-mode version should
// be a separate implementation built from repeated solve() calls on the
// passive-set submatrix instead, not a naive template instantiation of
// nnls_core above.

// TODO: DSL-facing wrapper (etr::nnls(...) taking etr::Array<Double,...>,
// mirroring how uniroot_plain -> etr::uniroot was layered) not written
// yet -- nnls_core above is the internal engine only.

} // namespace etr

#endif
