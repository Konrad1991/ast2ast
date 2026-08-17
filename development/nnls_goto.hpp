#ifndef NNLS_GOTO_ETR_HPP
#define NNLS_GOTO_ETR_HPP

namespace etr {

// cvec (H12's C() argument) is sometimes a genuine rank-1 array (zz, dummy)
// and sometimes a subset(a, TRUE, col) view -- rank-2 ({m,1}), because that
// "2" is baked into the type itself (SubsetView<..., 2, ...>). Dispatch the
// at() arity at compile time on which one it is, so a single linear index
// (i2/i3/i4, matching the Fortran's flat C(*) addressing) works for both.
template<typename T>
inline decltype(auto) at_lin(T& cvec, Integer idx) {
  if constexpr (IsSubsetArray<Decayed<T>>) {
    return at(cvec, idx, Integer(1));
  } else {
    return at(cvec, idx);
  }
}

//  CONSTRUCTION AND/OR APPLICATION OF A SINGLE
//  HOUSEHOLDER TRANSFORMATION..     Q = I + U*(U**T)/B
template<typename SubsetArr, typename Arr>
void H12(Integer mode, Integer lpivot, Integer l1, Integer m,
         SubsetArr u, Integer iue, Double& up, Arr& cvec,
         Integer ice, Integer icv, Integer ncv) {

  Integer i2;
  Integer i3;
  Integer i4;
  Integer incr;
  Double b;
  Double cl;
  Double clinv;
  Double sm;

  if (Integer(0) >= lpivot || lpivot >= l1 || l1 > m) return;

  cl = abs(at(u, lpivot, Integer(1)));
  if (mode == Integer(2)) {
    if (cl <= Double(0.0)) {
      return;
    }
  } else {
    // construct transformation
    for (Integer j = l1; j <= m; j = j + Integer(1)) {
      cl = max(c(abs(at(u, j, Integer(1))), cl));
    }
    if (cl <= Double(0.0)) return;
    clinv = Double(1.0) / cl;
    sm = power(at(u, lpivot, Integer(1))*clinv, Double(2.0));
    for (Integer j = l1; j <= m; j = j + Integer(1)) {
      sm = sm + power(at(u, j, Integer(1))*clinv, Double(2.0));
    }
    cl = cl * sqrt(sm);
    if (at(u, lpivot, Integer(1)) > Double(0.0)) {
      cl = -cl;
    }
    up = at(u, lpivot, Integer(1)) - cl;
    at(u, lpivot, Integer(1)) = cl;
  }

  if (ncv <= Integer(0)) return;
  b = up * at(u, lpivot, Integer(1));
  // b must be nonpositive here
  if (b >= Double(0.0)) return;

  b = Double(1.0) / b;
  i2 = Integer(1) - icv + ice * (lpivot - Integer(1));
  incr = ice * (l1 - lpivot);
  for (Integer j = Integer(1); j <= ncv; j = j + Integer(1)) {
    i2 = i2 + icv;
    i3 = i2 + incr;
    i4 = i3;
    sm = at_lin(cvec, i2) * up;
    for (Integer i = l1; i <= m; i = i + Integer(1)) {
      sm = sm + at_lin(cvec, i3)*at(u, i, Integer(1));
      i3 = i3 + ice;
    }
    if (sm == Double(0.0)) continue;

    sm = sm * b;
    at_lin(cvec, i2) = at_lin(cvec, i2) + sm * up;
    for (Integer i = l1; i <= m; i = i + Integer(1)) {
      at_lin(cvec, i4) = at_lin(cvec, i4) + sm*at(u, i, Integer(1));
      i4 = i4 + ice;
    }
  }
}

// COMPUTE ORTHOGONAL ROTATION MATRIX:
//   ( cterm sterm) (a)   (sig)
//   (-sterm cterm) (b) = (0  )
// sig = sqrt(a^2+b^2), computed last: at the one call site sig aliases the
// same storage as a (both are at(A, J-1, II)), so a must be fully read
// before sig is written -- sig is templated (taken by value) so that when
// it's an at()-returned reference-proxy (e.g. DoubleRef), assigning to the
// local copy still writes through to the original storage.
void G1(DoubleRef a, DoubleRef b, Double& cterm, Double& sterm, DoubleRef sig) {
  const Double one = 1.0;
  const Double zero = 0.0;

  if (abs(a) > abs(b)) {
    Double xr = b / a;
    Double yr = sqrt(one + xr * xr);
    cterm = (a < zero) ? -(one / yr) : (one / yr); // sign(1/yr, a)
    sterm = cterm * xr;
    sig = abs(a) * yr;
    return;
  }

  if (b != zero) {
    Double xr = a / b;
    Double yr = sqrt(one + xr * xr);
    sterm = (b < zero) ? -(one / yr) : (one / yr); // sign(1/yr, b)
    cterm = sterm * xr;
    sig = abs(b) * yr;
    return;
  }

  sig = zero;
  cterm = zero;
  sterm = one;
}

template<typename Arr> requires (IsArray<Decayed<Arr>>)
void nnls_goto(Arr& a,
          const Integer mda,  const Integer m, const Integer n,
          Arr& b, Arr& x,
          Double& rnorm,  Arr& w,
          Arr& zz,  Array<Integer, Buffer<Integer>>& index,
          Integer& mode) {

  Integer i, ii, ip, iter, itmax, iz, iz1, iz2, izmax;
  Integer j, jj, jz, l;
  Integer npp1, nsetup;

  Double alpha, asave, cc;
  Double factor = 0.01;
  Double sm, si, t, temp, unorm, up, wmax;
  const Double two = 2.0;
  const Double zero = 0.0;
  Double ztest;

  Array<Double, Buffer<Double>> dummy = numeric(Integer(1));

  mode = Integer(1);
  if (m <= Integer(0) || n <= Integer(0)) {
    mode = Integer(2);
    return;
  }
  iter = Integer(0);
  itmax = Integer(3) * n;

  x = numeric(n);
  index = seq_len(n);

  iz2 = n;
  iz1 = Integer(1);
  nsetup = Integer(0);
  npp1 = Integer(1);

  const auto lambda_350 = [&]() {
    sm = zero;
    if (npp1 <= m) {
      for (i = npp1; i <= m; i = i + Integer(1)) {
        sm = sm + at(b, i)*at(b, i);
      }
    } else {
      w = numeric(n);
    }
    rnorm = sqrt(sm);
  };

  const auto lambda_400 = [&]() {
    for (l = Integer(1); l <= nsetup; l = l + Integer(1)) {
      ip = nsetup + Integer(1) - l;
      if (l != Integer(1)) {
        for (ii = Integer(1); ii <= ip; ii = ii + Integer(1)) {
          at(zz, ii) = at(zz, ii) - at(a, ii, jj) * at(zz, ip + Integer(1));
        }
      }
      jj = at(index, ip);
      at(zz, ip) = at(zz, ip) / at(a, ip, jj);
    }
  };

  while(true) {
    // Quit if all coefficients are already in the solution
    // or if m cols of a haave been triangualized
    if (iz1 > iz2 || nsetup >= m) {
      lambda_350(); return;
    }

    // compute components of the dual (negative gradient) vector w
    for (iz = iz1; iz <= iz2; iz = iz + Integer(1)) {
      j = at(index, iz);
      sm = zero;
      for (l = npp1; l <= m; l = l + Integer(1)) {
        sm = sm + at(a, l, j)*at(b, l);
      }
      at(w, j) = sm;
    }

    // Find largest positive w[j]
    const auto find_largest_positive_w = [&]() {
      wmax = zero;
      for (iz = iz1; iz <= iz2; iz = iz + Integer(1)) {
        j = at(index, iz);
        if (at(w, j) > wmax) {
          wmax = at(w, j);
          izmax = iz;
        }
      }
    };
  retry_60:
    find_largest_positive_w();

    // Is the KUHN-TUCKER conditions satsified?
    if (wmax <= zero) {
      lambda_350();
      return;
    }
    iz = izmax;
    j = at(index, iz);

    // The sign of w[j] is ok for j to be moved to set P
    // Begin transformation and check new diagonal element
    // to avoid near linear dependence
    asave = at(a, npp1, j);
    H12(
      Integer(1), npp1, npp1 + Integer(1), m, subset(a, Logical(true), j),
      Integer(1), up, dummy, Integer(1), Integer(1), Integer(0)
    );
    unorm = zero;
    if (nsetup != Integer(0)) {
      for (l = Integer(1); l <= nsetup; l = l + Integer(1)) {
        unorm = unorm + at(a, l, j)*at(a, l, j);
      }
    }
    unorm = sqrt(unorm);

    bool accepted = false;
    if ((unorm + abs(at(a, npp1, j))*factor - unorm) > zero) {
      // Col j is sufficiently independent. copy b into zz, update zz
      // and solve for ztest (= proposed new value for x[j])
      zz = b;
      H12(
        Integer(2), npp1, npp1 + Integer(1), m, subset(a, Logical(true), j),
        Integer(1), up, zz, Integer(1), Integer(1), Integer(1)
      );
      ztest = at(zz, npp1) / at(a, npp1, j);
      if (ztest > zero) accepted = true;
    }

    if (!accepted) {
      // Reject j: restore a(npp1,j), zero its dual value, retry
      // candidate selection from the top (label 60).
      at(a, npp1, j) = asave;
      at(w, j) = zero;
      goto retry_60;
    }

    // The index j = index[iz] has been selected to be moved from
    // Z zo set P. Update b, Update indices, Apply HOUSEHOLDER
    // TRANSFORMATION to cols in new set Z, ZERO Subdiagonal elements in
    // col j, set w[j] = 0
    b = zz;

    at(index, iz) = at(index, iz1);
    at(index, iz1) = j;
    iz1 = iz1 + Integer(1);
    nsetup = npp1;
    npp1 = npp1 + Integer(1);

    if (iz1 <= iz2) {
      for (jz = iz1; jz <= iz2; jz = jz + Integer(1)) {
        jj = at(index, jz);
        auto colJJ = subset(a, Logical(true), jj);
        H12(Integer(2), nsetup, npp1, m, subset(a, Logical(true), j),
            Integer(1), up, colJJ, Integer(1), mda, Integer(1)
            );
      }
    }

    if (nsetup != m) {
      for (l = npp1; l <= m; l = l + Integer(1)) {
        at(a, l, j) = zero;
      }
    }
    at(w, j) = zero;
    // solve triangualr system
    // store the solution temporarily in zz
    lambda_400();

    while (true) {
      iter = iter + Integer(1);
      if (iter > itmax) {
        mode = Integer(3);
        warn<"NNLS quitting on iteration count.">(false);
        lambda_350();
        return;
      }

      // check if all new constrained coefficients are feasable
      // if not compute alpha
      alpha = two;
      for (ip = Integer(1); ip <= nsetup; ip = ip + Integer(1)) {
        l = at(index, ip);
        if (at(zz, ip) <= zero) {
          t = -at(x, l) / (at(zz, ip) - at(x, l));
          if (alpha > t) {
            alpha = t;
            jj = ip;
          }
        }
      }

      // if all new constrained coefficients are feasible then alpha will still == 2.
      if (alpha == two) {
        break;
      } else {
        // use alpha which will be [0, 1]to interpolate between the old x and the new zz
        for (ip = Integer(1); ip <= nsetup; ip = ip + Integer(1)) {
          l = at(index, ip);
          at(x, l) = at(x, l) + alpha * (at(zz, ip) - at(x, l));
        }
        // modify a and b and the index arrays to move coefficient i
        // from set P to set Z
        i = at(index, jj);
        while(true) {
          at(x, i) = zero;

          if (jj != nsetup) {
            jj = jj + Integer(1);
            for (j = jj; j <= nsetup; j = j + Integer(1)) {
              ii = at(index, j);
              at(index, j - Integer(1)) = ii;
              G1(
                at(a, j - Integer(1), ii), at(a, j, ii),
                cc, si, at(a, j - Integer(1), ii)
              );
              at(a, j, ii) = zero;
              for (l = Integer(1); l <= n; l = l + Integer(1)) {
                if (l != ii) {
                  temp = at(a, j - Integer(1), l);
                  at(a, j - Integer(1), l) = cc * temp + si * at(a, j, l);
                  at(a, j, l) = -si*temp + cc * at(a, j, l);
                }
              }
              temp = at(b, j - Integer(1));
              at(b, j - Integer(1)) = cc * temp + si*at(b, j);
              at(b, j) = -si*temp + cc*at(b, j);
            }

          }

          npp1 = nsetup;
          nsetup = nsetup - Integer(1);
          iz1 = iz1 - Integer(1);
          at(index, iz1) = i;

          // check if the remaining coefficients in set p are feasible.
          bool need_retry = false;
          for (jj = Integer(1); jj <= nsetup; jj = jj + Integer(1)) {
            i = at(index, jj);
            if (at(x, i) <= zero) {
              need_retry = true;
              break;
            }
          }
          if (need_retry) continue;

          zz = b;
          lambda_400();
          break;
        }
        continue;
      }
    } // secundary loop

    for (ip = Integer(1); ip <= nsetup; ip = ip + Integer(1)) {
      i = at(index, ip);
      at(x, i) = at(zz, ip);
    }
  } // main loop
}

// DSL-facing wrapper: nnls_goto(A, b) -> x. Overloaded on the 11-arg engine
// above (distinguished by signature). Copies A/b first, same as etr::nnls
// in nnls.hpp -- the engine above mutates a/b in place (faithfully matching
// the Fortran), so without this the caller's A/b would get overwritten.
template<typename MA, typename MB>
requires (IsArray<Decayed<MA>> && IsArray<Decayed<MB>>)
inline auto nnls_goto(const MA& Ain, const MB& bin) {
  const auto& dim = Ain.get_dim();
  ass<"nnls_goto: A has to be a matrix">(dim.size() == 2);
  const std::size_t mrow = dim[0];
  const std::size_t ncol = dim[1];
  ass<"nnls_goto: b must have as many entries as A has rows">(bin.size() == mrow);

  std::vector<double> araw = matmul_extract_dvals(Ain);
  std::vector<double> braw = matmul_extract_dvals(bin);

  Array<Double, Buffer<Double>> a_copy = numeric(Integer(static_cast<int>(mrow * ncol)));
  a_copy.dim = std::vector<std::size_t>{mrow, ncol};
  for (std::size_t i = 0; i < araw.size(); ++i) a_copy.set(i, Double(araw[i]));

  Array<Double, Buffer<Double>> b_copy = numeric(Integer(static_cast<int>(mrow)));
  for (std::size_t i = 0; i < braw.size(); ++i) b_copy.set(i, Double(braw[i]));

  Array<Double, Buffer<Double>> x = numeric(Integer(static_cast<int>(ncol)));
  Array<Double, Buffer<Double>> w = numeric(Integer(static_cast<int>(ncol)));
  Array<Double, Buffer<Double>> zz = numeric(Integer(static_cast<int>(mrow)));
  Array<Integer, Buffer<Integer>> index;
  Double rnorm;
  Integer mode;

  nnls_goto(a_copy, Integer(static_cast<int>(mrow)), Integer(static_cast<int>(mrow)),
            Integer(static_cast<int>(ncol)), b_copy, x, rnorm, w, zz, index, mode);

  ass<"nnls_goto: bad dimensions">(mode != Integer(2));
  ass<"nnls_goto: iteration count exceeded">(mode != Integer(3));

  return x;
}

}

#endif // !NNLS_GOTO_ETR_HPP
