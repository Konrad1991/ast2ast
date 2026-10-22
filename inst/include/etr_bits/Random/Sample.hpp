#ifndef SAMPLE_ETR_HPP
#define SAMPLE_ETR_HPP

#include <climits>
#include <cmath>
#include <vector>
#include "MersenneTwister.hpp"

namespace etr {

namespace sample_internally {

// R's revsort (main/sort.c): heapsort into descending order, ib alongside.
// Ported literally: the order of ties decides which element is drawn.
inline void revsort(double* a, int* ib, int n) {
  if (n <= 1) return;
  a--; ib--;
  int l = (n >> 1) + 1;
  int ir = n;
  int i, j, ii;
  double ra;
  for (;;) {
    if (l > 1) {
      l = l - 1;
      ra = a[l];
      ii = ib[l];
    } else {
      ra = a[ir];
      ii = ib[ir];
      a[ir] = a[1];
      ib[ir] = ib[1];
      if (--ir == 1) {
        a[1] = ra;
        ib[1] = ii;
        return;
      }
    }
    i = l;
    j = l << 1;
    while (j <= ir) {
      if (j < ir && a[j] > a[j + 1]) ++j;
      if (ra > a[j]) {
        a[i] = a[j];
        ib[i] = ib[j];
        j += (i = j);
      } else {
        j = ir + 1;
      }
    }
    a[i] = ra;
    ib[i] = ii;
  }
}

inline void fixup_prob(std::vector<double>& p, int require_k, bool replace) {
  double sum = 0.0;
  int npos = 0;
  for (const double v : p) {
    ass<"sample: NA in probability vector">(std::isfinite(v));
    ass<"sample: negative probability">(v >= 0.0);
    if (v > 0.0) {
      npos++;
      sum += v;
    }
  }
  ass<"sample: too few positive probabilities">(!(npos == 0 || (!replace && require_k > npos)));
  for (double& v : p) v /= sum;
}

template <typename E>
inline void prob_sample_replace(E& engine, std::vector<double>& p, int nans, std::vector<int>& ans) {
  const int n = static_cast<int>(p.size());
  std::vector<int> perm(n);
  for (int i = 0; i < n; i++) perm[i] = i + 1;
  revsort(p.data(), perm.data(), n);
  for (int i = 1; i < n; i++) p[i] += p[i - 1];
  const int nm1 = n - 1;
  for (int i = 0; i < nans; i++) {
    const double rU = engine.runif();
    int j;
    for (j = 0; j < nm1; j++) {
      if (rU <= p[j]) break;
    }
    ans[i] = perm[j];
  }
}

// Walker's alias method (R's walker_ProbSampleReplace, sample.kind = "Rejection")
template <typename E>
inline void walker_prob_sample_replace(E& engine, const std::vector<double>& p, int nans, std::vector<int>& ans) {
  const int n = static_cast<int>(p.size());
  std::vector<int> HL(n);
  std::vector<int> a(n);
  std::vector<double> q(n);
  int* H = HL.data();
  int* L = HL.data() + n;
  for (int i = 0; i < n; i++) {
    q[i] = p[i] * n;
    if (q[i] < 1.) *H++ = i; else *--L = i;
  }
  if (H > HL.data() && L < HL.data() + n) {
    for (int k = 0; k < n - 1; k++) {
      const int i = HL[k];
      const int j = *L;
      a[i] = j;
      q[j] += q[i] - 1.;
      if (q[j] < 1.) L++;
      if (L >= HL.data() + n) break;
    }
  }
  for (int i = 0; i < n; i++) q[i] += i;
  for (int i = 0; i < nans; i++) {
    const int k = static_cast<int>(engine.unif_index(static_cast<std::size_t>(n)));
    const double rU = k + engine.runif();
    ans[i] = (rU < q[k]) ? k + 1 : a[k] + 1;
  }
}

template <typename E>
inline void prob_sample_no_replace(E& engine, std::vector<double>& p, int nans, std::vector<int>& ans) {
  const int n = static_cast<int>(p.size());
  std::vector<int> perm(n);
  for (int i = 0; i < n; i++) perm[i] = i + 1;
  revsort(p.data(), perm.data(), n);
  double totalmass = 1;
  int n1 = n - 1;
  for (int i = 0; i < nans; i++, n1--) {
    const double rT = totalmass * engine.runif();
    double mass = 0;
    int j;
    for (j = 0; j < n1; j++) {
      mass += p[j];
      if (rT <= mass) break;
    }
    ans[i] = perm[j];
    totalmass -= p[j];
    for (int k = j; k < n1; k++) {
      p[k] = p[k + 1];
      perm[k] = perm[k + 1];
    }
  }
}

// R's do_sample (main/random.c) without the long vector path; 1-based indices
template <typename E>
inline std::vector<int> sample_indices(E& engine, double dn, double size, bool replace,
                                       std::vector<double>* prob) {
  ass<"sample: invalid 'size' argument">(std::isfinite(size) && size >= 0);
  if (prob != nullptr) {
    ass<"sample: invalid first argument">(std::isfinite(dn) && dn >= 0 && dn <= INT_MAX);
    const int n = static_cast<int>(dn);
    ass<"sample: invalid 'size' argument">(size <= INT_MAX);
    const int k = static_cast<int>(size);
    ass<"sample: invalid first argument">(!(k > 0 && n == 0));
    ass<"sample: cannot take a sample larger than the population when 'replace = FALSE'">(replace || k <= n);
    ass<"sample: incorrect number of probabilities">(static_cast<int>(prob->size()) == n);
    fixup_prob(*prob, k, replace);
    std::vector<int> ans(k);
    if (replace) {
      int nc = 0;
      for (int i = 0; i < n; i++) if (n * (*prob)[i] > 0.1) nc++;
      if (nc > 200) {
        walker_prob_sample_replace(engine, *prob, k, ans);
      } else {
        prob_sample_replace(engine, *prob, k, ans);
      }
    } else {
      prob_sample_no_replace(engine, *prob, k, ans);
    }
    return ans;
  }
  ass<"sample: invalid first argument">(std::isfinite(dn) && dn >= 0 && dn <= 4.5e15);
  ass<"sample: n larger than 2147483647 is not supported">(dn <= INT_MAX && size <= INT_MAX);
  const int k = static_cast<int>(size);
  ass<"sample: invalid first argument">(!(k > 0 && dn == 0));
  ass<"sample: cannot take a sample larger than the population when 'replace = FALSE'">(replace || k <= dn);
  int n = static_cast<int>(dn);
  std::vector<int> ans(k);
  if (replace || k < 2) {
    for (int i = 0; i < k; i++) ans[i] = static_cast<int>(engine.unif_index(static_cast<std::size_t>(n)) + 1);
    return ans;
  }
  std::vector<int> x(n);
  for (int i = 0; i < n; i++) x[i] = i;
  for (int i = 0; i < k; i++) {
    const int j = static_cast<int>(engine.unif_index(static_cast<std::size_t>(n)));
    ans[i] = x[j] + 1;
    x[j] = x[--n];
  }
  return ans;
}

template <typename T> inline double scalar_as_double(const T& s) {
  const auto sv = get_scalar_val(s);
  if (sv.isNA()) return std::numeric_limits<double>::quiet_NaN();
  return static_cast<double>(get_val(sv));
}

template <typename T> inline double size_as_double(const T& s) {
  if constexpr (IsScalarLike<Decayed<T>>) {
    return scalar_as_double(s);
  } else {
    ass<"sample: invalid 'size' argument">(s.size() == 1);
    return scalar_as_double(s.get(0));
  }
}

template <typename R> inline bool replace_as_bool(const R& r) {
  const auto rv = get_scalar_val(r);
  ass<"sample: invalid 'replace' argument">(!rv.isNA());
  return static_cast<bool>(get_val(rv));
}

template <typename P> inline std::vector<double> prob_as_vector(const P& prob) {
  if constexpr (IsScalarLike<Decayed<P>>) {
    return std::vector<double>{scalar_as_double(prob)};
  } else {
    std::vector<double> p(prob.size());
    for (std::size_t i = 0; i < p.size(); i++) p[i] = scalar_as_double(prob.get(i));
    return p;
  }
}

// NOTE: a missing size / prob arrives as Logical (registry default NA);
// the type inference rejects logical values given by the user
template <typename T> inline constexpr bool IsMissingArgument = std::is_same_v<Decayed<T>, Logical>;

template <typename E, typename P>
inline std::vector<int> draw_indices(E& engine, double dn, double size, bool replace, const P& prob) {
  if constexpr (IsMissingArgument<P>) {
    return sample_indices(engine, dn, size, replace, nullptr);
  } else {
    std::vector<double> p = prob_as_vector(prob);
    return sample_indices(engine, dn, size, replace, &p);
  }
}

// value type: references such as DualRef have no default constructor
template <typename X> inline auto data_type_of() {
  if constexpr (IsScalarLike<Decayed<X>>) {
    return Decayed<decltype(get_scalar_val(std::declval<const Decayed<X>&>()))>{};
  } else {
    return typename ExtractDataType<Decayed<X>>::value_type{};
  }
}

template <typename Type> inline auto indices_as_vector(const std::vector<int>& idx) {
  auto res = createRVec<Type>(Integer(static_cast<int>(idx.size())));
  for (std::size_t i = 0; i < idx.size(); i++) res.set(i, Type(idx[i]));
  return res;
}

} // namespace sample_internally

// sample.int(n, size = n, replace, prob)
template <typename E, typename N, typename S, typename R, typename P>
inline auto sample_int(E&& engine, const N& n, const S& size, const R& replace, const P& prob) {
  const double dn = sample_internally::size_as_double(n);
  double k = dn;
  if constexpr (!sample_internally::IsMissingArgument<S>) k = sample_internally::size_as_double(size);
  const auto idx = sample_internally::draw_indices(engine, dn, k, sample_internally::replace_as_bool(replace), prob);
  return sample_internally::indices_as_vector<Integer>(idx);
}

// sample(x, size = length(x), replace, prob); a single number x >= 1 means 1:x like in R
template <typename E, typename X, typename S, typename R, typename P>
inline auto sample(E&& engine, const X& x, const S& size, const R& replace, const P& prob) {
  using DataType = decltype(sample_internally::data_type_of<X>());
  const bool rep = sample_internally::replace_as_bool(replace);
  constexpr bool numeric = !std::is_same_v<DataType, Logical>;
  double first = 0.0;
  std::size_t len = 1;
  if constexpr (IsScalarLike<Decayed<X>>) {
    first = sample_internally::scalar_as_double(x);
  } else {
    len = x.size();
    if (len == 1) first = sample_internally::scalar_as_double(x.get(0));
  }
  if (numeric && len == 1 && std::isfinite(first) && first >= 1) {
    double k = first;
    if constexpr (!sample_internally::IsMissingArgument<S>) k = sample_internally::size_as_double(size);
    const auto idx = sample_internally::draw_indices(engine, first, k, rep, prob);
    return sample_internally::indices_as_vector<DataType>(idx);
  }
  double k = static_cast<double>(len);
  if constexpr (!sample_internally::IsMissingArgument<S>) k = sample_internally::size_as_double(size);
  const auto idx = sample_internally::draw_indices(engine, static_cast<double>(len), k, rep, prob);
  auto res = createRVec<DataType>(Integer(static_cast<int>(idx.size())));
  for (std::size_t i = 0; i < idx.size(); i++) {
    if constexpr (IsScalarLike<Decayed<X>>) {
      res.set(i, x);
    } else {
      res.set(i, x.get(static_cast<std::size_t>(idx[i] - 1)));
    }
  }
  return res;
}

} // namespace etr

#endif
