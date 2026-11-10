#ifndef NUTS_ETR_HPP
#define NUTS_ETR_HPP

// nuts(log_post, theta0, n_warmup, n_samples, n_chains, seed, options[, data]):
// No-U-Turn sampler (Hoffman & Gelman 2014) with multinomial sampling, diagonal
// mass matrix and Stan's windowed warmup. Gradient via value_and_gradient.
// A solver failure (numeric_error) inside log_post counts as divergent.

#include <cmath>
#include <cstdint>
#include <limits>
#include <thread>
#include <vector>

namespace etr {

// slots must match predefined_known_types in R/TypeParser.R; defaults in the registry (nuts_options())
struct nuts_options {
  Integer max_treedepth;
  Double target_accept;
  Double step_size;
  Logical adapt_mass;
  Integer n_threads;
  nuts_options(Integer max_treedepth_, Double target_accept_, Double step_size_, Logical adapt_mass_,
               Integer n_threads_)
    : max_treedepth(max_treedepth_), target_accept(target_accept_), step_size(step_size_),
      adapt_mass(adapt_mass_), n_threads(n_threads_) {}
#ifndef STANDALONE_ETR
  explicit nuts_options(SEXP arg) :
    max_treedepth(SEXP2Scalar<Integer>(checked_elt(arg, "max_treedepth", 5, "nuts_options"))),
    target_accept(SEXP2Scalar<Double>(checked_elt(arg, "target_accept", 5, "nuts_options"))),
    step_size(SEXP2Scalar<Double>(checked_elt(arg, "step_size", 5, "nuts_options"))),
    adapt_mass(SEXP2Scalar<Logical>(checked_elt(arg, "adapt_mass", 5, "nuts_options"))),
    n_threads(SEXP2Scalar<Integer>(checked_elt(arg, "n_threads", 5, "nuts_options"))) {}
  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 5));
    SET_VECTOR_ELT(res, 0, Cast(max_treedepth));
    SET_VECTOR_ELT(res, 1, Cast(target_accept));
    SET_VECTOR_ELT(res, 2, Cast(step_size));
    SET_VECTOR_ELT(res, 3, Cast(adapt_mass));
    SET_VECTOR_ELT(res, 4, Cast(n_threads));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 5));
    SET_STRING_ELT(names, 0, Rf_mkChar("max_treedepth"));
    SET_STRING_ELT(names, 1, Rf_mkChar("target_accept"));
    SET_STRING_ELT(names, 2, Rf_mkChar("step_size"));
    SET_STRING_ELT(names, 3, Rf_mkChar("adapt_mass"));
    SET_STRING_ELT(names, 4, Rf_mkChar("n_threads"));
    Rf_setAttrib(res, R_NamesSymbol, names);
    Rf_setAttrib(res, R_ClassSymbol, Rf_mkString("nuts_options"));
    UNPROTECT(2);
    return res;
  }
#endif // !STANDALONE_ETR
  nuts_options() = default;
  nuts_options(const nuts_options&) = default;
  nuts_options(nuts_options&&) noexcept = default;
  nuts_options& operator=(const nuts_options&) = default;
  nuts_options& operator=(nuts_options&&) noexcept = default;
};

inline void print(const nuts_options& x) {
  PRINT_STREAM << "nuts_options" << std::endl;
  PRINT_STREAM << "  max_treedepth: " << x.max_treedepth << std::endl;
  PRINT_STREAM << "  target_accept: " << x.target_accept << std::endl;
  PRINT_STREAM << "  step_size: " << x.step_size << std::endl;
  PRINT_STREAM << "  adapt_mass: " << x.adapt_mass << std::endl;
  PRINT_STREAM << "  n_threads: " << x.n_threads << std::endl;
}

// draws: n_samples x n_chains x n_par (posterior::as_draws_array layout);
// per-iteration diagnostics: n_samples x n_chains; step_size: n_chains; inv_metric: n_par x n_chains
struct nuts_result {
  Array<Double, Buffer<Double>> draws;
  Array<Double, Buffer<Double>> lp;
  Array<Double, Buffer<Double>> accept_stat;
  Array<Integer, Buffer<Integer>> treedepth;
  Array<Integer, Buffer<Integer>> n_leapfrog;
  Array<Logical, Buffer<Logical>> divergent;
  Array<Double, Buffer<Double>> step_size;
  Array<Double, Buffer<Double>> inv_metric;
#ifndef STANDALONE_ETR
  explicit nuts_result(SEXP arg) :
    draws(checked_elt(arg, "draws", 8, "nuts_result")),
    lp(checked_elt(arg, "lp", 8, "nuts_result")),
    accept_stat(checked_elt(arg, "accept_stat", 8, "nuts_result")),
    treedepth(checked_elt(arg, "treedepth", 8, "nuts_result")),
    n_leapfrog(checked_elt(arg, "n_leapfrog", 8, "nuts_result")),
    divergent(checked_elt(arg, "divergent", 8, "nuts_result")),
    step_size(checked_elt(arg, "step_size", 8, "nuts_result")),
    inv_metric(checked_elt(arg, "inv_metric", 8, "nuts_result")) {}
  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 8));
    SET_VECTOR_ELT(res, 0, Cast(draws));
    SET_VECTOR_ELT(res, 1, Cast(lp));
    SET_VECTOR_ELT(res, 2, Cast(accept_stat));
    SET_VECTOR_ELT(res, 3, Cast(treedepth));
    SET_VECTOR_ELT(res, 4, Cast(n_leapfrog));
    SET_VECTOR_ELT(res, 5, Cast(divergent));
    SET_VECTOR_ELT(res, 6, Cast(step_size));
    SET_VECTOR_ELT(res, 7, Cast(inv_metric));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 8));
    SET_STRING_ELT(names, 0, Rf_mkChar("draws"));
    SET_STRING_ELT(names, 1, Rf_mkChar("lp"));
    SET_STRING_ELT(names, 2, Rf_mkChar("accept_stat"));
    SET_STRING_ELT(names, 3, Rf_mkChar("treedepth"));
    SET_STRING_ELT(names, 4, Rf_mkChar("n_leapfrog"));
    SET_STRING_ELT(names, 5, Rf_mkChar("divergent"));
    SET_STRING_ELT(names, 6, Rf_mkChar("step_size"));
    SET_STRING_ELT(names, 7, Rf_mkChar("inv_metric"));
    Rf_setAttrib(res, R_NamesSymbol, names);
    Rf_setAttrib(res, R_ClassSymbol, Rf_mkString("nuts_result"));
    UNPROTECT(2);
    return res;
  }
#endif // !STANDALONE_ETR
  nuts_result() = default;
  nuts_result(const nuts_result&) = default;
  nuts_result(nuts_result&&) noexcept = default;
  nuts_result& operator=(const nuts_result&) = default;
  nuts_result& operator=(nuts_result&&) noexcept = default;
};

inline void print(const nuts_result& x) {
  const Dim& d = x.draws.dim;
  PRINT_STREAM << "nuts_result" << std::endl;
  if (d.size() == 3) {
    PRINT_STREAM << "  draws: " << d[0] << " samples x " << d[1] << " chains x " << d[2]
                 << " parameters" << std::endl;
  }
  std::size_t n_div = 0;
  for (std::size_t i = 0; i < x.divergent.size(); i++) {
    if (get_val(x.divergent.get(i))) n_div++;
  }
  PRINT_STREAM << "  divergent transitions: " << n_div << std::endl;
}

namespace nuts_detail {

using vec = std::vector<double>;
constexpr double inf = std::numeric_limits<double>::infinity();

struct Tree {
  vec theta_minus, r_minus, grad_minus;
  vec theta_plus, r_plus, grad_plus;
  vec theta_prop, grad_prop;
  double lp_prop = 0.0;
  double log_w = 0.0;
  double sum_accept = 0.0;
  int n_leapfrog = 0;
  bool done = false;
  bool divergent = false;
};

inline double log_sum_exp(double a, double b) {
  if (a == -inf) return b;
  if (b == -inf) return a;
  const double m = a > b ? a : b;
  return m + std::log(std::exp(a - m) + std::exp(b - m));
}

// Welford, per coordinate
struct VarianceEstimator {
  std::size_t n = 0;
  vec mean, m2;
  explicit VarianceEstimator(std::size_t dim) : mean(dim, 0.0), m2(dim, 0.0) {}
  void restart() {
    n = 0;
    std::fill(mean.begin(), mean.end(), 0.0);
    std::fill(m2.begin(), m2.end(), 0.0);
  }
  void add(const vec& x) {
    n++;
    for (std::size_t j = 0; j < x.size(); j++) {
      const double delta = x[j] - mean[j];
      mean[j] += delta / static_cast<double>(n);
      m2[j] += delta * (x[j] - mean[j]);
    }
  }
};

// Stan's windowed adaptation: init buffer, doubling slow windows, term buffer
struct Windows {
  int n_warmup = 0;
  int init_buffer = 75;
  int term_buffer = 50;
  int window_size = 25;
  int next_window_end = 0;
  int counter = 0;
  bool active = true;
  explicit Windows(int n) : n_warmup(n) {
    if (n < 20) {
      active = false;
      return;
    }
    if (init_buffer + term_buffer + window_size > n) {
      init_buffer = static_cast<int>(0.15 * n);
      term_buffer = static_cast<int>(0.1 * n);
      window_size = n - (init_buffer + term_buffer);
    }
    next_window_end = init_buffer + window_size - 1;
  }
  bool in_window() const {
    return counter >= init_buffer && counter < n_warmup - term_buffer && counter != n_warmup;
  }
  bool window_ends() const { return counter == next_window_end && counter != n_warmup; }
  void compute_next_window() {
    const int last = n_warmup - term_buffer - 1;
    if (next_window_end == last) return;
    window_size *= 2;
    next_window_end = counter + window_size;
    if (next_window_end != last && next_window_end + 2 * window_size >= n_warmup - term_buffer) {
      next_window_end = last;
    }
  }
};

// eval(theta, lp, grad): false -> solver failure or non-finite value/gradient
template <typename Eval>
struct Chain {
  Eval& eval;
  std::size_t n;
  int max_depth;
  vec m_inv;
  normal momentum;
  uniform unif;
  double step = 1.0;

  Chain(Eval& eval_, std::size_t n_, int max_depth_, std::uint32_t seed)
    : eval(eval_), n(n_), max_depth(max_depth_), m_inv(n_, 1.0),
      // own engines: two distributions on one engine would share a stream
      momentum(mersenne_twister(static_cast<int>(seed)), Double(0.0), Double(1.0)),
      unif(mersenne_twister(static_cast<int>(seed + 1u)), Double(0.0), Double(1.0)) {}

  double kinetic(const vec& r) const {
    double k = 0.0;
    for (std::size_t j = 0; j < n; j++) k += m_inv[j] * r[j] * r[j];
    return 0.5 * k;
  }

  void draw_momentum(vec& r) {
    r.resize(n);
    for (std::size_t j = 0; j < n; j++) r[j] = momentum.draw_one() / std::sqrt(m_inv[j]);
  }

  // returns the energy after one step; inf on failure
  double leapfrog(const vec& theta, const vec& r, const vec& grad, double eps,
                  vec& th_out, vec& r_out, vec& g_out, double& lp_out) {
    r_out.resize(n);
    th_out.resize(n);
    for (std::size_t j = 0; j < n; j++) r_out[j] = r[j] + 0.5 * eps * grad[j];
    for (std::size_t j = 0; j < n; j++) th_out[j] = theta[j] + eps * m_inv[j] * r_out[j];
    if (!eval(th_out, lp_out, g_out)) return inf;
    for (std::size_t j = 0; j < n; j++) r_out[j] += 0.5 * eps * g_out[j];
    return -lp_out + kinetic(r_out);
  }

  bool u_turn(const Tree& t) const {
    double a = 0.0;
    double b = 0.0;
    for (std::size_t j = 0; j < n; j++) {
      const double d = t.theta_plus[j] - t.theta_minus[j];
      a += d * m_inv[j] * t.r_minus[j];
      b += d * m_inv[j] * t.r_plus[j];
    }
    return a < 0.0 || b < 0.0;
  }

  // subtree of 2^depth leapfrog steps; log_w relative to the start energy H0
  Tree build_tree(const vec& theta, const vec& r, const vec& grad, double direction, int depth,
                  double H0) {
    if (depth == 0) {
      Tree t;
      t.n_leapfrog = 1;
      vec th, rn, g;
      double lp = 0.0;
      const double H = leapfrog(theta, r, grad, direction * step, th, rn, g, lp);
      // NaN energy fails the comparison too
      if (!(H - H0 < 1000.0)) {
        t.done = true;
        t.divergent = true;
        t.log_w = -inf;
        return t;
      }
      t.log_w = H0 - H;
      t.sum_accept = H0 - H < 0.0 ? std::exp(H0 - H) : 1.0;
      t.lp_prop = lp;
      t.theta_minus = th;
      t.theta_plus = th;
      t.theta_prop = std::move(th);
      t.r_minus = rn;
      t.r_plus = std::move(rn);
      t.grad_minus = g;
      t.grad_plus = g;
      t.grad_prop = std::move(g);
      return t;
    }
    Tree t = build_tree(theta, r, grad, direction, depth - 1, H0);
    if (t.done) return t;
    const bool fwd = direction > 0.0;
    Tree right = build_tree(fwd ? t.theta_plus : t.theta_minus, fwd ? t.r_plus : t.r_minus,
                            fwd ? t.grad_plus : t.grad_minus, direction, depth - 1, H0);
    t.n_leapfrog += right.n_leapfrog;
    t.sum_accept += right.sum_accept;
    if (right.done) {
      t.done = true;
      t.divergent = right.divergent;
      return t;
    }
    const double log_w = log_sum_exp(t.log_w, right.log_w);
    // uniform multinomial inside the subtree
    if (std::log(unif.draw_one()) < right.log_w - log_w) {
      t.theta_prop = std::move(right.theta_prop);
      t.grad_prop = std::move(right.grad_prop);
      t.lp_prop = right.lp_prop;
    }
    t.log_w = log_w;
    if (fwd) {
      t.theta_plus = std::move(right.theta_plus);
      t.r_plus = std::move(right.r_plus);
      t.grad_plus = std::move(right.grad_plus);
    } else {
      t.theta_minus = std::move(right.theta_minus);
      t.r_minus = std::move(right.r_minus);
      t.grad_minus = std::move(right.grad_minus);
    }
    t.done = u_turn(t);
    return t;
  }

  struct Stats {
    int depth = 0;
    int n_leapfrog = 0;
    bool divergent = false;
    double accept_stat = 0.0;
  };

  Stats transition(vec& theta, double& lp, vec& grad) {
    vec r;
    draw_momentum(r);
    const double H0 = -lp + kinetic(r);
    Tree tree;
    tree.theta_minus = theta;
    tree.theta_plus = theta;
    tree.theta_prop = theta;
    tree.r_minus = r;
    tree.r_plus = r;
    tree.grad_minus = grad;
    tree.grad_plus = grad;
    tree.grad_prop = grad;
    tree.lp_prop = lp;
    tree.log_w = 0.0;
    Stats s;
    double sum_accept = 0.0;
    while (s.depth < max_depth) {
      const double direction = unif.draw_one() < 0.5 ? -1.0 : 1.0;
      const bool fwd = direction > 0.0;
      Tree sub = build_tree(fwd ? tree.theta_plus : tree.theta_minus, fwd ? tree.r_plus : tree.r_minus,
                            fwd ? tree.grad_plus : tree.grad_minus, direction, s.depth, H0);
      s.n_leapfrog += sub.n_leapfrog;
      sum_accept += sub.sum_accept;
      s.depth++;
      if (sub.done) {
        s.divergent = sub.divergent;
        break;
      }
      // biased progressive sampling: favours the new subtree
      if (std::log(unif.draw_one()) < sub.log_w - tree.log_w) {
        tree.theta_prop = std::move(sub.theta_prop);
        tree.grad_prop = std::move(sub.grad_prop);
        tree.lp_prop = sub.lp_prop;
      }
      tree.log_w = log_sum_exp(tree.log_w, sub.log_w);
      if (fwd) {
        tree.theta_plus = std::move(sub.theta_plus);
        tree.r_plus = std::move(sub.r_plus);
        tree.grad_plus = std::move(sub.grad_plus);
      } else {
        tree.theta_minus = std::move(sub.theta_minus);
        tree.r_minus = std::move(sub.r_minus);
        tree.grad_minus = std::move(sub.grad_minus);
      }
      if (u_turn(tree)) break;
    }
    theta = std::move(tree.theta_prop);
    grad = std::move(tree.grad_prop);
    lp = tree.lp_prop;
    s.accept_stat = sum_accept / static_cast<double>(s.n_leapfrog);
    return s;
  }

  // Stan's init_stepsize: double / halve until one leapfrog step crosses acceptance 0.8
  void init_step(const vec& theta, double lp, const vec& grad) {
    const double log_target = std::log(0.8);
    vec r, th, rn, g;
    double lp_new = 0.0;
    draw_momentum(r);
    double H0 = -lp + kinetic(r);
    double dH = H0 - leapfrog(theta, r, grad, step, th, rn, g, lp_new);
    const int direction = dH > log_target ? 1 : -1;
    while (true) {
      draw_momentum(r);
      H0 = -lp + kinetic(r);
      dH = H0 - leapfrog(theta, r, grad, step, th, rn, g, lp_new);
      if (direction == 1 && !(dH > log_target)) break;
      if (direction == -1 && !(dH < log_target)) break;
      step = direction == 1 ? 2.0 * step : 0.5 * step;
      ass<"nuts: step size grows without bound; is the posterior improper?">(step < 1e7);
      ass<"nuts: no acceptably small step size; log_post fails close to theta">(step > 1e-300);
    }
  }
};

// dual averaging (Hoffman & Gelman 2014, Alg. 5)
struct StepAdaptation {
  double delta;
  double gamma = 0.05;
  double t0 = 10.0;
  double kappa = 0.75;
  double mu = 0.0;
  double s_bar = 0.0;
  double x_bar = 0.0;
  double counter = 0.0;
  explicit StepAdaptation(double delta_) : delta(delta_) {}
  void restart(double step) {
    mu = std::log(10.0 * step);
    s_bar = 0.0;
    x_bar = 0.0;
    counter = 0.0;
  }
  double learn(double accept_stat) {
    counter += 1.0;
    if (accept_stat > 1.0) accept_stat = 1.0;
    const double eta = 1.0 / (counter + t0);
    s_bar = (1.0 - eta) * s_bar + eta * (delta - accept_stat);
    const double x = mu - s_bar * std::sqrt(counter) / gamma;
    const double w = std::pow(counter, -kappa);
    x_bar = w * x + (1.0 - w) * x_bar;
    return std::exp(x);
  }
};

struct Settings {
  std::size_t n_par;
  int n_warmup;
  int n_samples;
  std::size_t n_chains;
  int max_depth;
  double target_accept;
  double step0;
  bool adapt_mass;
};

template <typename Eval>
inline void run_chain(Eval& eval, const vec& theta0, std::size_t chain, std::uint32_t seed,
                      const Settings& st, nuts_result& res) {
  Chain<Eval> ch(eval, st.n_par, st.max_depth, seed);
  ch.step = st.step0;
  vec theta = theta0;
  vec grad;
  double lp = 0.0;
  ass<"nuts: log_post or its gradient is not finite at theta0">(eval(theta, lp, grad));
  // without warmup the given step size is used as is
  if (st.n_warmup > 0) ch.init_step(theta, lp, grad);
  StepAdaptation adapt(st.target_accept);
  adapt.restart(ch.step);
  Windows windows(st.n_warmup);
  VarianceEstimator var(st.n_par);
  const std::size_t ns = static_cast<std::size_t>(st.n_samples);
  const std::size_t nc = st.n_chains;

  for (int it = 0; it < st.n_warmup + st.n_samples; it++) {
    const auto s = ch.transition(theta, lp, grad);
    if (it < st.n_warmup) {
      ch.step = adapt.learn(s.accept_stat);
      if (st.adapt_mass && windows.active) {
        if (windows.in_window()) var.add(theta);
        if (windows.window_ends()) {
          windows.compute_next_window();
          const double n = static_cast<double>(var.n);
          // regularised towards 1e-3 like Stan
          for (std::size_t j = 0; j < st.n_par; j++) {
            const double v = var.m2[j] / (n - 1.0);
            ch.m_inv[j] = (n / (n + 5.0)) * v + 1e-3 * (5.0 / (n + 5.0));
          }
          var.restart();
          ch.init_step(theta, lp, grad);
          adapt.restart(ch.step);
        }
        windows.counter++;
      }
      if (it == st.n_warmup - 1) ch.step = std::exp(adapt.x_bar);
      continue;
    }
    const std::size_t i = static_cast<std::size_t>(it - st.n_warmup);
    const std::size_t ic = i + ns * chain;
    for (std::size_t j = 0; j < st.n_par; j++) res.draws.set(ic + ns * nc * j, Double(theta[j]));
    res.lp.set(ic, Double(lp));
    res.accept_stat.set(ic, Double(s.accept_stat));
    res.treedepth.set(ic, Integer(s.depth));
    res.n_leapfrog.set(ic, Integer(s.n_leapfrog));
    res.divergent.set(ic, Logical(s.divergent));
  }
  res.step_size.set(chain, Double(ch.step));
  for (std::size_t j = 0; j < st.n_par; j++) res.inv_metric.set(j + st.n_par * chain, Double(ch.m_inv[j]));
}

template <typename T>
inline double scalar_value(const T& x) {
  return static_cast<double>(get_val(x));
}

// VG: x -> value_and_gradient_result
template <typename VG>
struct Evaluator {
  const VG& vg;
  Array<Double, Buffer<Double>> x;
  bool operator()(const vec& theta, double& lp, vec& grad) {
    const std::size_t n = theta.size();
    if (x.size() != n) {
      x = Array<Double, Buffer<Double>>(SI{n});
      x.dim = Dim{n};
    }
    for (std::size_t j = 0; j < n; j++) x.set(j, Double(theta[j]));
    try {
      const value_and_gradient_result r = vg(x);
      lp = get_val(r.value);
      if (!std::isfinite(lp)) return false;
      grad.resize(n);
      for (std::size_t j = 0; j < n; j++) {
        grad[j] = get_val(r.gradient.get(j));
        if (!std::isfinite(grad[j])) return false;
      }
    } catch (const numeric_error&) {
      return false;
    }
    return true;
  }
};

} // namespace nuts_detail

template <typename VG, typename X, typename W, typename S, typename C, typename Seed>
inline nuts_result nuts_impl(const VG& vg, const X& theta0, const W& n_warmup, const S& n_samples,
                             const C& n_chains, const Seed& seed, const nuts_options& opt) {
  using namespace nuts_detail;
  const std::size_t n_par = theta0.size();
  ass<"nuts: theta0 has to contain at least one value">(n_par > 0);
  vec theta_start(n_par);
  for (std::size_t j = 0; j < n_par; j++) {
    theta_start[j] = static_cast<double>(get_val(theta0.get(j)));
    ass<"nuts: theta0 has to be finite">(std::isfinite(theta_start[j]));
  }
  const double warm = scalar_value(n_warmup);
  const double samples = scalar_value(n_samples);
  const double chains = scalar_value(n_chains);
  const double seed_v = scalar_value(seed);
  ass<"nuts: n_warmup has to be a whole number >= 0">(std::isfinite(warm) && warm >= 0 && warm == std::floor(warm));
  ass<"nuts: n_samples has to be a whole number >= 1">(std::isfinite(samples) && samples >= 1 && samples == std::floor(samples));
  ass<"nuts: n_chains has to be a whole number >= 1">(std::isfinite(chains) && chains >= 1 && chains == std::floor(chains));
  ass<"nuts: seed has to be a finite number">(std::isfinite(seed_v));
  ass<"nuts_options: max_treedepth has to be between 1 and 30">(
    !opt.max_treedepth.isNA() && get_val(opt.max_treedepth) >= 1 && get_val(opt.max_treedepth) <= 30);
  ass<"nuts_options: target_accept has to be in (0, 1)">(
    !opt.target_accept.isNA() && get_val(opt.target_accept) > 0.0 && get_val(opt.target_accept) < 1.0);
  ass<"nuts_options: step_size has to be a positive finite number">(
    !opt.step_size.isNA() && std::isfinite(get_val(opt.step_size)) && get_val(opt.step_size) > 0.0);
  ass<"nuts_options: adapt_mass has to be TRUE or FALSE">(!opt.adapt_mass.isNA());
  ass<"nuts_options: n_threads has to be >= 1">(!opt.n_threads.isNA() && get_val(opt.n_threads) >= 1);

  Settings st;
  st.n_par = n_par;
  st.n_warmup = static_cast<int>(warm);
  st.n_samples = static_cast<int>(samples);
  st.n_chains = static_cast<std::size_t>(chains);
  st.max_depth = get_val(opt.max_treedepth);
  st.target_accept = get_val(opt.target_accept);
  st.step0 = get_val(opt.step_size);
  st.adapt_mass = get_val(opt.adapt_mass);

  const std::size_t ns = static_cast<std::size_t>(st.n_samples);
  const std::size_t nc = st.n_chains;
  nuts_result res;
  res.draws = Array<Double, Buffer<Double>>(SI{ns * nc * n_par});
  res.draws.dim = Dim{ns, nc, n_par};
  res.lp = Array<Double, Buffer<Double>>(SI{ns * nc});
  res.lp.dim = Dim{ns, nc};
  res.accept_stat = Array<Double, Buffer<Double>>(SI{ns * nc});
  res.accept_stat.dim = Dim{ns, nc};
  res.treedepth = Array<Integer, Buffer<Integer>>(SI{ns * nc});
  res.treedepth.dim = Dim{ns, nc};
  res.n_leapfrog = Array<Integer, Buffer<Integer>>(SI{ns * nc});
  res.n_leapfrog.dim = Dim{ns, nc};
  res.divergent = Array<Logical, Buffer<Logical>>(SI{ns * nc});
  res.divergent.dim = Dim{ns, nc};
  res.step_size = Array<Double, Buffer<Double>>(SI{nc});
  res.step_size.dim = Dim{nc};
  res.inv_metric = Array<Double, Buffer<Double>>(SI{n_par * nc});
  res.inv_metric.dim = Dim{n_par, nc};

  // unsigned: wraps instead of overflowing for seeds close to INT_MAX
  const std::uint32_t seed_base = static_cast<std::uint32_t>(static_cast<std::int64_t>(seed_v));
  auto run = [&](std::size_t c) {
    Evaluator<VG> eval{vg, Array<Double, Buffer<Double>>()};
    run_chain(eval, theta_start, c, seed_base + 2u * static_cast<std::uint32_t>(c), st, res);
  };

  const std::size_t hw = std::max<std::size_t>(std::thread::hardware_concurrency(), 1);
  std::size_t nthr = static_cast<std::size_t>(get_val(opt.n_threads));
  if (nthr > hw) nthr = hw;
  if (nthr > nc) nthr = nc;
#ifdef STANDALONE_ETR
  nthr = 1;
#endif
  if (nthr <= 1) {
    for (std::size_t c = 0; c < nc; c++) run(c);
    return res;
  }
#ifndef STANDALONE_ETR
  std::vector<std::vector<std::string>> warns(nc);
  RcppThread::ThreadPool pool(nthr);
  pmap_run(pool, [&] {
    pool.parallelFor(0, static_cast<int>(nc), [&](auto c_) {
      const std::size_t c = static_cast<std::size_t>(c_);
      run_pmap_task(warns[c], [&] { run(c); });
    });
  });
  flush_warnings(warns);
#endif
  return res;
}

template <typename F, typename X, typename W, typename S, typename C, typename Seed>
inline nuts_result nuts(const F& f, const X& theta0, const W& n_warmup, const S& n_samples,
                        const C& n_chains, const Seed& seed, const nuts_options& opt) {
  auto vg = [&f](const Array<Double, Buffer<Double>>& x) { return value_and_gradient(f, x); };
  return nuts_impl(vg, theta0, n_warmup, n_samples, n_chains, seed, opt);
}

template <typename F, typename X, typename W, typename S, typename C, typename Seed, typename E>
inline nuts_result nuts(const F& f, const X& theta0, const W& n_warmup, const S& n_samples,
                        const C& n_chains, const Seed& seed, const nuts_options& opt, const E& data) {
  auto vg = [&f, &data](const Array<Double, Buffer<Double>>& x) { return value_and_gradient(f, x, data); };
  return nuts_impl(vg, theta0, n_warmup, n_samples, n_chains, seed, opt);
}

} // namespace etr

#endif
