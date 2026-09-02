# Generates src/tests_ADArrayOps.cpp
#
# Covers the AD gap that let a forward-mode bug hide: arithmetic (+ - / ^) and
# unary math on *arrays*, in both forward (Dual) and reverse (ReverseDouble)
# mode. All seeds are non-unit so a "dropped / hard-coded incoming dot" bug
# fails the assertion. Expected values are computed here, never hand-derived.

fmt <- function(v) paste(sprintf("%.12g", v), collapse = ", ")
vec <- function(v) sprintf("{%s}", fmt(v))

# --- data -------------------------------------------------------------------
a  <- c(0.6, 1.3, 2.1, 0.9, 1.7, 2.8)   # left operand,  all > 0 (needed for ^, /)
b  <- c(1.1, 0.7, 1.9, 2.4, 0.5, 1.5)   # right operand, all > 0
c0 <- 1.3                                # broadcast scalar
sa <- 2.0                                # seed on the left  variable
sb <- 3.0                                # seed on the right variable
su <- 2.0                                # seed for unary tests

u   <- c(0.3, 0.7, 1.1, 1.9, 2.3, 3.1)  # unary arg, in-domain for log/sqrt
ur  <- c(-0.8, -0.4, -0.1, 0.2, 0.5, 0.85) # unary arg for asin/acos (|x| < 1)
n   <- length(a)

# --- analytic derivatives -------------------------------------------------
bin_val <- list(
  "+" = function(a, b) a + b,
  "-" = function(a, b) a - b,
  "/" = function(a, b) a / b,
  "^" = function(a, b) a^b
)
# d/da and d/db of a <op> b
bin_da <- list(
  "+" = function(a, b) rep(1, length(a)),
  "-" = function(a, b) rep(1, length(a)),
  "/" = function(a, b) 1 / b,
  "^" = function(a, b) b * a^(b - 1)
)
bin_db <- list(
  "+" = function(a, b) rep(1, length(a)),
  "-" = function(a, b) rep(-1, length(a)),
  "/" = function(a, b) -a / (b * b),
  "^" = function(a, b) a^b * log(a)
)
op_name <- c("+" = "plus", "-" = "minus", "/" = "div", "^" = "pow")
# C++ form: arrays use operator +-*/ but ^ is the free function power(a, b)
cxx <- function(op, l, r) {
  if (op == "^") sprintf("power(%s, %s)", l, r) else sprintf("%s %s %s", l, op, r)
}

unary <- list(
  sin  = list(f = sin,  d = function(x) cos(x),               arg = "u"),
  cos  = list(f = cos,  d = function(x) -sin(x),              arg = "u"),
  tan  = list(f = tan,  d = function(x) 1 / cos(x)^2,         arg = "u"),
  asin = list(f = asin, d = function(x) 1 / sqrt(1 - x^2),    arg = "ur"),
  acos = list(f = acos, d = function(x) -1 / sqrt(1 - x^2),   arg = "ur"),
  atan = list(f = atan, d = function(x) 1 / (1 + x^2),        arg = "u"),
  sinh = list(f = sinh, d = function(x) cosh(x),              arg = "u"),
  cosh = list(f = cosh, d = function(x) sinh(x),              arg = "u"),
  tanh = list(f = tanh, d = function(x) 1 - tanh(x)^2,        arg = "u"),
  exp  = list(f = exp,  d = function(x) exp(x),               arg = "u"),
  log  = list(f = log,  d = function(x) 1 / x,                arg = "u"),
  sqrt = list(f = sqrt, d = function(x) 0.5 / sqrt(x),        arg = "u")
)

L <- character(0)
add <- function(...) L <<- c(L, sprintf(...))

# --- forward-mode binary -------------------------------------------------
add("  // ===== forward mode: + - / ^ on arrays (seed dl=%g, dr=%g) =====", sa, sb)
for (op in names(bin_val)) {
  val <- bin_val[[op]](a, b)
  # both operands are variables
  dot_both <- bin_da[[op]](a, b) * sa + bin_db[[op]](a, b) * sb
  add('  { auto x = mk_dual(%s, dots(%g)); auto y = mk_dual(%s, dots(%g));',
      vec(a), sa, vec(b), sb)
  add('    ass<"fwd %s: var %s var">(chk_fwd(%s, %s, %s)); }',
      op_name[op], op_name[op], cxx(op, "x", "y"), vec(val), vec(dot_both))
  # left variable, right constant (Dual (x) Double promotion)
  dot_l <- bin_da[[op]](a, b) * sa
  add('  { auto x = mk_dual(%s, dots(%g)); auto y = mk_dbl(%s);', vec(a), sa, vec(b))
  add('    ass<"fwd %s: var %s const">(chk_fwd(%s, %s, %s)); }',
      op_name[op], op_name[op], cxx(op, "x", "y"), vec(val), vec(dot_l))
  # left constant, right variable
  dot_r <- bin_db[[op]](a, b) * sb
  add('  { auto x = mk_dbl(%s); auto y = mk_dual(%s, dots(%g));', vec(a), vec(b), sb)
  add('    ass<"fwd %s: const %s var">(chk_fwd(%s, %s, %s)); }',
      op_name[op], op_name[op], cxx(op, "x", "y"), vec(val), vec(dot_r))
  # left variable, right broadcast scalar
  val_s  <- bin_val[[op]](a, c0)
  dot_s  <- bin_da[[op]](a, rep(c0, n)) * sa
  add('  { auto x = mk_dual(%s, dots(%g)); Dual s(%g, 0.0);', vec(a), sa, c0)
  add('    ass<"fwd %s: var %s scalar">(chk_fwd(%s, %s, %s)); }',
      op_name[op], op_name[op], cxx(op, "x", "s"), vec(val_s), vec(dot_s))
}

# --- forward-mode unary -------------------------------------------------
add("")
add("  // ===== forward mode: unary math on arrays (seed du=%g) =====", su)
for (nm in names(unary)) {
  spec <- unary[[nm]]
  x <- get(spec$arg)
  add('  { auto x = mk_dual(%s, dots(%g));', vec(x), su)
  add('    ass<"fwd %s">(chk_fwd(%s(x), %s, %s)); }',
      nm, nm, vec(spec$f(x)), vec(spec$d(x) * su))
}

# --- reverse-mode binary ----------------------------------------------
add("")
add("  // ===== reverse mode: + - / ^ on arrays =====")
for (op in names(bin_val)) {
  val <- bin_val[[op]](a, b)
  ga  <- bin_da[[op]](a, b)
  gb  <- bin_db[[op]](a, b)
  # both variables
  add('  { TAPE_INTERN.clear(); auto x = mk_rev(%s); auto y = mk_rev(%s); auto e = %s;',
      vec(a), vec(b), cxx(op, "x", "y"))
  add('    ass<"rev %s: var %s var">(chk_rev2(x, y, e, %s, %s, %s)); }',
      op_name[op], op_name[op], vec(val), vec(ga), vec(gb))
  # left variable only
  add('  { TAPE_INTERN.clear(); auto x = mk_rev(%s); auto y = mk_dbl(%s); auto e = %s;',
      vec(a), vec(b), cxx(op, "x", "y"))
  add('    ass<"rev %s: var %s const">(chk_rev1(x, e, %s, %s)); }',
      op_name[op], op_name[op], vec(val), vec(ga))
  # right variable only
  add('  { TAPE_INTERN.clear(); auto x = mk_dbl(%s); auto y = mk_rev(%s); auto e = %s;',
      vec(a), vec(b), cxx(op, "x", "y"))
  add('    ass<"rev %s: const %s var">(chk_rev1(y, e, %s, %s)); }',
      op_name[op], op_name[op], vec(val), vec(gb))
}

# --- reverse-mode unary ---------------------------------------------
add("")
add("  // ===== reverse mode: unary math on arrays =====")
for (nm in names(unary)) {
  spec <- unary[[nm]]
  x <- get(spec$arg)
  add('  { TAPE_INTERN.clear(); auto x = mk_rev(%s); auto e = %s(x);', vec(x), nm)
  add('    ass<"rev %s">(chk_rev1(x, e, %s, %s)); }', nm, vec(spec$f(x)), vec(spec$d(x)))
}

# --- assemble the .cpp -------------------------------------------------
header <- '#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;
#include <cmath>
#include <vector>

// GENERATED by inst/tools/generate_tests_ad_array_ops.R -- do not edit by hand.
// AD coverage for + - / ^ and unary math on arrays, forward and reverse mode.
// Seeds are deliberately non-unit so a dropped/hard-coded incoming dot fails.

namespace {

constexpr double AD_TOL = 1e-6;
inline bool feq(double x, double y) { return std::abs(x - y) < AD_TOL; }

inline std::vector<double> dots(double s, std::size_t n = 6) {
  return std::vector<double>(n, s);
}

inline Array<Dual, Buffer<Dual, LBufferTrait>>
mk_dual(const std::vector<double>& v, const std::vector<double>& d) {
  Array<Dual, Buffer<Dual, LBufferTrait>> a(SI{v.size()});
  for (std::size_t i = 0; i < v.size(); i++) a.set(i, Dual(v[i], d[i]));
  return a;
}
inline Array<Double, Buffer<Double, LBufferTrait>>
mk_dbl(const std::vector<double>& v) {
  Array<Double, Buffer<Double, LBufferTrait>> a(SI{v.size()});
  for (std::size_t i = 0; i < v.size(); i++) a.set(i, Double(v[i]));
  return a;
}
inline Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>>
mk_rev(const std::vector<double>& v) {
  Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> a(SI{v.size()});
  for (std::size_t i = 0; i < v.size(); i++) a.set(i, ReverseDouble::Var(v[i]));
  return a;
}

template<class E>
bool chk_fwd(const E& expr, const std::vector<double>& val,
             const std::vector<double>& dot) {
  if (expr.size() != val.size()) return false;
  for (std::size_t i = 0; i < expr.size(); i++) {
    auto e = expr.get(i);
    if (!feq(get_val(e), val[i])) return false;
    if (!feq(e.dot, dot[i]))      return false;
  }
  return true;
}

template<class V, class E>
bool chk_rev1(V& var, const E& expr, const std::vector<double>& val,
              const std::vector<double>& grad) {
  if (expr.size() != val.size()) return false;
  for (std::size_t i = 0; i < expr.size(); i++) {
    auto t = expr.get(i);
    if (!feq(get_val(t), val[i])) return false;
    auto d = deriv(t, var.get(i));
    if (!feq(get_val(d.get(0)), grad[i])) return false;
  }
  return true;
}

template<class A, class B, class E>
bool chk_rev2(A& va, B& vb, const E& expr, const std::vector<double>& val,
              const std::vector<double>& ga, const std::vector<double>& gb) {
  if (expr.size() != val.size()) return false;
  for (std::size_t i = 0; i < expr.size(); i++) {
    auto t = expr.get(i);
    if (!feq(get_val(t), val[i])) return false;
    auto da = deriv(t, va.get(i));
    if (!feq(get_val(da.get(0)), ga[i])) return false;
    auto db = deriv(t, vb.get(i));
    if (!feq(get_val(db.get(0)), gb[i])) return false;
  }
  return true;
}

} // namespace

// [[Rcpp::export]]
void test_ad_array_ops() {
  TAPE_INTERN.clear();
'

writeLines(c(header, L, "}"), "src/tests_ADArrayOps.cpp")
cat("wrote src/tests_ADArrayOps.cpp (", length(L), "lines of assertions)\n")
