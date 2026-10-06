library(deSolve)
library(microbenchmark)

compile_for_desolve <- function(f, npar) {
  src <- ast2ast::translate(f, output = "XPtr", getsource = TRUE, debug = FALSE)
  fn_def <- sub("(?s)^.*?SEXP getXPtr\\(\\);\\s*", "", src, perl = TRUE)
  fn_def <- sub("(?s)\\s*SEXP getXPtr\\(\\) \\{.*$", "", fn_def, perl = TRUE)
  fname <- regmatches(fn_def,
    regexpr("[A-Za-z_][A-Za-z0-9_]*(?=\\s*\\()", fn_def, perl = TRUE))
  template <- '
#include <Rcpp.h>
// [[Rcpp::depends(ast2ast)]]
// [[Rcpp::plugins(cpp20)]]
#include "etr.hpp"

#ifdef _WIN32
#define A2A_EXPORT extern "C" __declspec(dllexport)
#else
#define A2A_EXPORT extern "C"
#endif

// [[Rcpp::export]]
int a2a_marker() { return 0; }

static double a2a_params[%d];

A2A_EXPORT void a2a_initmod(void (*odeparms)(int *, double *)) {
  int n = %d;
  odeparms(&n, a2a_params);
}

@@FNDEF@@

A2A_EXPORT void a2a_derivs(int *neq, double *t, double *y, double *ydot,
                           double *yout, int *ip) {
  const std::size_t n = static_cast<std::size_t>(*neq);
  const std::size_t np = %d;
  etr::Array<etr::Double, etr::Borrow<etr::Double>> y_(y, n, {n});
  etr::Array<etr::Double, etr::Borrow<etr::Double>> ydot_(ydot, n, {n});
  etr::Array<etr::Double, etr::Borrow<etr::Double>> params_(a2a_params, np, {np});
  %s(etr::Double(*t), y_, ydot_, params_);
}
'
  code <- sprintf(template, npar, npar, npar, fname)
  code <- paste(strsplit(code, "@@FNDEF@@", fixed = TRUE)[[1]], collapse = fn_def)

  Rcpp::sourceCpp(code = code, cacheDir = tempdir())
  dll <- tail(grep("^sourceCpp", names(getLoadedDLLs()), value = TRUE), 1L)

  function(y, times, parms, ...) {
    ode(y = y, times = times, parms = parms,
      func = "a2a_derivs", initfunc = "a2a_initmod", dllname = dll,
      nout = 0, ...)
  }
}

# ==========================================================================
# Lorenz: same solver, same grid, only the RHS language differs.
# ==========================================================================
Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- a * X + Y * Z
    dY <- b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
  })
}

lorenz_fct <- function(t, state, state_deriv, params) {
  argtypes(
    t |> type(double),
    state |> type(borrow_vec(double)) |> ref(),
    state_deriv |> type(borrow_vec(double)) |> ref(),
    params |> type(borrow_vec(double)) |> ref()
  )
  a <- params[[1L]]
  b <- params[[2L]]
  cpar <- params[[3L]]
  state_deriv[[1L]] <- a * state[[1L]] + state[[2L]] * state[[3L]]
  state_deriv[[2L]] <- b * (state[[2L]] - state[[3L]])
  state_deriv[[3L]] <- -state[[1L]] * state[[2L]] + cpar * state[[2L]] - state[[3L]]
}

parameters <- c(a = -8 / 3, b = -10, c = 28)
state <- c(X = 1, Y = 1, Z = 1)
times <- seq(0, 100, by = 0.01)

lorenz_solver <- compile_for_desolve(lorenz_fct, npar = 3)

out_r <- ode(y = state, times = times, func = Lorenz, parms = parameters)
out_c <- lorenz_solver(state, times, parameters)

cat("max abs diff R-RHS vs compiled-RHS:",
  max(abs(out_r[, -1] - out_c[, -1])), "\n")
stopifnot(isTRUE(all.equal(out_r[, -1], out_c[, -1], tolerance = 1e-6)))

microbenchmark(
  R_rhs        = ode(state, times, func = Lorenz, parms = parameters),
  compiled_rhs = lorenz_solver(state, times, parameters),
  times = 10
)
