extract_fn_def <- function(src) {
  fn_def <- sub("(?s)^.*?SEXP getXPtr\\(\\);\\s*", "", src, perl = TRUE)
  fn_def <- sub("(?s)\\s*SEXP getXPtr\\(\\) \\{.*$", "", fn_def, perl = TRUE)
  fname <- regmatches(fn_def, regexpr("[A-Za-z_][A-Za-z0-9_]*(?=\\s*\\()", fn_def, perl = TRUE))
  list(def = fn_def, name = fname)
}

# f is (y, data) -> ydot. jacobian() needs f as a local fn value in its own
# translation unit, so f's formals/body are spliced in as "ode <- function(...) {...}"
# rather than referenced across translate() calls.
build_jacobian_wrapper <- function(f) {
  ode_def <- as.call(list(quote(`<-`), quote(ode),
    as.call(list(quote(`function`), formals(f), body(f)))))
  wrapper_body <- as.call(list(quote(`{`),
    quote(argtypes(
      y |> type(vec(double)),
      data |> type(vec(double))
    )),
    quote(returntype(mat(double))),
    ode_def,
    quote(return(jacobian(ode, y, data)))
  ))
  wrapper <- function(y, data) NULL
  body(wrapper) <- wrapper_body
  wrapper
}

translate_for_deSolve <- function(f, npar, jacobian = FALSE) {
  fn_def <- extract_fn_def(translate(f, output = "XPtr", getsource = TRUE, debug = FALSE))

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
    std::vector<double> data_buf(np + 1);
    data_buf[0] = *t;
    for (std::size_t i = 0; i < np; ++i) data_buf[i + 1] = a2a_params[i];
    etr::Array<etr::Double, etr::Borrow<etr::Double>> y_(y, n, {n});
    etr::Array<etr::Double, etr::Borrow<etr::Double>> data_(data_buf.data(), np + 1, {np + 1});
    auto ydot_result = %s(y_, data_);
    for (std::size_t i = 0; i < n; ++i) ydot[i] = etr::get_val(ydot_result.get(i));
    }
    @@JACDEF@@
    '
  code <- sprintf(template, npar, npar, npar, fn_def$name)
  code <- paste(strsplit(code, "@@FNDEF@@", fixed = TRUE)[[1]], collapse = fn_def$def)

  jacfunc_name <- NULL
  jac_glue <- ""
  if (jacobian) {
    jac_fn_def <- extract_fn_def(translate(build_jacobian_wrapper(f), output = "XPtr",
      getsource = TRUE, derivative = "forward", debug = FALSE))

    jac_template <- '
    %s

    A2A_EXPORT void a2a_jac(int *neq, double *t, double *y, int *ml, int *mu,
    double *pd, int *nrowpd, double *rpar, int *ipar) {
    const std::size_t n = static_cast<std::size_t>(*neq);
    const std::size_t np = %d;
    std::vector<double> data_buf(np + 1);
    data_buf[0] = *t;
    for (std::size_t i = 0; i < np; ++i) data_buf[i + 1] = a2a_params[i];
    etr::Array<etr::Double, etr::Borrow<etr::Double>> y_(y, n, {n});
    etr::Array<etr::Double, etr::Borrow<etr::Double>> data_(data_buf.data(), np + 1, {np + 1});
    etr::Array<etr::Double, etr::Borrow<etr::Double>> jac_(pd, n * n, {n, n});
    jac_ = %s(y_, data_);
    }
    '
    jac_glue <- sprintf(jac_template, jac_fn_def$def, npar, jac_fn_def$name)
    jacfunc_name <- "a2a_jac"
  }
  code <- sub("@@JACDEF@@", jac_glue, code, fixed = TRUE)

  Rcpp::sourceCpp(code = code, cacheDir = tempdir())
  dll <- tail(grep("^sourceCpp", names(getLoadedDLLs()), value = TRUE), 1L)
  list(
    dll = dll, func = "a2a_derivs", initfunc = "a2a_initmod", jacfunc = jacfunc_name
  )
}
