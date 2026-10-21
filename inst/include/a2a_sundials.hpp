#ifndef A2A_SUNDIALS_HPP
#define A2A_SUNDIALS_HPP

#include <cvodes/cvodes.h>
#include <idas/idas.h>
#include <nvector/nvector_serial.h>
#include <sundials/sundials_linearsolver.h>
#include <sunmatrix/sunmatrix_dense.h>

#include <cstdlib>
#include <string>
#include <vector>

namespace etr {

// slots must match predefined_known_types in R/TypeParser.R;
// the defaults live in the registry (cvode_options() / ida_options())
struct cvode_options {
  Double reltol;
  Double abstol;
  Logical stiff;
  Integer max_steps;
  Logical stats;
  cvode_options(Double reltol_, Double abstol_, Logical stiff_, Integer max_steps_, Logical stats_)
    : reltol(reltol_), abstol(abstol_), stiff(stiff_), max_steps(max_steps_), stats(stats_) {}
#ifndef STANDALONE_ETR
  explicit cvode_options(SEXP arg) :
    reltol(SEXP2Scalar<Double>(checked_elt(arg, "reltol", 5, "cvode_options"))),
    abstol(SEXP2Scalar<Double>(checked_elt(arg, "abstol", 5, "cvode_options"))),
    stiff(SEXP2Scalar<Logical>(checked_elt(arg, "stiff", 5, "cvode_options"))),
    max_steps(SEXP2Scalar<Integer>(checked_elt(arg, "max_steps", 5, "cvode_options"))),
    stats(SEXP2Scalar<Logical>(checked_elt(arg, "stats", 5, "cvode_options"))) {}
  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 5));
    SET_VECTOR_ELT(res, 0, Cast(reltol));
    SET_VECTOR_ELT(res, 1, Cast(abstol));
    SET_VECTOR_ELT(res, 2, Cast(stiff));
    SET_VECTOR_ELT(res, 3, Cast(max_steps));
    SET_VECTOR_ELT(res, 4, Cast(stats));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 5));
    SET_STRING_ELT(names, 0, Rf_mkChar("reltol"));
    SET_STRING_ELT(names, 1, Rf_mkChar("abstol"));
    SET_STRING_ELT(names, 2, Rf_mkChar("stiff"));
    SET_STRING_ELT(names, 3, Rf_mkChar("max_steps"));
    SET_STRING_ELT(names, 4, Rf_mkChar("stats"));
    Rf_setAttrib(res, R_NamesSymbol, names);
    Rf_setAttrib(res, R_ClassSymbol, Rf_mkString("cvode_options"));
    UNPROTECT(2);
    return res;
  }
#endif // !STANDALONE_ETR
  cvode_options() = default;
  cvode_options(const cvode_options&) = default;
  cvode_options(cvode_options&&) noexcept = default;
  cvode_options& operator=(const cvode_options&) = default;
  cvode_options& operator=(cvode_options&&) noexcept = default;
};

inline void print(const cvode_options& x) {
  PRINT_STREAM << "cvode_options" << std::endl;
  PRINT_STREAM << "  reltol: " << x.reltol << std::endl;
  PRINT_STREAM << "  abstol: " << x.abstol << std::endl;
  PRINT_STREAM << "  stiff: " << x.stiff << std::endl;
  PRINT_STREAM << "  max_steps: " << x.max_steps << std::endl;
  PRINT_STREAM << "  stats: " << x.stats << std::endl;
}

struct ida_options {
  Double reltol;
  Double abstol;
  Integer max_steps;
  Logical stats;
  ida_options(Double reltol_, Double abstol_, Integer max_steps_, Logical stats_)
    : reltol(reltol_), abstol(abstol_), max_steps(max_steps_), stats(stats_) {}
#ifndef STANDALONE_ETR
  explicit ida_options(SEXP arg) :
    reltol(SEXP2Scalar<Double>(checked_elt(arg, "reltol", 4, "ida_options"))),
    abstol(SEXP2Scalar<Double>(checked_elt(arg, "abstol", 4, "ida_options"))),
    max_steps(SEXP2Scalar<Integer>(checked_elt(arg, "max_steps", 4, "ida_options"))),
    stats(SEXP2Scalar<Logical>(checked_elt(arg, "stats", 4, "ida_options"))) {}
  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 4));
    SET_VECTOR_ELT(res, 0, Cast(reltol));
    SET_VECTOR_ELT(res, 1, Cast(abstol));
    SET_VECTOR_ELT(res, 2, Cast(max_steps));
    SET_VECTOR_ELT(res, 3, Cast(stats));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 4));
    SET_STRING_ELT(names, 0, Rf_mkChar("reltol"));
    SET_STRING_ELT(names, 1, Rf_mkChar("abstol"));
    SET_STRING_ELT(names, 2, Rf_mkChar("max_steps"));
    SET_STRING_ELT(names, 3, Rf_mkChar("stats"));
    Rf_setAttrib(res, R_NamesSymbol, names);
    Rf_setAttrib(res, R_ClassSymbol, Rf_mkString("ida_options"));
    UNPROTECT(2);
    return res;
  }
#endif // !STANDALONE_ETR
  ida_options() = default;
  ida_options(const ida_options&) = default;
  ida_options(ida_options&&) noexcept = default;
  ida_options& operator=(const ida_options&) = default;
  ida_options& operator=(ida_options&&) noexcept = default;
};

inline void print(const ida_options& x) {
  PRINT_STREAM << "ida_options" << std::endl;
  PRINT_STREAM << "  reltol: " << x.reltol << std::endl;
  PRINT_STREAM << "  abstol: " << x.abstol << std::endl;
  PRINT_STREAM << "  max_steps: " << x.max_steps << std::endl;
  PRINT_STREAM << "  stats: " << x.stats << std::endl;
}

} // namespace etr

namespace a2a {

using BorrowVec = etr::Array<etr::Double, etr::Borrow<etr::Double>>;

// NOTE: no max_steps <= 0 (SUNDIALS: unlimited), the solver loop cannot be interrupted from R
template <typename Opt> inline long int checked_max_steps(const Opt &opt, const char *solver) {
  etr::ass(!opt.max_steps.isNA() && etr::get_val(opt.max_steps) > 0,
           std::string(solver) + ": max_steps must be a positive integer");
  return static_cast<long int>(etr::get_val(opt.max_steps));
}
template <typename Opt> inline void check_tolerances(const Opt &opt, const char *solver) {
  etr::ass(!opt.reltol.isNA() && !opt.abstol.isNA() && etr::get_val(opt.reltol) > 0.0 &&
             etr::get_val(opt.abstol) > 0.0,
           std::string(solver) + ": reltol and abstol must be positive");
}

// Dense linear solver backed by LAPACK (dgetrf/dgetrs), same interface as
// SUNLinSol_Dense.
// NOTE: SUNDIALS' own dense LU is unblocked and single threaded; for n ~ 2000
// it is 10-20x slower than LAPACK, which dominates stiff runs
struct lapack_dense_content {
  std::vector<int> ipiv;
  sunindextype last_flag = 0;
};

inline lapack_dense_content *lapack_content(SUNLinearSolver S) {
  return static_cast<lapack_dense_content *>(S->content);
}

inline SUNLinearSolver_Type lapack_dense_gettype(SUNLinearSolver) {
  return SUNLINEARSOLVER_DIRECT;
}

inline SUNLinearSolver_ID lapack_dense_getid(SUNLinearSolver) {
  return SUNLINEARSOLVER_CUSTOM;
}

inline SUNErrCode lapack_dense_initialize(SUNLinearSolver S) {
  lapack_content(S)->last_flag = 0;
  return SUN_SUCCESS;
}

inline int lapack_dense_setup(SUNLinearSolver S, SUNMatrix A) {
  auto *content = lapack_content(S);
  int n = static_cast<int>(SUNDenseMatrix_Rows(A));
  int info = 0;
  content->ipiv.resize(static_cast<std::size_t>(n));
  F77_CALL(dgetrf)(&n, &n, SUNDenseMatrix_Data(A), &n, content->ipiv.data(), &info);
  content->last_flag = info;
  // info > 0: zero pivot, recoverable (the integrator retries with a smaller step)
  if (info > 0) return SUNLS_LUFACT_FAIL;
  if (info < 0) return SUN_ERR_ARG_CORRUPT;
  return SUN_SUCCESS;
}

inline int lapack_dense_solve(SUNLinearSolver S, SUNMatrix A, N_Vector x, N_Vector b,
                              sunrealtype) {
  auto *content = lapack_content(S);
  N_VScale(SUN_RCONST(1.0), b, x);
  int n = static_cast<int>(SUNDenseMatrix_Rows(A));
  int nrhs = 1;
  int info = 0;
  F77_CALL(dgetrs)("N", &n, &nrhs, SUNDenseMatrix_Data(A), &n, content->ipiv.data(),
                   N_VGetArrayPointer(x), &n, &info FCONE);
  content->last_flag = info;
  return info == 0 ? SUN_SUCCESS : SUN_ERR_ARG_CORRUPT;
}

inline sunindextype lapack_dense_lastflag(SUNLinearSolver S) {
  return lapack_content(S)->last_flag;
}

inline SUNErrCode lapack_dense_free(SUNLinearSolver S) {
  if (S == nullptr) return SUN_SUCCESS;
  delete lapack_content(S);
  S->content = nullptr;
  SUNLinSolFreeEmpty(S);
  return SUN_SUCCESS;
}

inline SUNLinearSolver lapack_dense_solver(SUNContext ctx) {
  SUNLinearSolver S = SUNLinSolNewEmpty(ctx);
  if (S == nullptr) return nullptr;
  S->ops->gettype = lapack_dense_gettype;
  S->ops->getid = lapack_dense_getid;
  S->ops->initialize = lapack_dense_initialize;
  S->ops->setup = lapack_dense_setup;
  S->ops->solve = lapack_dense_solve;
  S->ops->lastflag = lapack_dense_lastflag;
  S->ops->free = lapack_dense_free;
  S->content = new lapack_dense_content();
  return S;
}

// NOTE: the *GetReturnFlagName functions return malloc'ed memory
inline std::string flag_name(char *name) {
  std::string out = name ? name : "unknown flag";
  std::free(name);
  return out;
}

struct no_jac {};

// RHS: functor f(t, y, ydot, params). params is an opaque passthrough (double
// vector, scalar, or a new_type struct) -- cvode_solve never inspects it.
// jac: optional functor jac(t, y, J, params), nullptr -> difference quotients
template <typename F, typename P, typename J = no_jac> struct fct_ctx {
  const F *f;
  P *params;
  const J *jac = nullptr;
};

template <typename Ctx>
int rhs_trampoline(sunrealtype t, N_Vector y, N_Vector ydot, void *user_data) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *yp = N_VGetArrayPointer(y);
  sunrealtype *ydp = N_VGetArrayPointer(ydot);
  if (yp == nullptr || ydp == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  BorrowVec yv(yp, n, etr::Dim{n});
  BorrowVec ydv(ydp, n, etr::Dim{n});
  etr::Double tt = static_cast<double>(t);
  (*ctx->f)(tt, yv, ydv, *ctx->params);
  return 0;
}

// J: dense, column-major n x n like an R matrix; SUNDIALS zeroes it before the call
template <typename Ctx>
int cvode_jac_trampoline(sunrealtype t, N_Vector y, N_Vector, SUNMatrix J, void *user_data,
                         N_Vector, N_Vector, N_Vector) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *y_ptr = N_VGetArrayPointer(y);
  sunrealtype *j_ptr = SUNDenseMatrix_Data(J);
  if (y_ptr == nullptr || j_ptr == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  BorrowVec yv(y_ptr, n, etr::Dim{n});
  BorrowVec jv(j_ptr, n * n, etr::Dim{n, n});
  etr::Double tt = static_cast<double>(t);
  (*ctx->jac)(tt, yv, jv, *ctx->params);
  return 0;
}

struct cvode_workspace {
  SUNContext ctx = nullptr;
  N_Vector y = nullptr;
  SUNMatrix A = nullptr;
  SUNLinearSolver LS = nullptr;
  void *mem = nullptr;
  ~cvode_workspace() {
    if (mem) CVodeFree(&mem);
    if (LS) SUNLinSolFree(LS);
    if (A) SUNMatDestroy(A);
    if (y) N_VDestroy(y);
    if (ctx) SUNContext_Free(&ctx);
  }
};

inline void print_cvode_stats(void *mem) {
  long int steps = 0, rhs_evals = 0, jac_evals = 0, jac_rhs_evals = 0, lu_setups = 0,
           newton_iters = 0, newton_fails = 0, error_test_fails = 0;
  CVodeGetNumSteps(mem, &steps);
  CVodeGetNumRhsEvals(mem, &rhs_evals);
  CVodeGetNumJacEvals(mem, &jac_evals);
  CVodeGetNumLinRhsEvals(mem, &jac_rhs_evals);
  CVodeGetNumLinSolvSetups(mem, &lu_setups);
  CVodeGetNumNonlinSolvIters(mem, &newton_iters);
  CVodeGetNumNonlinSolvConvFails(mem, &newton_fails);
  CVodeGetNumErrTestFails(mem, &error_test_fails);
  Rprintf("cvode stats: steps %ld | rhs calls %ld (+ %ld for the jacobian) | "
          "jacobians %ld | LU setups %ld | newton iterations %ld | newton fails %ld | "
          "error test fails %ld\n",
          steps, rhs_evals, jac_rhs_evals, jac_evals, lu_setups, newton_iters, newton_fails,
          error_test_fails);
}

// cvode(rhs, y0, times, params, options[, jac]) -> length(times) x
// length(y0) matrix, row k = state at times[k] (row 0 = y0, times[0] = t0).
// options$stiff -> BDF, else ADAMS. Dense Newton (both), Jacobian from jac or
// difference quotients.
template <typename JAC, typename RHS, typename Y, typename T, typename P>
requires(
etr::IsArray<etr::Decayed<Y>> && etr::IsArray<etr::Decayed<T>>
)
inline auto cvode_impl(const RHS &rhs, const JAC *jac, const Y &y0, const T &times,
                       const P &params, const etr::cvode_options &opt) {
  const std::size_t neq = y0.size();
  const std::size_t nt = times.size();
  etr::ass<"cvode: need at least one initial state">(neq > 0);
  etr::ass<"cvode: need at least two time points">(nt >= 2);
  check_tolerances(opt, "cvode");
  const long int max_steps = checked_max_steps(opt, "cvode");
  etr::ass<"cvode: stiff must be TRUE or FALSE">(!opt.stiff.isNA());
  etr::ass<"cvode: stats must be TRUE or FALSE">(!opt.stats.isNA());

  P p_local = params; // fixed for the whole integration
  using Ctx = fct_ctx<RHS, P, JAC>;
  Ctx ctx{&rhs, &p_local, jac};

  cvode_workspace w;
  etr::ass<"cvode: SUNContext_Create failed">(SUNContext_Create(SUN_COMM_NULL, &w.ctx) == 0);
  SUNContext_ClearErrHandlers(w.ctx); // errors come back as return codes

  w.y = N_VNew_Serial(static_cast<sunindextype>(neq), w.ctx);
  etr::ass<"cvode: N_VNew_Serial failed">(w.y != nullptr);
  sunrealtype *yp = N_VGetArrayPointer(w.y);
  for (std::size_t i = 0; i < neq; ++i) {
    etr::Double yi = y0.get(i);
    etr::ass<"cvode: NA in initial state">(!yi.isNA());
    yp[i] = etr::get_val(yi);
  }

  const int lmm = etr::get_val(opt.stiff) ? CV_BDF : CV_ADAMS;
  w.mem = CVodeCreate(lmm, w.ctx);
  etr::ass<"cvode: CVodeCreate failed">(w.mem != nullptr);

  double t0 = etr::get_val(times.get(0));
  etr::ass<"cvode: CVodeInit failed">(CVodeInit(w.mem, &rhs_trampoline<Ctx>, t0, w.y) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSStolerances failed">(CVodeSStolerances(w.mem, etr::get_val(opt.reltol), etr::get_val(opt.abstol)) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetUserData failed">(CVodeSetUserData(w.mem, &ctx) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetMaxNumSteps failed">(CVodeSetMaxNumSteps(w.mem, max_steps) == CV_SUCCESS);

  w.A = SUNDenseMatrix(static_cast<sunindextype>(neq),
                       static_cast<sunindextype>(neq), w.ctx);
  etr::ass<"cvode: SUNDenseMatrix failed">(w.A != nullptr);
  w.LS = lapack_dense_solver(w.ctx);
  etr::ass<"cvode: creating the LAPACK dense linear solver failed">(w.LS != nullptr);
  etr::ass<"cvode: CVodeSetLinearSolver failed">(
    CVodeSetLinearSolver(w.mem, w.LS, w.A) == CV_SUCCESS);
  if constexpr (!std::is_same_v<JAC, no_jac>) {
    etr::ass<"cvode: CVodeSetJacFn failed">(
      CVodeSetJacFn(w.mem, &cvode_jac_trampoline<Ctx>) == CV_SUCCESS);
  }

  etr::Array<etr::Double, etr::Buffer<etr::Double, etr::RBufferTrait>> res(etr::SI{neq * nt});
  res.dim = etr::Dim{nt, neq};
  for (std::size_t j = 0; j < neq; ++j) {
    res.set(j * nt + 0, etr::Double(yp[j]));
  }
  for (std::size_t k = 1; k < nt; ++k) {
    sunrealtype tret = 0.0;
    int flag = CVode(w.mem, etr::get_val(times.get(k)), w.y, &tret, CV_NORMAL);
    etr::ass(flag == CV_SUCCESS,
             std::string("cvode: integration failed at t = ") + std::to_string(tret) + ": " +
               flag_name(CVodeGetReturnFlagName(flag)));
    for (std::size_t j = 0; j < neq; ++j) {
      res.set(j * nt + k, etr::Double(yp[j]));
    }
  }
  if (etr::get_val(opt.stats)) print_cvode_stats(w.mem);
  return res;
}

template <typename RHS, typename Y, typename T, typename P>
inline auto cvode_solve(const RHS &rhs, const Y &y0, const T &times, const P &params,
                        const etr::cvode_options &opt) {
  return cvode_impl<no_jac>(rhs, nullptr, y0, times, params, opt);
}

template <typename RHS, typename Y, typename T, typename P, typename JAC>
inline auto cvode_solve(const RHS &rhs, const Y &y0, const T &times, const P &params,
                        const etr::cvode_options &opt, const JAC &jac) {
  return cvode_impl(rhs, &jac, y0, times, params, opt);
}

template <typename Ctx>
int res_trampoline(sunrealtype t, N_Vector y, N_Vector yp, N_Vector r, void *user_data) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *y_ptr = N_VGetArrayPointer(y);
  sunrealtype *yp_ptr = N_VGetArrayPointer(yp);
  sunrealtype *r_ptr = N_VGetArrayPointer(r);
  if (y_ptr == nullptr || yp_ptr == nullptr || r_ptr == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  BorrowVec yv(y_ptr, n, etr::Dim{n});
  BorrowVec ypv(yp_ptr, n, etr::Dim{n});
  BorrowVec rv(r_ptr, n, etr::Dim{n});
  etr::Double tt = static_cast<double>(t);
  (*ctx->f)(tt, yv, ypv, rv, *ctx->params);
  return 0;
}

// J = dF/dy + cj * dF/dyp, dense column-major; SUNDIALS zeroes it before the call
template <typename Ctx>
int ida_jac_trampoline(sunrealtype t, sunrealtype cj, N_Vector y, N_Vector yp, N_Vector,
                       SUNMatrix J, void *user_data, N_Vector, N_Vector, N_Vector) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *y_ptr = N_VGetArrayPointer(y);
  sunrealtype *yp_ptr = N_VGetArrayPointer(yp);
  sunrealtype *j_ptr = SUNDenseMatrix_Data(J);
  if (y_ptr == nullptr || yp_ptr == nullptr || j_ptr == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  BorrowVec yv(y_ptr, n, etr::Dim{n});
  BorrowVec ypv(yp_ptr, n, etr::Dim{n});
  BorrowVec jv(j_ptr, n * n, etr::Dim{n, n});
  etr::Double tt = static_cast<double>(t);
  etr::Double cjj = static_cast<double>(cj);
  (*ctx->jac)(tt, cjj, yv, ypv, jv, *ctx->params);
  return 0;
}

struct ida_workspace {
  SUNContext ctx = nullptr;
  N_Vector y = nullptr;
  N_Vector yp = nullptr;
  N_Vector id = nullptr;
  SUNMatrix A = nullptr;
  SUNLinearSolver LS = nullptr;
  void *mem = nullptr;
  ~ida_workspace() {
    if (mem) IDAFree(&mem);
    if (LS) SUNLinSolFree(LS);
    if (A) SUNMatDestroy(A);
    if (id) N_VDestroy(id);
    if (yp) N_VDestroy(yp);
    if (y) N_VDestroy(y);
    if (ctx) SUNContext_Free(&ctx);
  }
};

inline void print_ida_stats(void *mem) {
  long int steps = 0, res_evals = 0, jac_evals = 0, jac_res_evals = 0, lu_setups = 0,
           newton_iters = 0, newton_fails = 0, error_test_fails = 0;
  IDAGetNumSteps(mem, &steps);
  IDAGetNumResEvals(mem, &res_evals);
  IDAGetNumJacEvals(mem, &jac_evals);
  IDAGetNumLinResEvals(mem, &jac_res_evals);
  IDAGetNumLinSolvSetups(mem, &lu_setups);
  IDAGetNumNonlinSolvIters(mem, &newton_iters);
  IDAGetNumNonlinSolvConvFails(mem, &newton_fails);
  IDAGetNumErrTestFails(mem, &error_test_fails);
  Rprintf("ida stats: steps %ld | residual calls %ld (+ %ld for the jacobian) | "
          "jacobians %ld | LU setups %ld | newton iterations %ld | newton fails %ld | "
          "error test fails %ld\n",
          steps, res_evals, jac_res_evals, jac_evals, lu_setups, newton_iters, newton_fails,
          error_test_fails);
}

template <typename Vec>
void copy_to_nvector(const Vec &src, N_Vector dst, std::size_t n) {
  sunrealtype *ptr = N_VGetArrayPointer(dst);
  for (std::size_t i = 0; i < n; ++i) {
    etr::Double v = src.get(i);
    etr::ass<"ida: NA in y0, yp0 or id">(!v.isNA());
    ptr[i] = etr::get_val(v);
  }
}

// ida(res, y0, yp0, times, params, id, options[, jac]) -> length(times) x
// length(y0) matrix of y. id: 1 = differential, 0 = algebraic.
// yp0 (and the algebraic part of y0) is made consistent via IDACalcIC.
// Jacobian from jac or difference quotients.
template <typename JAC, typename RES, typename Y, typename YP, typename T, typename P, typename ID>
requires(
etr::IsArray<etr::Decayed<Y>> && etr::IsArray<etr::Decayed<YP>> &&
etr::IsArray<etr::Decayed<T>> && etr::IsArray<etr::Decayed<ID>>
)
inline auto ida_impl(const RES &res_fct, const JAC *jac, const Y &y0, const YP &yp0,
                     const T &times, const P &params, const ID &id,
                     const etr::ida_options &opt) {
  const std::size_t neq = y0.size();
  const std::size_t nt = times.size();
  etr::ass<"ida: need at least one initial state">(neq > 0);
  etr::ass<"ida: y0, yp0 and id must have the same length">(
    yp0.size() == neq && id.size() == neq);
  etr::ass<"ida: need at least two time points">(nt >= 2);
  check_tolerances(opt, "ida");
  const long int max_steps = checked_max_steps(opt, "ida");
  etr::ass<"ida: stats must be TRUE or FALSE">(!opt.stats.isNA());

  P p_local = params;
  using Ctx = fct_ctx<RES, P, JAC>;
  Ctx ctx{&res_fct, &p_local, jac};

  ida_workspace w;
  etr::ass<"ida: SUNContext_Create failed">(SUNContext_Create(SUN_COMM_NULL, &w.ctx) == 0);
  SUNContext_ClearErrHandlers(w.ctx);

  const auto n = static_cast<sunindextype>(neq);
  w.y = N_VNew_Serial(n, w.ctx);
  w.yp = N_VNew_Serial(n, w.ctx);
  w.id = N_VNew_Serial(n, w.ctx);
  etr::ass<"ida: N_VNew_Serial failed">(w.y != nullptr && w.yp != nullptr && w.id != nullptr);
  copy_to_nvector(y0, w.y, neq);
  copy_to_nvector(yp0, w.yp, neq);
  copy_to_nvector(id, w.id, neq);
  sunrealtype *id_ptr = N_VGetArrayPointer(w.id);
  for (std::size_t i = 0; i < neq; ++i) {
    etr::ass<"ida: id entries must be 0 (algebraic) or 1 (differential)">(
      id_ptr[i] == 0.0 || id_ptr[i] == 1.0);
  }

  w.mem = IDACreate(w.ctx);
  etr::ass<"ida: IDACreate failed">(w.mem != nullptr);

  double t0 = etr::get_val(times.get(0));
  etr::ass<"ida: IDAInit failed">(IDAInit(w.mem, &res_trampoline<Ctx>, t0, w.y, w.yp) == IDA_SUCCESS);
  etr::ass<"ida: IDASStolerances failed">(IDASStolerances(w.mem, etr::get_val(opt.reltol), etr::get_val(opt.abstol)) == IDA_SUCCESS);
  etr::ass<"ida: IDASetUserData failed">(IDASetUserData(w.mem, &ctx) == IDA_SUCCESS);
  etr::ass<"ida: IDASetId failed">(IDASetId(w.mem, w.id) == IDA_SUCCESS);
  etr::ass<"ida: IDASetMaxNumSteps failed">(IDASetMaxNumSteps(w.mem, max_steps) == IDA_SUCCESS);

  w.A = SUNDenseMatrix(n, n, w.ctx);
  etr::ass<"ida: SUNDenseMatrix failed">(w.A != nullptr);
  w.LS = lapack_dense_solver(w.ctx);
  etr::ass<"ida: creating the LAPACK dense linear solver failed">(w.LS != nullptr);
  etr::ass<"ida: IDASetLinearSolver failed">(IDASetLinearSolver(w.mem, w.LS, w.A) == IDA_SUCCESS);
  if constexpr (!std::is_same_v<JAC, no_jac>) {
    etr::ass<"ida: IDASetJacFn failed">(IDASetJacFn(w.mem, &ida_jac_trampoline<Ctx>) == IDA_SUCCESS);
  }

  int ic_flag = IDACalcIC(w.mem, IDA_YA_YDP_INIT, etr::get_val(times.get(1)));
  etr::ass(ic_flag == IDA_SUCCESS,
           std::string("ida: IDACalcIC failed (no consistent initial values found): ") +
             flag_name(IDAGetReturnFlagName(ic_flag)));
  etr::ass<"ida: IDAGetConsistentIC failed">(IDAGetConsistentIC(w.mem, w.y, w.yp) == IDA_SUCCESS);

  sunrealtype *y_ptr = N_VGetArrayPointer(w.y);
  etr::Array<etr::Double, etr::Buffer<etr::Double, etr::RBufferTrait>> res(etr::SI{neq * nt});
  res.dim = etr::Dim{nt, neq};
  for (std::size_t j = 0; j < neq; ++j) {
    res.set(j * nt + 0, etr::Double(y_ptr[j]));
  }
  for (std::size_t k = 1; k < nt; ++k) {
    sunrealtype tret = 0.0;
    int flag = IDASolve(w.mem, etr::get_val(times.get(k)), &tret, w.y, w.yp, IDA_NORMAL);
    etr::ass(flag == IDA_SUCCESS,
             std::string("ida: integration failed at t = ") + std::to_string(tret) + ": " +
               flag_name(IDAGetReturnFlagName(flag)));
    for (std::size_t j = 0; j < neq; ++j) {
      res.set(j * nt + k, etr::Double(y_ptr[j]));
    }
  }
  if (etr::get_val(opt.stats)) print_ida_stats(w.mem);
  return res;
}

template <typename RES, typename Y, typename YP, typename T, typename P, typename ID>
inline auto ida_solve(const RES &res_fct, const Y &y0, const YP &yp0, const T &times,
                      const P &params, const ID &id, const etr::ida_options &opt) {
  return ida_impl<no_jac>(res_fct, nullptr, y0, yp0, times, params, id, opt);
}

template <typename RES, typename Y, typename YP, typename T, typename P, typename ID,
          typename JAC>
inline auto ida_solve(const RES &res_fct, const Y &y0, const YP &yp0, const T &times,
                      const P &params, const ID &id, const etr::ida_options &opt,
                      const JAC &jac) {
  return ida_impl(res_fct, &jac, y0, yp0, times, params, id, opt);
}

} // namespace a2a

#endif
