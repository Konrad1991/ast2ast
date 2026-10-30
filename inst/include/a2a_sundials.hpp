#ifndef A2A_SUNDIALS_HPP
#define A2A_SUNDIALS_HPP

#include <cvodes/cvodes.h>
#include <idas/idas.h>
#include <nvector/nvector_serial.h>
#include <sundials/sundials_linearsolver.h>
#include <sunlinsol/sunlinsol_dense.h>
#include <sunmatrix/sunmatrix_dense.h>
#include <sunnonlinsol/sunnonlinsol_fixedpoint.h>

#include <algorithm>
#include <cstdlib>
#include <memory>
#include <string>
#include <vector>

namespace etr {

// slots must match predefined_known_types in R/TypeParser.R;
// the defaults live in the registry (cvode_options() / ida_options())
// linear_solver: 1 = SUNDIALS dense LU, 2 = LAPACK (linear_solvers in FunctionRegistry.R)
inline const char *linear_solver_name(const Integer &code) {
  if (code.isNA()) return "NA";
  const int c = get_val(code);
  return c == 1 ? "sundials" : c == 2 ? "lapack" : "unknown";
}

struct cvode_options {
  Double reltol;
  Double abstol;
  Logical stiff;
  Integer max_steps;
  Logical stats;
  Integer linear_solver;
  cvode_options(Double reltol_, Double abstol_, Logical stiff_, Integer max_steps_, Logical stats_,
                Integer linear_solver_)
    : reltol(reltol_), abstol(abstol_), stiff(stiff_), max_steps(max_steps_), stats(stats_),
      linear_solver(linear_solver_) {}
#ifndef STANDALONE_ETR
  explicit cvode_options(SEXP arg) :
    reltol(SEXP2Scalar<Double>(checked_elt(arg, "reltol", 6, "cvode_options"))),
    abstol(SEXP2Scalar<Double>(checked_elt(arg, "abstol", 6, "cvode_options"))),
    stiff(SEXP2Scalar<Logical>(checked_elt(arg, "stiff", 6, "cvode_options"))),
    max_steps(SEXP2Scalar<Integer>(checked_elt(arg, "max_steps", 6, "cvode_options"))),
    stats(SEXP2Scalar<Logical>(checked_elt(arg, "stats", 6, "cvode_options"))),
    linear_solver(SEXP2Scalar<Integer>(checked_elt(arg, "linear_solver", 6, "cvode_options"))) {}
  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 6));
    SET_VECTOR_ELT(res, 0, Cast(reltol));
    SET_VECTOR_ELT(res, 1, Cast(abstol));
    SET_VECTOR_ELT(res, 2, Cast(stiff));
    SET_VECTOR_ELT(res, 3, Cast(max_steps));
    SET_VECTOR_ELT(res, 4, Cast(stats));
    SET_VECTOR_ELT(res, 5, Cast(linear_solver));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, Rf_mkChar("reltol"));
    SET_STRING_ELT(names, 1, Rf_mkChar("abstol"));
    SET_STRING_ELT(names, 2, Rf_mkChar("stiff"));
    SET_STRING_ELT(names, 3, Rf_mkChar("max_steps"));
    SET_STRING_ELT(names, 4, Rf_mkChar("stats"));
    SET_STRING_ELT(names, 5, Rf_mkChar("linear_solver"));
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
  PRINT_STREAM << "  linear_solver: " << linear_solver_name(x.linear_solver) << std::endl;
}

struct ida_options {
  Double reltol;
  Double abstol;
  Integer max_steps;
  Logical stats;
  Integer linear_solver;
  ida_options(Double reltol_, Double abstol_, Integer max_steps_, Logical stats_,
              Integer linear_solver_)
    : reltol(reltol_), abstol(abstol_), max_steps(max_steps_), stats(stats_),
      linear_solver(linear_solver_) {}
#ifndef STANDALONE_ETR
  explicit ida_options(SEXP arg) :
    reltol(SEXP2Scalar<Double>(checked_elt(arg, "reltol", 5, "ida_options"))),
    abstol(SEXP2Scalar<Double>(checked_elt(arg, "abstol", 5, "ida_options"))),
    max_steps(SEXP2Scalar<Integer>(checked_elt(arg, "max_steps", 5, "ida_options"))),
    stats(SEXP2Scalar<Logical>(checked_elt(arg, "stats", 5, "ida_options"))),
    linear_solver(SEXP2Scalar<Integer>(checked_elt(arg, "linear_solver", 5, "ida_options"))) {}
  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 5));
    SET_VECTOR_ELT(res, 0, Cast(reltol));
    SET_VECTOR_ELT(res, 1, Cast(abstol));
    SET_VECTOR_ELT(res, 2, Cast(max_steps));
    SET_VECTOR_ELT(res, 3, Cast(stats));
    SET_VECTOR_ELT(res, 4, Cast(linear_solver));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 5));
    SET_STRING_ELT(names, 0, Rf_mkChar("reltol"));
    SET_STRING_ELT(names, 1, Rf_mkChar("abstol"));
    SET_STRING_ELT(names, 2, Rf_mkChar("max_steps"));
    SET_STRING_ELT(names, 3, Rf_mkChar("stats"));
    SET_STRING_ELT(names, 4, Rf_mkChar("linear_solver"));
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
  PRINT_STREAM << "  linear_solver: " << linear_solver_name(x.linear_solver) << std::endl;
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
template <typename Opt> inline int checked_linear_solver(const Opt &opt, const char *solver) {
  const int code = opt.linear_solver.isNA() ? 0 : etr::get_val(opt.linear_solver);
  etr::ass(code == 1 || code == 2,
           std::string(solver) + ": linear_solver must be \"sundials\" or \"lapack\"");
  return code;
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

// NOTE: SUNDIALS' LU is the default; with OpenBLAS, concurrent dgetrf calls
// serialise, so LAPACK does not scale inside pmap
inline SUNLinearSolver dense_solver(int code, N_Vector y, SUNMatrix A, SUNContext ctx) {
  return code == 2 ? lapack_dense_solver(ctx) : SUNLinSol_Dense(y, A, ctx);
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
  SUNNonlinearSolver NLS = nullptr;
  void *mem = nullptr;
  ~cvode_workspace() {
    if (mem) CVodeFree(&mem);
    if (NLS) SUNNonlinSolFree(NLS);
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
// options$stiff -> BDF with dense Newton (Jacobian from jac or difference
// quotients), else ADAMS with fixed-point iteration (no Jacobian, as deSolve's adams).
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
  const int linear_solver = checked_linear_solver(opt, "cvode");

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

  const bool stiff = etr::get_val(opt.stiff);
  w.mem = CVodeCreate(stiff ? CV_BDF : CV_ADAMS, w.ctx);
  etr::ass<"cvode: CVodeCreate failed">(w.mem != nullptr);

  double t0 = etr::get_val(times.get(0));
  etr::ass<"cvode: CVodeInit failed">(CVodeInit(w.mem, &rhs_trampoline<Ctx>, t0, w.y) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSStolerances failed">(CVodeSStolerances(w.mem, etr::get_val(opt.reltol), etr::get_val(opt.abstol)) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetUserData failed">(CVodeSetUserData(w.mem, &ctx) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetMaxNumSteps failed">(CVodeSetMaxNumSteps(w.mem, max_steps) == CV_SUCCESS);

  if (stiff) {
    w.A = SUNDenseMatrix(static_cast<sunindextype>(neq),
                         static_cast<sunindextype>(neq), w.ctx);
    etr::ass<"cvode: SUNDenseMatrix failed">(w.A != nullptr);
    w.LS = dense_solver(linear_solver, w.y, w.A, w.ctx);
    etr::ass<"cvode: creating the dense linear solver failed">(w.LS != nullptr);
    etr::ass<"cvode: CVodeSetLinearSolver failed">(
      CVodeSetLinearSolver(w.mem, w.LS, w.A) == CV_SUCCESS);
    if constexpr (!std::is_same_v<JAC, no_jac>) {
      etr::ass<"cvode: CVodeSetJacFn failed">(
        CVodeSetJacFn(w.mem, &cvode_jac_trampoline<Ctx>) == CV_SUCCESS);
    }
  } else {
    w.NLS = SUNNonlinSol_FixedPoint(w.y, 0, w.ctx);
    etr::ass<"cvode: creating the fixed-point solver failed">(w.NLS != nullptr);
    etr::ass<"cvode: CVodeSetNonlinearSolver failed">(
      CVodeSetNonlinearSolver(w.mem, w.NLS) == CV_SUCCESS);
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

// ---------- forward mode (Dual): CVODES forward sensitivities ----------------
// Dual carries one direction (y0dot, pdot) -> one sensitivity s = dy/d(direction),
// s(t0) = y0dot. Result: Dual(y, s).
// f and jac take Dual; val parts view SUNDIALS memory, dot parts the buffers below.
using DualBorrowVec = etr::Array<etr::Dual, etr::Borrow<etr::Dual>>;

template <typename F, typename P, typename J = no_jac> struct dual_fct_ctx {
  const F *f;
  P *params;
  const J *jac = nullptr;
  std::vector<double> zero_dot, sink_dot, sink_val, jac_dot;
};

template <typename Ctx>
int dual_rhs_trampoline(sunrealtype t, N_Vector y, N_Vector ydot, void *user_data) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *yp = N_VGetArrayPointer(y);
  sunrealtype *ydp = N_VGetArrayPointer(ydot);
  if (yp == nullptr || ydp == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  // refilled: f may write into y
  std::fill(ctx->zero_dot.begin(), ctx->zero_dot.end(), 0.0);
  DualBorrowVec yv(yp, ctx->zero_dot.data(), n, etr::Dim{n});
  DualBorrowVec ydv(ydp, ctx->sink_dot.data(), n, etr::Dim{n});
  etr::Dual tt(static_cast<double>(t), 0.0);
  (*ctx->f)(tt, yv, ydv, *ctx->params);
  return 0;
}

// ySdot_i = J yS_i + df/dp pdot = dot part of f(Dual(y, yS_i), p)
template <typename Ctx>
int sens_rhs_trampoline(int Ns, sunrealtype t, N_Vector y, N_Vector, N_Vector *yS,
                        N_Vector *ySdot, void *user_data, N_Vector, N_Vector) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *yp = N_VGetArrayPointer(y);
  if (yp == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  etr::Dual tt(static_cast<double>(t), 0.0);
  for (int i = 0; i < Ns; ++i) {
    sunrealtype *sp = N_VGetArrayPointer(yS[i]);
    sunrealtype *sdp = N_VGetArrayPointer(ySdot[i]);
    if (sp == nullptr || sdp == nullptr) return -1;
    DualBorrowVec yv(yp, sp, n, etr::Dim{n});
    DualBorrowVec ydv(ctx->sink_val.data(), sdp, n, etr::Dim{n});
    (*ctx->f)(tt, yv, ydv, *ctx->params);
  }
  return 0;
}

template <typename Ctx>
int dual_jac_trampoline(sunrealtype t, N_Vector y, N_Vector, SUNMatrix J, void *user_data,
                        N_Vector, N_Vector, N_Vector) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *y_ptr = N_VGetArrayPointer(y);
  sunrealtype *j_ptr = SUNDenseMatrix_Data(J);
  if (y_ptr == nullptr || j_ptr == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  std::fill(ctx->zero_dot.begin(), ctx->zero_dot.end(), 0.0);
  DualBorrowVec yv(y_ptr, ctx->zero_dot.data(), n, etr::Dim{n});
  DualBorrowVec jv(j_ptr, ctx->jac_dot.data(), n * n, etr::Dim{n, n});
  etr::Dual tt(static_cast<double>(t), 0.0);
  (*ctx->jac)(tt, yv, jv, *ctx->params);
  return 0;
}

struct cvodes_workspace {
  SUNContext ctx = nullptr;
  N_Vector y = nullptr;
  N_Vector *yS = nullptr;
  SUNMatrix A = nullptr;
  SUNLinearSolver LS = nullptr;
  SUNNonlinearSolver NLS = nullptr;
  SUNNonlinearSolver NLS_sens = nullptr;
  void *mem = nullptr;
  ~cvodes_workspace() {
    if (mem) CVodeFree(&mem);
    if (NLS_sens) SUNNonlinSolFree(NLS_sens);
    if (NLS) SUNNonlinSolFree(NLS);
    if (LS) SUNLinSolFree(LS);
    if (A) SUNMatDestroy(A);
    if (yS) N_VDestroyVectorArray(yS, 1);
    if (y) N_VDestroy(y);
    if (ctx) SUNContext_Free(&ctx);
  }
};

template <typename JAC, typename RHS, typename Y, typename T, typename P>
requires(etr::IsArray<etr::Decayed<Y>> && etr::IsArray<etr::Decayed<T>>)
inline auto cvodes_forward_impl(const RHS &rhs, const JAC *jac, const Y &y0, const T &times,
                                const P &params, const etr::cvode_options &opt) {
  const std::size_t neq = y0.size();
  const std::size_t nt = times.size();
  etr::ass<"cvode: need at least one initial state">(neq > 0);
  etr::ass<"cvode: need at least two time points">(nt >= 2);
  check_tolerances(opt, "cvode");
  const long int max_steps = checked_max_steps(opt, "cvode");
  etr::ass<"cvode: stiff must be TRUE or FALSE">(!opt.stiff.isNA());
  etr::ass<"cvode: stats must be TRUE or FALSE">(!opt.stats.isNA());
  const int linear_solver = checked_linear_solver(opt, "cvode");

  P p_local = params;
  using Ctx = dual_fct_ctx<RHS, P, JAC>;
  Ctx ctx{&rhs, &p_local, jac};
  ctx.zero_dot.assign(neq, 0.0);
  ctx.sink_dot.assign(neq, 0.0);
  ctx.sink_val.assign(neq, 0.0);
  if constexpr (!std::is_same_v<JAC, no_jac>) ctx.jac_dot.assign(neq * neq, 0.0);

  cvodes_workspace w;
  etr::ass<"cvode: SUNContext_Create failed">(SUNContext_Create(SUN_COMM_NULL, &w.ctx) == 0);
  SUNContext_ClearErrHandlers(w.ctx);

  w.y = N_VNew_Serial(static_cast<sunindextype>(neq), w.ctx);
  etr::ass<"cvode: N_VNew_Serial failed">(w.y != nullptr);
  w.yS = N_VCloneVectorArray(1, w.y);
  etr::ass<"cvode: N_VCloneVectorArray failed">(w.yS != nullptr);
  sunrealtype *yp = N_VGetArrayPointer(w.y);
  sunrealtype *sp = N_VGetArrayPointer(w.yS[0]);
  for (std::size_t i = 0; i < neq; ++i) {
    etr::Dual yi = y0.get(i);
    etr::ass<"cvode: NA in initial state">(!yi.isNA());
    yp[i] = yi.val;
    sp[i] = yi.dot;
  }

  const bool stiff = etr::get_val(opt.stiff);
  w.mem = CVodeCreate(stiff ? CV_BDF : CV_ADAMS, w.ctx);
  etr::ass<"cvode: CVodeCreate failed">(w.mem != nullptr);

  double t0 = etr::get_val(times.get(0));
  etr::ass<"cvode: CVodeInit failed">(CVodeInit(w.mem, &dual_rhs_trampoline<Ctx>, t0, w.y) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSStolerances failed">(CVodeSStolerances(w.mem, etr::get_val(opt.reltol), etr::get_val(opt.abstol)) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetUserData failed">(CVodeSetUserData(w.mem, &ctx) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetMaxNumSteps failed">(CVodeSetMaxNumSteps(w.mem, max_steps) == CV_SUCCESS);

  if (stiff) {
    w.A = SUNDenseMatrix(static_cast<sunindextype>(neq),
                         static_cast<sunindextype>(neq), w.ctx);
    etr::ass<"cvode: SUNDenseMatrix failed">(w.A != nullptr);
    w.LS = dense_solver(linear_solver, w.y, w.A, w.ctx);
    etr::ass<"cvode: creating the dense linear solver failed">(w.LS != nullptr);
    etr::ass<"cvode: CVodeSetLinearSolver failed">(
      CVodeSetLinearSolver(w.mem, w.LS, w.A) == CV_SUCCESS);
    if constexpr (!std::is_same_v<JAC, no_jac>) {
      etr::ass<"cvode: CVodeSetJacFn failed">(
        CVodeSetJacFn(w.mem, &dual_jac_trampoline<Ctx>) == CV_SUCCESS);
    }
  } else {
    w.NLS = SUNNonlinSol_FixedPoint(w.y, 0, w.ctx);
    etr::ass<"cvode: creating the fixed-point solver failed">(w.NLS != nullptr);
    etr::ass<"cvode: CVodeSetNonlinearSolver failed">(
      CVodeSetNonlinearSolver(w.mem, w.NLS) == CV_SUCCESS);
  }

  etr::ass<"cvode: CVodeSensInit failed">(
    CVodeSensInit(w.mem, 1, CV_STAGGERED, &sens_rhs_trampoline<Ctx>, w.yS) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSensEEtolerances failed">(CVodeSensEEtolerances(w.mem) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetSensErrCon failed">(CVodeSetSensErrCon(w.mem, SUNTRUE) == CV_SUCCESS);
  // the default sensitivity solver is Newton, which needs the linear solver of the stiff case
  if (!stiff) {
    w.NLS_sens = SUNNonlinSol_FixedPointSens(1, w.y, 0, w.ctx);
    etr::ass<"cvode: creating the fixed-point sensitivity solver failed">(w.NLS_sens != nullptr);
    etr::ass<"cvode: CVodeSetNonlinearSolverSensStg failed">(
      CVodeSetNonlinearSolverSensStg(w.mem, w.NLS_sens) == CV_SUCCESS);
  }

  etr::Array<etr::Dual, etr::Buffer<etr::Dual, etr::RBufferTrait>> res(etr::SI{neq * nt});
  res.dim = etr::Dim{nt, neq};
  for (std::size_t j = 0; j < neq; ++j) {
    res.set(j * nt + 0, etr::Dual(yp[j], sp[j]));
  }
  for (std::size_t k = 1; k < nt; ++k) {
    sunrealtype tret = 0.0;
    int flag = CVode(w.mem, etr::get_val(times.get(k)), w.y, &tret, CV_NORMAL);
    etr::ass(flag == CV_SUCCESS,
             std::string("cvode: integration failed at t = ") + std::to_string(tret) + ": " +
               flag_name(CVodeGetReturnFlagName(flag)));
    flag = CVodeGetSens(w.mem, &tret, w.yS);
    etr::ass(flag == CV_SUCCESS,
             std::string("cvode: CVodeGetSens failed: ") + flag_name(CVodeGetReturnFlagName(flag)));
    for (std::size_t j = 0; j < neq; ++j) {
      res.set(j * nt + k, etr::Dual(yp[j], sp[j]));
    }
  }
  if (etr::get_val(opt.stats)) print_cvode_stats(w.mem);
  return res;
}

// ---------- reverse mode (ReverseDouble): CVODES adjoint ---------------------
// cvode is one Custom block on the tape. Forward: solve with rhs_t<Double>,
// checkpoints via CVodeAdjInit. Backward (per reverse sweep):
//   lambda' = -J^T lambda, qB' = -f_p^T lambda, lambda += ybar(t_k) at each t_k
//   y0bar = lambda(t0), pbar = qB(t0)
// J^T lambda and f_p^T lambda: rhs_t<ReverseDouble> recorded on a fresh tape
// segment, swept with reverse_range, rewound.

// f_t<ReverseDouble> -> f_t<Double>
template <typename F, typename S> struct rebind_scalar { using type = F; };
template <template <typename> class F, typename T, typename S> struct rebind_scalar<F<T>, S> {
  using type = F<S>;
};

using RevBorrowVec = etr::Array<etr::ReverseDouble, etr::Borrow<etr::ReverseDouble>>;
using DoubleVec = etr::Array<etr::Double, etr::Buffer<etr::Double>>;
using RevVec = etr::Array<etr::ReverseDouble, etr::Buffer<etr::ReverseDouble>>;

template <typename RHS_R, typename JAC_R> struct cvode_adjoint_state {
  using RHS_D = typename rebind_scalar<RHS_R, etr::Double>::type;
  using JAC_D = typename rebind_scalar<JAC_R, etr::Double>::type;
  using Ctx = fct_ctx<RHS_D, DoubleVec, JAC_D>;

  cvode_workspace w;
  N_Vector yB = nullptr;
  N_Vector qB = nullptr;
  SUNMatrix AB = nullptr;
  SUNLinearSolver LSB = nullptr;
  SUNNonlinearSolver NLSB = nullptr;
  int which = -1;

  RHS_D rhs_d{};
  JAC_D jac_d{};
  RHS_R rhs_r{};
  DoubleVec p_double;
  Ctx ctx{};
  std::vector<double> p_vals, times;
  std::vector<int> y0_ids, p_ids;
  std::size_t neq = 0, nt = 0;
  bool stiff = true;
  double reltol = 0.0, abstol = 0.0;
  long int max_steps = 0;
  int linear_solver = 1;
  // vjp cache: fB and fQB are called with the same (t, y, lambda)
  double vjp_t = 0.0;
  std::vector<double> vjp_y, vjp_lambda, vjp_out_y, vjp_out_p;
  bool vjp_valid = false;
  std::vector<etr::ReverseDouble> vjp_y_leaves, vjp_ydot;
  std::vector<int> vjp_y_ids, vjp_p_ids;
  RevVec vjp_p_local;

  ~cvode_adjoint_state() {
    if (w.mem) CVodeFree(&w.mem);
    if (NLSB) SUNNonlinSolFree(NLSB);
    if (LSB) SUNLinSolFree(LSB);
    if (AB) SUNMatDestroy(AB);
    if (qB) N_VDestroy(qB);
    if (yB) N_VDestroy(yB);
  }

  // out_y = J^T lambda, out_p = f_p^T lambda at (t, y)
  void vjp(double t, const double *y, const double *lambda) {
    if (vjp_valid && t == vjp_t &&
        std::equal(y, y + neq, vjp_y.begin()) && std::equal(lambda, lambda + neq, vjp_lambda.begin())) {
      return;
    }
    auto &tape = etr::TAPE_INTERN_ref();
    const std::size_t start = tape.size();
    const std::size_t nblocks = tape.blocks.size();
    const std::size_t np = p_vals.size();
    // buffers reused: vjp runs per backward rhs call
    if (vjp_y_leaves.size() != neq) {
      vjp_y_leaves.resize(neq);
      vjp_ydot.resize(neq);
      vjp_y_ids.resize(neq);
      vjp_p_ids.resize(np);
      vjp_p_local = RevVec(etr::SI{np});
    }
    for (std::size_t i = 0; i < neq; ++i) {
      vjp_y_ids[i] = tape.push_var(y[i]);
      vjp_y_leaves[i] = etr::ReverseDouble(etr::from_tape_id_t{}, vjp_y_ids[i], 0);
      // stale ids would point into the new segment if rhs skips an entry
      vjp_ydot[i] = etr::ReverseDouble();
    }
    for (std::size_t i = 0; i < np; ++i) {
      vjp_p_ids[i] = tape.push_var(p_vals[i]);
      vjp_p_local.set(i, etr::ReverseDouble(etr::from_tape_id_t{}, vjp_p_ids[i], 0));
    }
    etr::ReverseDouble tt(t);
    RevBorrowVec yv(vjp_y_leaves.data(), neq, etr::Dim{neq});
    RevBorrowVec ydv(vjp_ydot.data(), neq, etr::Dim{neq});
    rhs_r(tt, yv, ydv, vjp_p_local);
    const std::size_t end = tape.size();
    std::fill(tape.adj.begin() + static_cast<std::ptrdiff_t>(start), tape.adj.end(), 0.0);
    for (std::size_t i = 0; i < neq; ++i) {
      const int id = vjp_ydot[i].id;
      if (id >= static_cast<int>(start)) tape.adj[static_cast<std::size_t>(id)] += lambda[i];
    }
    tape.reverse_range(end, start);
    for (std::size_t i = 0; i < neq; ++i) vjp_out_y[i] = tape.adj[static_cast<std::size_t>(vjp_y_ids[i])];
    for (std::size_t i = 0; i < np; ++i) vjp_out_p[i] = tape.adj[static_cast<std::size_t>(vjp_p_ids[i])];
    tape.rewind(start, nblocks);
    vjp_t = t;
    std::copy(y, y + neq, vjp_y.begin());
    std::copy(lambda, lambda + neq, vjp_lambda.begin());
    vjp_valid = true;
  }

  static int rhs_b(sunrealtype t, N_Vector y, N_Vector yB, N_Vector yBdot, void *user_data) {
    auto *s = static_cast<cvode_adjoint_state *>(user_data);
    s->vjp(t, N_VGetArrayPointer(y), N_VGetArrayPointer(yB));
    sunrealtype *out = N_VGetArrayPointer(yBdot);
    for (std::size_t i = 0; i < s->neq; ++i) out[i] = -s->vjp_out_y[i];
    return 0;
  }

  static int quad_b(sunrealtype t, N_Vector y, N_Vector yB, N_Vector qBdot, void *user_data) {
    auto *s = static_cast<cvode_adjoint_state *>(user_data);
    s->vjp(t, N_VGetArrayPointer(y), N_VGetArrayPointer(yB));
    sunrealtype *out = N_VGetArrayPointer(qBdot);
    for (std::size_t i = 0; i < s->p_vals.size(); ++i) out[i] = -s->vjp_out_p[i];
    return 0;
  }

  void init_backward() {
    const std::size_t np = p_vals.size();
    yB = N_VNew_Serial(static_cast<sunindextype>(neq), w.ctx);
    qB = N_VNew_Serial(static_cast<sunindextype>(np), w.ctx);
    etr::ass<"cvode (adjoint): N_VNew_Serial failed">(yB != nullptr && qB != nullptr);
    N_VConst(0.0, yB);
    N_VConst(0.0, qB);
    etr::ass<"cvode (adjoint): CVodeCreateB failed">(
      CVodeCreateB(w.mem, stiff ? CV_BDF : CV_ADAMS, &which) == CV_SUCCESS);
    etr::ass<"cvode (adjoint): CVodeInitB failed">(
      CVodeInitB(w.mem, which, &rhs_b, times[nt - 1], yB) == CV_SUCCESS);
    etr::ass<"cvode (adjoint): CVodeSStolerancesB failed">(
      CVodeSStolerancesB(w.mem, which, reltol, abstol) == CV_SUCCESS);
    etr::ass<"cvode (adjoint): CVodeSetUserDataB failed">(
      CVodeSetUserDataB(w.mem, which, this) == CV_SUCCESS);
    etr::ass<"cvode (adjoint): CVodeSetMaxNumStepsB failed">(
      CVodeSetMaxNumStepsB(w.mem, which, max_steps) == CV_SUCCESS);
    if (stiff) {
      // no jacB: difference quotients of rhs_b
      AB = SUNDenseMatrix(static_cast<sunindextype>(neq), static_cast<sunindextype>(neq), w.ctx);
      etr::ass<"cvode (adjoint): SUNDenseMatrix failed">(AB != nullptr);
      LSB = dense_solver(linear_solver, yB, AB, w.ctx);
      etr::ass<"cvode (adjoint): creating the dense linear solver failed">(LSB != nullptr);
      etr::ass<"cvode (adjoint): CVodeSetLinearSolverB failed">(
        CVodeSetLinearSolverB(w.mem, which, LSB, AB) == CV_SUCCESS);
    } else {
      NLSB = SUNNonlinSol_FixedPoint(yB, 0, w.ctx);
      etr::ass<"cvode (adjoint): creating the fixed-point solver failed">(NLSB != nullptr);
      etr::ass<"cvode (adjoint): CVodeSetNonlinearSolverB failed">(
        CVodeSetNonlinearSolverB(w.mem, which, NLSB) == CV_SUCCESS);
    }
    etr::ass<"cvode (adjoint): CVodeQuadInitB failed">(
      CVodeQuadInitB(w.mem, which, &quad_b, qB) == CV_SUCCESS);
    etr::ass<"cvode (adjoint): CVodeQuadSStolerancesB failed">(
      CVodeQuadSStolerancesB(w.mem, which, reltol, abstol) == CV_SUCCESS);
    etr::ass<"cvode (adjoint): CVodeSetQuadErrConB failed">(
      CVodeSetQuadErrConB(w.mem, which, SUNTRUE) == CV_SUCCESS);
  }

  // ybar: column-major nt x neq (adjoints of the cvode result)
  void backward(const std::vector<double> &ybar, std::vector<double> &y0bar,
                std::vector<double> &pbar) {
    const std::size_t np = p_vals.size();
    if (which < 0) init_backward();
    sunrealtype *lam = N_VGetArrayPointer(yB);
    sunrealtype *q = N_VGetArrayPointer(qB);
    for (std::size_t j = 0; j < neq; ++j) lam[j] = ybar[j * nt + (nt - 1)];
    N_VConst(0.0, qB);
    for (std::size_t k = nt - 1; k >= 1; --k) {
      vjp_valid = false;
      etr::ass<"cvode (adjoint): CVodeReInitB failed">(
        CVodeReInitB(w.mem, which, times[k], yB) == CV_SUCCESS);
      etr::ass<"cvode (adjoint): CVodeQuadReInitB failed">(
        CVodeQuadReInitB(w.mem, which, qB) == CV_SUCCESS);
      int flag = CVodeB(w.mem, times[k - 1], CV_NORMAL);
      etr::ass(flag >= 0, std::string("cvode (adjoint): backward integration failed at t = ") +
                            std::to_string(times[k]) + ": " +
                            flag_name(CVodeGetReturnFlagName(flag)));
      sunrealtype tret = 0.0;
      etr::ass<"cvode (adjoint): CVodeGetB failed">(CVodeGetB(w.mem, which, &tret, yB) == CV_SUCCESS);
      etr::ass<"cvode (adjoint): CVodeGetQuadB failed">(
        CVodeGetQuadB(w.mem, which, &tret, qB) == CV_SUCCESS);
      for (std::size_t j = 0; j < neq; ++j) lam[j] += ybar[j * nt + (k - 1)];
    }
    y0bar.assign(lam, lam + neq);
    pbar.assign(q, q + np);
  }

  // ---------- second order (HessianSweep), one direction d = (y0dot, pdot) ----------
  // tangent:   forward solve with one sensitivity s = dy/dd, rhs_t<Dual> (p carries pdot)
  // backward2: backward state [lambda; mu], quadrature [q; q2]
  //   lambda' = -f_y^T lambda
  //   mu'     = -f_y^T mu - (f_yy s + f_yp pdot)^T lambda
  //   q'      = -f_p^T lambda
  //   q2'     = -f_p^T mu - (f_py s + f_pp pdot)^T lambda
  //   jumps lambda += ybar(t_k), mu += ybar_dot(t_k)
  // both terms: one forward-over-reverse sweep of rhs_t<ReverseDouble> (vjp2)
  using RHS_Dual = typename rebind_scalar<RHS_R, etr::Dual>::type;
  using JAC_Dual = typename rebind_scalar<JAC_R, etr::Dual>::type;
  using DualVec = etr::Array<etr::Dual, etr::Buffer<etr::Dual>>;
  using DualCtx = dual_fct_ctx<RHS_Dual, DualVec, JAC_Dual>;

  struct second_order {
    cvodes_workspace w;
    N_Vector yB = nullptr;
    N_Vector qB = nullptr;
    SUNMatrix AB = nullptr;
    SUNLinearSolver LSB = nullptr;
    SUNNonlinearSolver NLSB = nullptr;
    int which = -1;
    RHS_Dual rhs{};
    JAC_Dual jac{};
    DualVec p_dual;
    DualCtx ctx{};
    std::vector<double> pdot;
    ~second_order() {
      if (w.mem) CVodeFree(&w.mem);
      if (NLSB) SUNNonlinSolFree(NLSB);
      if (LSB) SUNLinSolFree(LSB);
      if (AB) SUNMatDestroy(AB);
      if (qB) N_VDestroy(qB);
      if (yB) N_VDestroy(yB);
    }
  };
  std::unique_ptr<second_order> so;
  std::vector<double> y0_vals;

  // vjp2 cache, key = (t, y, s, lambda, mu)
  double vjp2_t = 0.0;
  std::vector<double> vjp2_key, vjp2_ay, vjp2_by, vjp2_ap, vjp2_bp;
  bool vjp2_valid = false;

  // a = f^T lambda, b = f^T mu + (f'' [s, pdot])^T lambda, split into y and p parts
  void vjp2(double t, const double *y, const double *s, const double *lambda, const double *mu) {
    const std::size_t np = p_vals.size();
    std::vector<double> key;
    key.reserve(4 * neq);
    key.insert(key.end(), y, y + neq);
    key.insert(key.end(), s, s + neq);
    key.insert(key.end(), lambda, lambda + neq);
    key.insert(key.end(), mu, mu + neq);
    if (vjp2_valid && t == vjp2_t && key == vjp2_key) return;
    auto &tape = etr::TAPE_INTERN_ref();
    const std::size_t start = tape.size();
    const std::size_t nblocks = tape.blocks.size();
    std::vector<etr::ReverseDouble> y_leaves(neq);
    std::vector<etr::ReverseDouble> ydot(neq);
    std::vector<int> x_ids(neq + np);
    for (std::size_t i = 0; i < neq; ++i) {
      x_ids[i] = tape.push_var(y[i]);
      y_leaves[i] = etr::ReverseDouble(etr::from_tape_id_t{}, x_ids[i], 0);
    }
    RevVec p_local(etr::SI{np});
    for (std::size_t i = 0; i < np; ++i) {
      x_ids[neq + i] = tape.push_var(p_vals[i]);
      p_local.set(i, etr::ReverseDouble(etr::from_tape_id_t{}, x_ids[neq + i], 0));
    }
    etr::ReverseDouble tt(t);
    RevBorrowVec yv(y_leaves.data(), neq, etr::Dim{neq});
    RevBorrowVec ydv(ydot.data(), neq, etr::Dim{neq});
    rhs_r(tt, yv, ydv, p_local);
    std::vector<int> outs(neq);
    for (std::size_t i = 0; i < neq; ++i) outs[i] = ydot[i].id;
    std::vector<double> dir(neq + np);
    std::copy(s, s + neq, dir.begin());
    std::copy(so->pdot.begin(), so->pdot.end(), dir.begin() + static_cast<std::ptrdiff_t>(neq));
    {
      etr::HessianSweep sweep(tape, start);
      sweep.forward(x_ids, dir.data());
      sweep.reverse_seeded(outs, lambda, mu);
      vjp2_ay.resize(neq);
      vjp2_by.resize(neq);
      vjp2_ap.resize(np);
      vjp2_bp.resize(np);
      for (std::size_t i = 0; i < neq; ++i) {
        const std::size_t k = static_cast<std::size_t>(x_ids[i]) - start;
        vjp2_ay[i] = sweep.adj[k];
        vjp2_by[i] = sweep.adj_dot[k];
      }
      for (std::size_t i = 0; i < np; ++i) {
        const std::size_t k = static_cast<std::size_t>(x_ids[neq + i]) - start;
        vjp2_ap[i] = sweep.adj[k];
        vjp2_bp[i] = sweep.adj_dot[k];
      }
    }
    tape.rewind(start, nblocks);
    vjp2_t = t;
    vjp2_key = std::move(key);
    vjp2_valid = true;
  }

  static int rhs_bs(sunrealtype t, N_Vector y, N_Vector *yS, N_Vector yB, N_Vector yBdot,
                    void *user_data) {
    auto *s = static_cast<cvode_adjoint_state *>(user_data);
    const sunrealtype *b = N_VGetArrayPointer(yB);
    s->vjp2(t, N_VGetArrayPointer(y), N_VGetArrayPointer(yS[0]), b, b + s->neq);
    sunrealtype *out = N_VGetArrayPointer(yBdot);
    for (std::size_t i = 0; i < s->neq; ++i) {
      out[i] = -s->vjp2_ay[i];
      out[s->neq + i] = -s->vjp2_by[i];
    }
    return 0;
  }

  static int quad_bs(sunrealtype t, N_Vector y, N_Vector *yS, N_Vector yB, N_Vector qBdot,
                     void *user_data) {
    auto *s = static_cast<cvode_adjoint_state *>(user_data);
    const sunrealtype *b = N_VGetArrayPointer(yB);
    s->vjp2(t, N_VGetArrayPointer(y), N_VGetArrayPointer(yS[0]), b, b + s->neq);
    const std::size_t np = s->p_vals.size();
    sunrealtype *out = N_VGetArrayPointer(qBdot);
    for (std::size_t i = 0; i < np; ++i) {
      out[i] = -s->vjp2_ap[i];
      out[np + i] = -s->vjp2_bp[i];
    }
    return 0;
  }

  // in_dot = (y0dot, pdot); out_dot = s(t_k), column-major nt x neq
  void tangent(const std::vector<double> &in_dot, std::vector<double> &out_dot) {
    const std::size_t np = p_vals.size();
    so = std::make_unique<second_order>();
    second_order &o = *so;
    o.pdot.assign(in_dot.begin() + static_cast<std::ptrdiff_t>(neq), in_dot.end());
    o.p_dual = DualVec(etr::SI{np});
    for (std::size_t i = 0; i < np; ++i) o.p_dual.set(i, etr::Dual(p_vals[i], o.pdot[i]));
    o.ctx.f = &o.rhs;
    o.ctx.params = &o.p_dual;
    if constexpr (!std::is_same_v<JAC_R, no_jac>) {
      o.ctx.jac = &o.jac;
      o.ctx.jac_dot.assign(neq * neq, 0.0);
    }
    o.ctx.zero_dot.assign(neq, 0.0);
    o.ctx.sink_dot.assign(neq, 0.0);
    o.ctx.sink_val.assign(neq, 0.0);

    cvodes_workspace &w = o.w;
    etr::ass<"hessian (cvode): SUNContext_Create failed">(SUNContext_Create(SUN_COMM_NULL, &w.ctx) == 0);
    SUNContext_ClearErrHandlers(w.ctx);
    w.y = N_VNew_Serial(static_cast<sunindextype>(neq), w.ctx);
    etr::ass<"hessian (cvode): N_VNew_Serial failed">(w.y != nullptr);
    w.yS = N_VCloneVectorArray(1, w.y);
    etr::ass<"hessian (cvode): N_VCloneVectorArray failed">(w.yS != nullptr);
    sunrealtype *yp = N_VGetArrayPointer(w.y);
    sunrealtype *sp = N_VGetArrayPointer(w.yS[0]);
    for (std::size_t i = 0; i < neq; ++i) {
      yp[i] = y0_vals[i];
      sp[i] = in_dot[i];
    }
    w.mem = CVodeCreate(stiff ? CV_BDF : CV_ADAMS, w.ctx);
    etr::ass<"hessian (cvode): CVodeCreate failed">(w.mem != nullptr);
    etr::ass<"hessian (cvode): CVodeInit failed">(
      CVodeInit(w.mem, &dual_rhs_trampoline<DualCtx>, times[0], w.y) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeSStolerances failed">(CVodeSStolerances(w.mem, reltol, abstol) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeSetUserData failed">(CVodeSetUserData(w.mem, &o.ctx) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeSetMaxNumSteps failed">(CVodeSetMaxNumSteps(w.mem, max_steps) == CV_SUCCESS);
    if (stiff) {
      w.A = SUNDenseMatrix(static_cast<sunindextype>(neq), static_cast<sunindextype>(neq), w.ctx);
      etr::ass<"hessian (cvode): SUNDenseMatrix failed">(w.A != nullptr);
      w.LS = dense_solver(linear_solver, w.y, w.A, w.ctx);
      etr::ass<"hessian (cvode): creating the dense linear solver failed">(w.LS != nullptr);
      etr::ass<"hessian (cvode): CVodeSetLinearSolver failed">(CVodeSetLinearSolver(w.mem, w.LS, w.A) == CV_SUCCESS);
      if constexpr (!std::is_same_v<JAC_R, no_jac>) {
        etr::ass<"hessian (cvode): CVodeSetJacFn failed">(
          CVodeSetJacFn(w.mem, &dual_jac_trampoline<DualCtx>) == CV_SUCCESS);
      }
    } else {
      w.NLS = SUNNonlinSol_FixedPoint(w.y, 0, w.ctx);
      etr::ass<"hessian (cvode): creating the fixed-point solver failed">(w.NLS != nullptr);
      etr::ass<"hessian (cvode): CVodeSetNonlinearSolver failed">(CVodeSetNonlinearSolver(w.mem, w.NLS) == CV_SUCCESS);
    }
    etr::ass<"hessian (cvode): CVodeSensInit failed">(
      CVodeSensInit(w.mem, 1, CV_STAGGERED, &sens_rhs_trampoline<DualCtx>, w.yS) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeSensEEtolerances failed">(CVodeSensEEtolerances(w.mem) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeSetSensErrCon failed">(CVodeSetSensErrCon(w.mem, SUNTRUE) == CV_SUCCESS);
    if (!stiff) {
      w.NLS_sens = SUNNonlinSol_FixedPointSens(1, w.y, 0, w.ctx);
      etr::ass<"hessian (cvode): creating the fixed-point sensitivity solver failed">(w.NLS_sens != nullptr);
      etr::ass<"hessian (cvode): CVodeSetNonlinearSolverSensStg failed">(
        CVodeSetNonlinearSolverSensStg(w.mem, w.NLS_sens) == CV_SUCCESS);
    }
    etr::ass<"hessian (cvode): CVodeAdjInit failed">(CVodeAdjInit(w.mem, 100, CV_HERMITE) == CV_SUCCESS);

    out_dot.assign(neq * nt, 0.0);
    for (std::size_t j = 0; j < neq; ++j) out_dot[j * nt] = sp[j];
    for (std::size_t k = 1; k < nt; ++k) {
      sunrealtype tret = 0.0;
      int ncheck = 0;
      int flag = CVodeF(w.mem, times[k], w.y, &tret, CV_NORMAL, &ncheck);
      etr::ass(flag == CV_SUCCESS, std::string("hessian (cvode): integration failed at t = ") +
                                     std::to_string(tret) + ": " +
                                     flag_name(CVodeGetReturnFlagName(flag)));
      flag = CVodeGetSens(w.mem, &tret, w.yS);
      etr::ass(flag == CV_SUCCESS, std::string("hessian (cvode): CVodeGetSens failed: ") +
                                     flag_name(CVodeGetReturnFlagName(flag)));
      for (std::size_t j = 0; j < neq; ++j) out_dot[j * nt + k] = sp[j];
    }
  }

  void init_backward2(second_order &o) {
    const std::size_t np = p_vals.size();
    const std::size_t nb = 2 * neq;
    cvodes_workspace &w = o.w;
    o.yB = N_VNew_Serial(static_cast<sunindextype>(nb), w.ctx);
    o.qB = N_VNew_Serial(static_cast<sunindextype>(2 * np), w.ctx);
    etr::ass<"hessian (cvode): N_VNew_Serial failed">(o.yB != nullptr && o.qB != nullptr);
    N_VConst(0.0, o.yB);
    N_VConst(0.0, o.qB);
    etr::ass<"hessian (cvode): CVodeCreateB failed">(
      CVodeCreateB(w.mem, stiff ? CV_BDF : CV_ADAMS, &o.which) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeInitBS failed">(
      CVodeInitBS(w.mem, o.which, &rhs_bs, times[nt - 1], o.yB) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeSStolerancesB failed">(
      CVodeSStolerancesB(w.mem, o.which, reltol, abstol) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeSetUserDataB failed">(
      CVodeSetUserDataB(w.mem, o.which, this) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeSetMaxNumStepsB failed">(
      CVodeSetMaxNumStepsB(w.mem, o.which, max_steps) == CV_SUCCESS);
    if (stiff) {
      o.AB = SUNDenseMatrix(static_cast<sunindextype>(nb), static_cast<sunindextype>(nb), w.ctx);
      etr::ass<"hessian (cvode): SUNDenseMatrix failed">(o.AB != nullptr);
      o.LSB = dense_solver(linear_solver, o.yB, o.AB, w.ctx);
      etr::ass<"hessian (cvode): creating the dense linear solver failed">(o.LSB != nullptr);
      etr::ass<"hessian (cvode): CVodeSetLinearSolverB failed">(
        CVodeSetLinearSolverB(w.mem, o.which, o.LSB, o.AB) == CV_SUCCESS);
    } else {
      o.NLSB = SUNNonlinSol_FixedPoint(o.yB, 0, w.ctx);
      etr::ass<"hessian (cvode): creating the fixed-point solver failed">(o.NLSB != nullptr);
      etr::ass<"hessian (cvode): CVodeSetNonlinearSolverB failed">(
        CVodeSetNonlinearSolverB(w.mem, o.which, o.NLSB) == CV_SUCCESS);
    }
    etr::ass<"hessian (cvode): CVodeQuadInitBS failed">(
      CVodeQuadInitBS(w.mem, o.which, &quad_bs, o.qB) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeQuadSStolerancesB failed">(
      CVodeQuadSStolerancesB(w.mem, o.which, reltol, abstol) == CV_SUCCESS);
    etr::ass<"hessian (cvode): CVodeSetQuadErrConB failed">(
      CVodeSetQuadErrConB(w.mem, o.which, SUNTRUE) == CV_SUCCESS);
  }

  // out_bar / out_bar_dot: column-major nt x neq; in_* = (y0 part, p part)
  void backward2(const std::vector<double> &out_bar, const std::vector<double> &out_bar_dot,
                 std::vector<double> &in_bar, std::vector<double> &in_bar_dot) {
    etr::ass<"hessian (cvode): backward pass without a tangent pass">(so != nullptr);
    second_order &o = *so;
    const std::size_t np = p_vals.size();
    if (o.which < 0) init_backward2(o);
    sunrealtype *b = N_VGetArrayPointer(o.yB);
    sunrealtype *q = N_VGetArrayPointer(o.qB);
    for (std::size_t j = 0; j < neq; ++j) {
      b[j] = out_bar[j * nt + (nt - 1)];
      b[neq + j] = out_bar_dot[j * nt + (nt - 1)];
    }
    N_VConst(0.0, o.qB);
    for (std::size_t k = nt - 1; k >= 1; --k) {
      vjp2_valid = false;
      etr::ass<"hessian (cvode): CVodeReInitB failed">(
        CVodeReInitB(o.w.mem, o.which, times[k], o.yB) == CV_SUCCESS);
      etr::ass<"hessian (cvode): CVodeQuadReInitB failed">(
        CVodeQuadReInitB(o.w.mem, o.which, o.qB) == CV_SUCCESS);
      int flag = CVodeB(o.w.mem, times[k - 1], CV_NORMAL);
      etr::ass(flag >= 0, std::string("hessian (cvode): backward integration failed at t = ") +
                            std::to_string(times[k]) + ": " +
                            flag_name(CVodeGetReturnFlagName(flag)));
      sunrealtype tret = 0.0;
      etr::ass<"hessian (cvode): CVodeGetB failed">(CVodeGetB(o.w.mem, o.which, &tret, o.yB) == CV_SUCCESS);
      etr::ass<"hessian (cvode): CVodeGetQuadB failed">(
        CVodeGetQuadB(o.w.mem, o.which, &tret, o.qB) == CV_SUCCESS);
      for (std::size_t j = 0; j < neq; ++j) {
        b[j] += out_bar[j * nt + (k - 1)];
        b[neq + j] += out_bar_dot[j * nt + (k - 1)];
      }
    }
    for (std::size_t i = 0; i < neq; ++i) {
      in_bar[i] = b[i];
      in_bar_dot[i] = b[neq + i];
    }
    for (std::size_t i = 0; i < np; ++i) {
      in_bar[neq + i] = q[i];
      in_bar_dot[neq + i] = q[np + i];
    }
  }
};

template <typename JAC, typename RHS, typename Y, typename T, typename P>
requires(etr::IsArray<etr::Decayed<Y>> && etr::IsArray<etr::Decayed<T>>)
inline auto cvodes_reverse_impl(const RHS &, const JAC *, const Y &y0, const T &times,
                                const P &params, const etr::cvode_options &opt) {
  static_assert(etr::IsArray<etr::Decayed<P>>,
                "cvode: in reverse mode params has to be a double vector");
  using State = cvode_adjoint_state<RHS, JAC>;
  const std::size_t neq = y0.size();
  const std::size_t nt = times.size();
  const std::size_t np = params.size();
  etr::ass<"cvode: need at least one initial state">(neq > 0);
  etr::ass<"cvode: need at least two time points">(nt >= 2);
  etr::ass<"cvode: in reverse mode params needs at least one element">(np > 0);
  check_tolerances(opt, "cvode");
  etr::ass<"cvode: stiff must be TRUE or FALSE">(!opt.stiff.isNA());
  etr::ass<"cvode: stats must be TRUE or FALSE">(!opt.stats.isNA());

  auto s = std::make_shared<State>();
  s->neq = neq;
  s->nt = nt;
  s->max_steps = checked_max_steps(opt, "cvode");
  s->linear_solver = checked_linear_solver(opt, "cvode");
  s->stiff = etr::get_val(opt.stiff);
  s->reltol = etr::get_val(opt.reltol);
  s->abstol = etr::get_val(opt.abstol);
  s->times.resize(nt);
  for (std::size_t k = 0; k < nt; ++k) s->times[k] = etr::get_val(times.get(k));
  s->p_vals.resize(np);
  s->p_ids.resize(np);
  s->p_double = DoubleVec(etr::SI{np});
  for (std::size_t i = 0; i < np; ++i) {
    etr::ReverseDouble pi = params.get(i);
    etr::ass<"cvode: NA in params">(!pi.isNA());
    s->p_vals[i] = etr::get_val(pi);
    s->p_ids[i] = pi.id;
    s->p_double.set(i, etr::Double(s->p_vals[i]));
  }
  s->vjp_y.resize(neq);
  s->vjp_lambda.resize(neq);
  s->vjp_out_y.resize(neq);
  s->vjp_out_p.resize(np);
  s->ctx.f = &s->rhs_d;
  s->ctx.params = &s->p_double;
  if constexpr (!std::is_same_v<JAC, no_jac>) s->ctx.jac = &s->jac_d;

  cvode_workspace &w = s->w;
  etr::ass<"cvode: SUNContext_Create failed">(SUNContext_Create(SUN_COMM_NULL, &w.ctx) == 0);
  SUNContext_ClearErrHandlers(w.ctx);
  w.y = N_VNew_Serial(static_cast<sunindextype>(neq), w.ctx);
  etr::ass<"cvode: N_VNew_Serial failed">(w.y != nullptr);
  sunrealtype *yp = N_VGetArrayPointer(w.y);
  s->y0_ids.resize(neq);
  s->y0_vals.resize(neq);
  for (std::size_t i = 0; i < neq; ++i) {
    etr::ReverseDouble yi = y0.get(i);
    etr::ass<"cvode: NA in initial state">(!yi.isNA());
    yp[i] = etr::get_val(yi);
    s->y0_vals[i] = yp[i];
    s->y0_ids[i] = yi.id;
  }

  w.mem = CVodeCreate(s->stiff ? CV_BDF : CV_ADAMS, w.ctx);
  etr::ass<"cvode: CVodeCreate failed">(w.mem != nullptr);
  using Ctx = typename State::Ctx;
  etr::ass<"cvode: CVodeInit failed">(CVodeInit(w.mem, &rhs_trampoline<Ctx>, s->times[0], w.y) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSStolerances failed">(CVodeSStolerances(w.mem, s->reltol, s->abstol) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetUserData failed">(CVodeSetUserData(w.mem, &s->ctx) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetMaxNumSteps failed">(CVodeSetMaxNumSteps(w.mem, s->max_steps) == CV_SUCCESS);
  if (s->stiff) {
    w.A = SUNDenseMatrix(static_cast<sunindextype>(neq), static_cast<sunindextype>(neq), w.ctx);
    etr::ass<"cvode: SUNDenseMatrix failed">(w.A != nullptr);
    w.LS = dense_solver(s->linear_solver, w.y, w.A, w.ctx);
    etr::ass<"cvode: creating the dense linear solver failed">(w.LS != nullptr);
    etr::ass<"cvode: CVodeSetLinearSolver failed">(CVodeSetLinearSolver(w.mem, w.LS, w.A) == CV_SUCCESS);
    if constexpr (!std::is_same_v<JAC, no_jac>) {
      etr::ass<"cvode: CVodeSetJacFn failed">(
        CVodeSetJacFn(w.mem, &cvode_jac_trampoline<Ctx>) == CV_SUCCESS);
    }
  } else {
    w.NLS = SUNNonlinSol_FixedPoint(w.y, 0, w.ctx);
    etr::ass<"cvode: creating the fixed-point solver failed">(w.NLS != nullptr);
    etr::ass<"cvode: CVodeSetNonlinearSolver failed">(CVodeSetNonlinearSolver(w.mem, w.NLS) == CV_SUCCESS);
  }
  // checkpoint every 100 steps, Hermite interpolation of the forward solution
  etr::ass<"cvode: CVodeAdjInit failed">(CVodeAdjInit(w.mem, 100, CV_HERMITE) == CV_SUCCESS);

  std::vector<double> vals(neq * nt);
  std::vector<uint8_t> na(neq * nt, 0);
  for (std::size_t j = 0; j < neq; ++j) vals[j * nt] = yp[j];
  for (std::size_t k = 1; k < nt; ++k) {
    sunrealtype tret = 0.0;
    int ncheck = 0;
    int flag = CVodeF(w.mem, s->times[k], w.y, &tret, CV_NORMAL, &ncheck);
    etr::ass(flag == CV_SUCCESS,
             std::string("cvode: integration failed at t = ") + std::to_string(tret) + ": " +
               flag_name(CVodeGetReturnFlagName(flag)));
    for (std::size_t j = 0; j < neq; ++j) vals[j * nt + k] = yp[j];
  }
  if (etr::get_val(opt.stats)) print_cvode_stats(w.mem);

  std::vector<int> input_ids = s->y0_ids;
  input_ids.insert(input_ids.end(), s->p_ids.begin(), s->p_ids.end());
  const int head = etr::TAPE_INTERN_ref().push_custom(vals, na, std::move(input_ids),
    [s](etr::ReverseTape &tape, int out0) {
      const std::size_t n_out = s->neq * s->nt;
      std::vector<double> ybar(n_out);
      bool any = false;
      for (std::size_t i = 0; i < n_out; ++i) {
        const std::size_t id = static_cast<std::size_t>(out0) + i;
        ybar[i] = tape.is_na[id] ? 0.0 : tape.adj[id];
        if (ybar[i] != 0.0) any = true;
      }
      if (!any) return;
      std::vector<double> y0bar, pbar;
      s->backward(ybar, y0bar, pbar);
      for (std::size_t i = 0; i < s->neq; ++i) {
        tape.adj[static_cast<std::size_t>(s->y0_ids[i])] += y0bar[i];
      }
      for (std::size_t i = 0; i < pbar.size(); ++i) {
        tape.adj[static_cast<std::size_t>(s->p_ids[i])] += pbar[i];
      }
    },
    [s](const std::vector<double> &in_dot, std::vector<double> &out_dot) {
      s->tangent(in_dot, out_dot);
    },
    [s](const std::vector<double> &out_bar, const std::vector<double> &out_bar_dot,
        std::vector<double> &in_bar, std::vector<double> &in_bar_dot) {
      s->backward2(out_bar, out_bar_dot, in_bar, in_bar_dot);
    });

  etr::Array<etr::ReverseDouble, etr::Buffer<etr::ReverseDouble, etr::LBufferTrait>> res(etr::SI{neq * nt});
  res.dim = etr::Dim{nt, neq};
  for (std::size_t i = 0; i < neq * nt; ++i) {
    res.set(i, etr::ReverseDouble(etr::from_tape_id_t{}, head + static_cast<int>(i), 0));
  }
  return res;
}

template <typename JAC, typename RHS, typename Y, typename T, typename P>
inline auto cvode_dispatch(const RHS &rhs, const JAC *jac, const Y &y0, const T &times,
                           const P &params, const etr::cvode_options &opt) {
  using TY = typename etr::ExtractDataType<etr::Decayed<Y>>::value_type;
  if constexpr (etr::IS<TY, etr::ReverseDouble>) {
    return cvodes_reverse_impl(rhs, jac, y0, times, params, opt);
  } else if constexpr (etr::IS<TY, etr::Dual>) {
    return cvodes_forward_impl(rhs, jac, y0, times, params, opt);
  } else {
    return cvode_impl(rhs, jac, y0, times, params, opt);
  }
}

template <typename RHS, typename Y, typename T, typename P>
inline auto cvode_solve(const RHS &rhs, const Y &y0, const T &times, const P &params,
                        const etr::cvode_options &opt) {
  return cvode_dispatch<no_jac>(rhs, nullptr, y0, times, params, opt);
}

template <typename RHS, typename Y, typename T, typename P, typename JAC>
inline auto cvode_solve(const RHS &rhs, const Y &y0, const T &times, const P &params,
                        const etr::cvode_options &opt, const JAC &jac) {
  return cvode_dispatch(rhs, &jac, y0, times, params, opt);
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
  const int linear_solver = checked_linear_solver(opt, "ida");

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
  w.LS = dense_solver(linear_solver, w.y, w.A, w.ctx);
  etr::ass<"ida: creating the dense linear solver failed">(w.LS != nullptr);
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
