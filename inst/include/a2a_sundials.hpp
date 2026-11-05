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

// sensitivity (gradient through cvode): codes as sensitivity_modes in FunctionRegistry.R
inline constexpr int sensitivity_auto = 0;
inline constexpr int sensitivity_forward = 1;
inline constexpr int sensitivity_adjoint = 2;
// auto: forward sensitivities up to this many active inputs, adjoint above
inline constexpr std::size_t forward_sens_max_inputs = 20;
inline const char *sensitivity_name(const Integer &code) {
  if (code.isNA()) return "NA";
  const int c = get_val(code);
  return c == sensitivity_auto ? "auto" : c == sensitivity_forward ? "forward" :
         c == sensitivity_adjoint ? "adjoint" : "unknown";
}

struct cvode_options {
  Double reltol;
  Double abstol;
  Logical stiff;
  Integer max_steps;
  Logical stats;
  Integer linear_solver;
  Integer sensitivity;
  cvode_options(Double reltol_, Double abstol_, Logical stiff_, Integer max_steps_, Logical stats_,
                Integer linear_solver_, Integer sensitivity_)
    : reltol(reltol_), abstol(abstol_), stiff(stiff_), max_steps(max_steps_), stats(stats_),
      linear_solver(linear_solver_), sensitivity(sensitivity_) {}
#ifndef STANDALONE_ETR
  explicit cvode_options(SEXP arg) :
    reltol(SEXP2Scalar<Double>(checked_elt(arg, "reltol", 7, "cvode_options"))),
    abstol(SEXP2Scalar<Double>(checked_elt(arg, "abstol", 7, "cvode_options"))),
    stiff(SEXP2Scalar<Logical>(checked_elt(arg, "stiff", 7, "cvode_options"))),
    max_steps(SEXP2Scalar<Integer>(checked_elt(arg, "max_steps", 7, "cvode_options"))),
    stats(SEXP2Scalar<Logical>(checked_elt(arg, "stats", 7, "cvode_options"))),
    linear_solver(SEXP2Scalar<Integer>(checked_elt(arg, "linear_solver", 7, "cvode_options"))),
    sensitivity(SEXP2Scalar<Integer>(checked_elt(arg, "sensitivity", 7, "cvode_options"))) {}
  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 7));
    SET_VECTOR_ELT(res, 0, Cast(reltol));
    SET_VECTOR_ELT(res, 1, Cast(abstol));
    SET_VECTOR_ELT(res, 2, Cast(stiff));
    SET_VECTOR_ELT(res, 3, Cast(max_steps));
    SET_VECTOR_ELT(res, 4, Cast(stats));
    SET_VECTOR_ELT(res, 5, Cast(linear_solver));
    SET_VECTOR_ELT(res, 6, Cast(sensitivity));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 7));
    SET_STRING_ELT(names, 0, Rf_mkChar("reltol"));
    SET_STRING_ELT(names, 1, Rf_mkChar("abstol"));
    SET_STRING_ELT(names, 2, Rf_mkChar("stiff"));
    SET_STRING_ELT(names, 3, Rf_mkChar("max_steps"));
    SET_STRING_ELT(names, 4, Rf_mkChar("stats"));
    SET_STRING_ELT(names, 5, Rf_mkChar("linear_solver"));
    SET_STRING_ELT(names, 6, Rf_mkChar("sensitivity"));
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
  PRINT_STREAM << "  sensitivity: " << sensitivity_name(x.sensitivity) << std::endl;
}

struct ida_options {
  Double reltol;
  Double abstol;
  Integer max_steps;
  Logical stats;
  Integer linear_solver;
  Integer sensitivity;
  ida_options(Double reltol_, Double abstol_, Integer max_steps_, Logical stats_,
              Integer linear_solver_, Integer sensitivity_)
    : reltol(reltol_), abstol(abstol_), max_steps(max_steps_), stats(stats_),
      linear_solver(linear_solver_), sensitivity(sensitivity_) {}
#ifndef STANDALONE_ETR
  explicit ida_options(SEXP arg) :
    reltol(SEXP2Scalar<Double>(checked_elt(arg, "reltol", 6, "ida_options"))),
    abstol(SEXP2Scalar<Double>(checked_elt(arg, "abstol", 6, "ida_options"))),
    max_steps(SEXP2Scalar<Integer>(checked_elt(arg, "max_steps", 6, "ida_options"))),
    stats(SEXP2Scalar<Logical>(checked_elt(arg, "stats", 6, "ida_options"))),
    linear_solver(SEXP2Scalar<Integer>(checked_elt(arg, "linear_solver", 6, "ida_options"))),
    sensitivity(SEXP2Scalar<Integer>(checked_elt(arg, "sensitivity", 6, "ida_options"))) {}
  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, 6));
    SET_VECTOR_ELT(res, 0, Cast(reltol));
    SET_VECTOR_ELT(res, 1, Cast(abstol));
    SET_VECTOR_ELT(res, 2, Cast(max_steps));
    SET_VECTOR_ELT(res, 3, Cast(stats));
    SET_VECTOR_ELT(res, 4, Cast(linear_solver));
    SET_VECTOR_ELT(res, 5, Cast(sensitivity));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, Rf_mkChar("reltol"));
    SET_STRING_ELT(names, 1, Rf_mkChar("abstol"));
    SET_STRING_ELT(names, 2, Rf_mkChar("max_steps"));
    SET_STRING_ELT(names, 3, Rf_mkChar("stats"));
    SET_STRING_ELT(names, 4, Rf_mkChar("linear_solver"));
    SET_STRING_ELT(names, 5, Rf_mkChar("sensitivity"));
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
  PRINT_STREAM << "  sensitivity: " << sensitivity_name(x.sensitivity) << std::endl;
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
  // TODO: why the hell is Rprintf used and not etr::print?
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
requires(etr::IsArray<etr::Decayed<Y>> && etr::IsArray<etr::Decayed<T>>)
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
  int ns = 1;
  ~cvodes_workspace() {
    if (mem) CVodeFree(&mem);
    if (NLS_sens) SUNNonlinSolFree(NLS_sens);
    if (NLS) SUNNonlinSolFree(NLS);
    if (LS) SUNLinSolFree(LS);
    if (A) SUNMatDestroy(A);
    if (yS) N_VDestroyVectorArray(yS, ns);
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

  // ---------- first order via forward sensitivities (small number of inputs) ----------
  // one CVODES solve: y with rhs_t<Double>, all Ns sensitivities with rhs_t<Dual>
  // (fS calls rhs_t<Dual> once per direction, p dot = direction).
  // Backward: input_bar_i = sum_k S_i(t_k)^T ybar(t_k), no backward solve, no tape.
  cvodes_workspace wf;
  std::unique_ptr<DualCtx> fs_ctx;
  RHS_Dual fs_rhs{};
  JAC_Dual fs_jac{};
  DualVec fs_p_dual;
  std::vector<int> active;   // input index (0..neq-1: y0, neq..: p)
  std::vector<double> sens;  // S_i(t_k)[j] at (k * Ns + i) * neq + j

  static int fwd_rhs(sunrealtype t, N_Vector y, N_Vector ydot, void *user_data) {
    return rhs_trampoline<Ctx>(t, y, ydot, &static_cast<cvode_adjoint_state *>(user_data)->ctx);
  }
  static int fwd_jac(sunrealtype t, N_Vector y, N_Vector fy, SUNMatrix J, void *user_data,
                     N_Vector tmp1, N_Vector tmp2, N_Vector tmp3) {
    return cvode_jac_trampoline<Ctx>(t, y, fy, J, &static_cast<cvode_adjoint_state *>(user_data)->ctx,
                                     tmp1, tmp2, tmp3);
  }
  static int fwd_sens(int Ns, sunrealtype t, N_Vector y, N_Vector ydot, N_Vector *yS,
                      N_Vector *ySdot, void *user_data, N_Vector tmp1, N_Vector tmp2) {
    auto *s = static_cast<cvode_adjoint_state *>(user_data);
    const std::size_t np = s->p_vals.size();
    for (int i = 0; i < Ns; ++i) {
      const int a = s->active[static_cast<std::size_t>(i)];
      for (std::size_t m = 0; m < np; ++m) {
        const double d = a == static_cast<int>(s->neq + m) ? 1.0 : 0.0;
        s->fs_p_dual.set(m, etr::Dual(s->p_vals[m], d));
      }
      const int flag = sens_rhs_trampoline<DualCtx>(1, t, y, ydot, &yS[i], &ySdot[i], s->fs_ctx.get(),
                                                    tmp1, tmp2);
      if (flag != 0) return flag;
    }
    return 0;
  }

  // vals: column-major nt x neq (the cvode result)
  void solve_forward_sens(std::vector<double> &vals) {
    const std::size_t np = p_vals.size();
    const int ns = static_cast<int>(active.size());
    fs_p_dual = DualVec(etr::SI{np});
    for (std::size_t m = 0; m < np; ++m) fs_p_dual.set(m, etr::Dual(p_vals[m], 0.0));
    fs_ctx = std::make_unique<DualCtx>();
    fs_ctx->f = &fs_rhs;
    fs_ctx->params = &fs_p_dual;
    fs_ctx->zero_dot.assign(neq, 0.0);
    fs_ctx->sink_dot.assign(neq, 0.0);
    fs_ctx->sink_val.assign(neq, 0.0);

    cvodes_workspace &v = wf;
    etr::ass<"cvode: SUNContext_Create failed">(SUNContext_Create(SUN_COMM_NULL, &v.ctx) == 0);
    SUNContext_ClearErrHandlers(v.ctx);
    v.y = N_VNew_Serial(static_cast<sunindextype>(neq), v.ctx);
    etr::ass<"cvode: N_VNew_Serial failed">(v.y != nullptr);
    v.yS = N_VCloneVectorArray(ns, v.y);
    etr::ass<"cvode: N_VCloneVectorArray failed">(v.yS != nullptr);
    v.ns = ns;
    sunrealtype *yp = N_VGetArrayPointer(v.y);
    for (std::size_t j = 0; j < neq; ++j) yp[j] = y0_vals[j];
    for (int i = 0; i < ns; ++i) {
      N_VConst(0.0, v.yS[i]);
      const int a = active[static_cast<std::size_t>(i)];
      if (a < static_cast<int>(neq)) N_VGetArrayPointer(v.yS[i])[a] = 1.0;
    }
    v.mem = CVodeCreate(stiff ? CV_BDF : CV_ADAMS, v.ctx);
    etr::ass<"cvode: CVodeCreate failed">(v.mem != nullptr);
    etr::ass<"cvode: CVodeInit failed">(CVodeInit(v.mem, &fwd_rhs, times[0], v.y) == CV_SUCCESS);
    etr::ass<"cvode: CVodeSStolerances failed">(CVodeSStolerances(v.mem, reltol, abstol) == CV_SUCCESS);
    etr::ass<"cvode: CVodeSetUserData failed">(CVodeSetUserData(v.mem, this) == CV_SUCCESS);
    etr::ass<"cvode: CVodeSetMaxNumSteps failed">(CVodeSetMaxNumSteps(v.mem, max_steps) == CV_SUCCESS);
    if (stiff) {
      v.A = SUNDenseMatrix(static_cast<sunindextype>(neq), static_cast<sunindextype>(neq), v.ctx);
      etr::ass<"cvode: SUNDenseMatrix failed">(v.A != nullptr);
      v.LS = dense_solver(linear_solver, v.y, v.A, v.ctx);
      etr::ass<"cvode: creating the dense linear solver failed">(v.LS != nullptr);
      etr::ass<"cvode: CVodeSetLinearSolver failed">(CVodeSetLinearSolver(v.mem, v.LS, v.A) == CV_SUCCESS);
      if constexpr (!std::is_same_v<JAC_R, no_jac>) {
        etr::ass<"cvode: CVodeSetJacFn failed">(CVodeSetJacFn(v.mem, &fwd_jac) == CV_SUCCESS);
      }
    } else {
      v.NLS = SUNNonlinSol_FixedPoint(v.y, 0, v.ctx);
      etr::ass<"cvode: creating the fixed-point solver failed">(v.NLS != nullptr);
      etr::ass<"cvode: CVodeSetNonlinearSolver failed">(CVodeSetNonlinearSolver(v.mem, v.NLS) == CV_SUCCESS);
    }
    etr::ass<"cvode: CVodeSensInit failed">(
      CVodeSensInit(v.mem, ns, CV_STAGGERED, &fwd_sens, v.yS) == CV_SUCCESS);
    etr::ass<"cvode: CVodeSensEEtolerances failed">(CVodeSensEEtolerances(v.mem) == CV_SUCCESS);
    etr::ass<"cvode: CVodeSetSensErrCon failed">(CVodeSetSensErrCon(v.mem, SUNTRUE) == CV_SUCCESS);
    if (!stiff) {
      v.NLS_sens = SUNNonlinSol_FixedPointSens(ns, v.y, 0, v.ctx);
      etr::ass<"cvode: creating the fixed-point sensitivity solver failed">(v.NLS_sens != nullptr);
      etr::ass<"cvode: CVodeSetNonlinearSolverSensStg failed">(
        CVodeSetNonlinearSolverSensStg(v.mem, v.NLS_sens) == CV_SUCCESS);
    }

    const std::size_t nsz = static_cast<std::size_t>(ns);
    sens.assign(nt * nsz * neq, 0.0);
    auto store = [&](std::size_t k) {
      for (std::size_t j = 0; j < neq; ++j) vals[j * nt + k] = yp[j];
      for (std::size_t i = 0; i < nsz; ++i) {
        const sunrealtype *sp = N_VGetArrayPointer(v.yS[i]);
        std::copy(sp, sp + neq, sens.begin() + static_cast<std::ptrdiff_t>((k * nsz + i) * neq));
      }
    };
    store(0);
    for (std::size_t k = 1; k < nt; ++k) {
      sunrealtype tret = 0.0;
      int flag = CVode(v.mem, times[k], v.y, &tret, CV_NORMAL);
      etr::ass(flag == CV_SUCCESS, std::string("cvode: integration failed at t = ") +
                                     std::to_string(tret) + ": " +
                                     flag_name(CVodeGetReturnFlagName(flag)));
      flag = CVodeGetSens(v.mem, &tret, v.yS);
      etr::ass(flag == CV_SUCCESS, std::string("cvode: CVodeGetSens failed: ") +
                                     flag_name(CVodeGetReturnFlagName(flag)));
      store(k);
    }
  }

  // in_bar_i = sum_k S_i(t_k)^T ybar(t_k), ybar column-major nt x neq
  void backward_forward_sens(const std::vector<double> &ybar, std::vector<double> &in_bar) const {
    const std::size_t nsz = active.size();
    in_bar.assign(nsz, 0.0);
    for (std::size_t k = 0; k < nt; ++k) {
      for (std::size_t i = 0; i < nsz; ++i) {
        const double *S = sens.data() + (k * nsz + i) * neq;
        double acc = 0.0;
        for (std::size_t j = 0; j < neq; ++j) acc += S[j] * ybar[j * nt + k];
        in_bar[i] += acc;
      }
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

  // inputs that are tape constants (e.g. a literal y0) get no sensitivity
  const auto &tape_ops = etr::TAPE_INTERN_ref().op;
  auto is_active = [&tape_ops](int id) {
    return id >= 0 && tape_ops[static_cast<std::size_t>(id)] != etr::ROp::Const;
  };
  for (std::size_t i = 0; i < neq; ++i) {
    if (is_active(s->y0_ids[i])) s->active.push_back(static_cast<int>(i));
  }
  for (std::size_t i = 0; i < np; ++i) {
    if (is_active(s->p_ids[i])) s->active.push_back(static_cast<int>(neq + i));
  }
  const int mode = etr::get_val(opt.sensitivity);
  const bool forward_sens = mode == etr::sensitivity_forward ||
    (mode == etr::sensitivity_auto && s->active.size() <= etr::forward_sens_max_inputs);

  std::vector<double> vals(neq * nt);
  std::vector<uint8_t> na(neq * nt, 0);
  if (forward_sens) {
    s->solve_forward_sens(vals);
    if (etr::get_val(opt.stats)) print_cvode_stats(s->wf.mem);
  } else {
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
  }

  std::vector<int> input_ids = s->y0_ids;
  input_ids.insert(input_ids.end(), s->p_ids.begin(), s->p_ids.end());
  const int head = etr::TAPE_INTERN_ref().push_custom(vals, na, std::move(input_ids),
    [s, forward_sens](etr::ReverseTape &tape, int out0) {
      const std::size_t n_out = s->neq * s->nt;
      std::vector<double> ybar(n_out);
      bool any = false;
      for (std::size_t i = 0; i < n_out; ++i) {
        const std::size_t id = static_cast<std::size_t>(out0) + i;
        ybar[i] = tape.is_na[id] ? 0.0 : tape.adj[id];
        if (ybar[i] != 0.0) any = true;
      }
      if (!any) return;
      if (forward_sens) {
        std::vector<double> in_bar;
        s->backward_forward_sens(ybar, in_bar);
        for (std::size_t i = 0; i < in_bar.size(); ++i) {
          const std::size_t a = static_cast<std::size_t>(s->active[i]);
          const int id = a < s->neq ? s->y0_ids[a] : s->p_ids[a - s->neq];
          tape.adj[static_cast<std::size_t>(id)] += in_bar[i];
        }
        return;
      }
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
  // TODO: see above
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

// ---------- IDAS forward sensitivities -----------------------------------------
// resS_i = F_y s_i + F_yp s_i' + F_p pdot_i = dot part of F(Dual(y, s_i), Dual(yp, s_i'), p).
// IDACalcIC also makes s(t0), s'(t0) consistent: only differential y0 components
// and params influence the solution, yp0 and algebraic y0 are initial guesses.
template <typename F, typename P, typename J = no_jac> struct dual_res_ctx {
  const F *f;
  P *params;
  const J *jac = nullptr;
  std::vector<double> zero_dot_y, zero_dot_yp, sink_dot, sink_val, jac_dot;
};

template <typename Ctx>
int dual_res_trampoline(sunrealtype t, N_Vector y, N_Vector yp, N_Vector r, void *user_data) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *y_ptr = N_VGetArrayPointer(y);
  sunrealtype *yp_ptr = N_VGetArrayPointer(yp);
  sunrealtype *r_ptr = N_VGetArrayPointer(r);
  if (y_ptr == nullptr || yp_ptr == nullptr || r_ptr == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  // refilled: res may write into y or yp
  std::fill(ctx->zero_dot_y.begin(), ctx->zero_dot_y.end(), 0.0);
  std::fill(ctx->zero_dot_yp.begin(), ctx->zero_dot_yp.end(), 0.0);
  DualBorrowVec yv(y_ptr, ctx->zero_dot_y.data(), n, etr::Dim{n});
  DualBorrowVec ypv(yp_ptr, ctx->zero_dot_yp.data(), n, etr::Dim{n});
  DualBorrowVec rv(r_ptr, ctx->sink_dot.data(), n, etr::Dim{n});
  etr::Dual tt(static_cast<double>(t), 0.0);
  (*ctx->f)(tt, yv, ypv, rv, *ctx->params);
  return 0;
}

template <typename Ctx>
int sens_res_trampoline(int Ns, sunrealtype t, N_Vector y, N_Vector yp, N_Vector, N_Vector *yS,
                        N_Vector *ypS, N_Vector *resS, void *user_data, N_Vector, N_Vector,
                        N_Vector) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *y_ptr = N_VGetArrayPointer(y);
  sunrealtype *yp_ptr = N_VGetArrayPointer(yp);
  if (y_ptr == nullptr || yp_ptr == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  etr::Dual tt(static_cast<double>(t), 0.0);
  for (int i = 0; i < Ns; ++i) {
    sunrealtype *s_ptr = N_VGetArrayPointer(yS[i]);
    sunrealtype *sp_ptr = N_VGetArrayPointer(ypS[i]);
    sunrealtype *rs_ptr = N_VGetArrayPointer(resS[i]);
    if (s_ptr == nullptr || sp_ptr == nullptr || rs_ptr == nullptr) return -1;
    DualBorrowVec yv(y_ptr, s_ptr, n, etr::Dim{n});
    DualBorrowVec ypv(yp_ptr, sp_ptr, n, etr::Dim{n});
    DualBorrowVec rv(ctx->sink_val.data(), rs_ptr, n, etr::Dim{n});
    (*ctx->f)(tt, yv, ypv, rv, *ctx->params);
  }
  return 0;
}

template <typename Ctx>
int dual_ida_jac_trampoline(sunrealtype t, sunrealtype cj, N_Vector y, N_Vector yp, N_Vector,
                            SUNMatrix J, void *user_data, N_Vector, N_Vector, N_Vector) {
  auto *ctx = static_cast<Ctx *>(user_data);
  sunrealtype *y_ptr = N_VGetArrayPointer(y);
  sunrealtype *yp_ptr = N_VGetArrayPointer(yp);
  sunrealtype *j_ptr = SUNDenseMatrix_Data(J);
  if (y_ptr == nullptr || yp_ptr == nullptr || j_ptr == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  std::fill(ctx->zero_dot_y.begin(), ctx->zero_dot_y.end(), 0.0);
  std::fill(ctx->zero_dot_yp.begin(), ctx->zero_dot_yp.end(), 0.0);
  DualBorrowVec yv(y_ptr, ctx->zero_dot_y.data(), n, etr::Dim{n});
  DualBorrowVec ypv(yp_ptr, ctx->zero_dot_yp.data(), n, etr::Dim{n});
  DualBorrowVec jv(j_ptr, ctx->jac_dot.data(), n * n, etr::Dim{n, n});
  etr::Dual tt(static_cast<double>(t), 0.0);
  etr::Dual cjj(static_cast<double>(cj), 0.0);
  (*ctx->jac)(tt, cjj, yv, ypv, jv, *ctx->params);
  return 0;
}

template <typename Ctx> void dual_res_ctx_init(Ctx &ctx, std::size_t neq, bool with_jac) {
  ctx.zero_dot_y.assign(neq, 0.0);
  ctx.zero_dot_yp.assign(neq, 0.0);
  ctx.sink_dot.assign(neq, 0.0);
  ctx.sink_val.assign(neq, 0.0);
  if (with_jac) ctx.jac_dot.assign(neq * neq, 0.0);
}

struct idas_workspace {
  SUNContext ctx = nullptr;
  N_Vector y = nullptr;
  N_Vector yp = nullptr;
  N_Vector id = nullptr;
  N_Vector *yS = nullptr;
  N_Vector *ypS = nullptr;
  SUNMatrix A = nullptr;
  SUNLinearSolver LS = nullptr;
  void *mem = nullptr;
  int ns = 0;
  ~idas_workspace() {
    if (mem) IDAFree(&mem);
    if (LS) SUNLinSolFree(LS);
    if (A) SUNMatDestroy(A);
    if (ypS) N_VDestroyVectorArray(ypS, ns);
    if (yS) N_VDestroyVectorArray(yS, ns);
    if (id) N_VDestroy(id);
    if (yp) N_VDestroy(yp);
    if (y) N_VDestroy(y);
    if (ctx) SUNContext_Free(&ctx);
  }
};

// values only (Double, Dual or ReverseDouble)
template <typename Vec> void values_to_nvector(const Vec &src, N_Vector dst, std::size_t n) {
  sunrealtype *ptr = N_VGetArrayPointer(dst);
  for (std::size_t i = 0; i < n; ++i) {
    auto v = src.get(i);
    etr::ass<"ida: NA in y0, yp0 or id">(!v.isNA());
    ptr[i] = etr::get_val(v);
  }
}

inline void check_ida_id(N_Vector id, std::size_t n) {
  const sunrealtype *id_ptr = N_VGetArrayPointer(id);
  for (std::size_t i = 0; i < n; ++i) {
    etr::ass<"ida: id entries must be 0 (algebraic) or 1 (differential)">(
      id_ptr[i] == 0.0 || id_ptr[i] == 1.0);
  }
}

// IDA memory with tolerances, id, max_steps and the dense linear solver; y, yp, id filled
template <typename ResFn, typename JacFn>
inline void idas_setup(idas_workspace &w, ResFn res, JacFn jac_fn, void *user_data, double t0,
                       double reltol, double abstol, long int max_steps, int linear_solver) {
  w.mem = IDACreate(w.ctx);
  etr::ass<"ida: IDACreate failed">(w.mem != nullptr);
  etr::ass<"ida: IDAInit failed">(IDAInit(w.mem, res, t0, w.y, w.yp) == IDA_SUCCESS);
  etr::ass<"ida: IDASStolerances failed">(IDASStolerances(w.mem, reltol, abstol) == IDA_SUCCESS);
  etr::ass<"ida: IDASetUserData failed">(IDASetUserData(w.mem, user_data) == IDA_SUCCESS);
  etr::ass<"ida: IDASetId failed">(IDASetId(w.mem, w.id) == IDA_SUCCESS);
  etr::ass<"ida: IDASetMaxNumSteps failed">(IDASetMaxNumSteps(w.mem, max_steps) == IDA_SUCCESS);
  const auto n = NV_LENGTH_S(w.y);
  w.A = SUNDenseMatrix(n, n, w.ctx);
  etr::ass<"ida: SUNDenseMatrix failed">(w.A != nullptr);
  w.LS = dense_solver(linear_solver, w.y, w.A, w.ctx);
  etr::ass<"ida: creating the dense linear solver failed">(w.LS != nullptr);
  etr::ass<"ida: IDASetLinearSolver failed">(IDASetLinearSolver(w.mem, w.LS, w.A) == IDA_SUCCESS);
  if (jac_fn != nullptr) {
    etr::ass<"ida: IDASetJacFn failed">(IDASetJacFn(w.mem, jac_fn) == IDA_SUCCESS);
  }
}

template <typename SensFn> inline void idas_sens_init(idas_workspace &w, SensFn sens_fn) {
  etr::ass<"ida: IDASensInit failed">(
    IDASensInit(w.mem, w.ns, IDA_STAGGERED, sens_fn, w.yS, w.ypS) == IDA_SUCCESS);
  etr::ass<"ida: IDASensEEtolerances failed">(IDASensEEtolerances(w.mem) == IDA_SUCCESS);
  etr::ass<"ida: IDASetSensErrCon failed">(IDASetSensErrCon(w.mem, SUNTRUE) == IDA_SUCCESS);
}

// IDACalcIC towards t1, also for the sensitivities when they are initialised
inline void idas_consistent_ic(idas_workspace &w, double t1) {
  int ic_flag = IDACalcIC(w.mem, IDA_YA_YDP_INIT, t1);
  etr::ass(ic_flag == IDA_SUCCESS,
           std::string("ida: IDACalcIC failed (no consistent initial values found): ") +
             flag_name(IDAGetReturnFlagName(ic_flag)));
  etr::ass<"ida: IDAGetConsistentIC failed">(IDAGetConsistentIC(w.mem, w.y, w.yp) == IDA_SUCCESS);
  if (w.ns > 0) {
    etr::ass<"ida: IDAGetSensConsistentIC failed">(
      IDAGetSensConsistentIC(w.mem, w.yS, w.ypS) == IDA_SUCCESS);
  }
}

inline void idas_step(idas_workspace &w, double t) {
  sunrealtype tret = 0.0;
  int flag = IDASolve(w.mem, t, &tret, w.y, w.yp, IDA_NORMAL);
  etr::ass(flag == IDA_SUCCESS, std::string("ida: integration failed at t = ") +
                                  std::to_string(tret) + ": " +
                                  flag_name(IDAGetReturnFlagName(flag)));
  if (w.ns > 0) {
    flag = IDAGetSens(w.mem, &tret, w.yS);
    etr::ass(flag == IDA_SUCCESS,
             std::string("ida: IDAGetSens failed: ") + flag_name(IDAGetReturnFlagName(flag)));
  }
}

// ---------- forward mode (Dual): one sensitivity, s(t0) = y0dot ----------------
template <typename JAC, typename RES, typename Y, typename YP, typename T, typename P, typename ID>
requires(
etr::IsArray<etr::Decayed<Y>> && etr::IsArray<etr::Decayed<YP>> &&
etr::IsArray<etr::Decayed<T>> && etr::IsArray<etr::Decayed<ID>>
)
inline auto idas_forward_impl(const RES &res_fct, const JAC *jac, const Y &y0, const YP &yp0,
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
  using Ctx = dual_res_ctx<RES, P, JAC>;
  Ctx ctx{&res_fct, &p_local, jac};
  dual_res_ctx_init(ctx, neq, !std::is_same_v<JAC, no_jac>);

  idas_workspace w;
  etr::ass<"ida: SUNContext_Create failed">(SUNContext_Create(SUN_COMM_NULL, &w.ctx) == 0);
  SUNContext_ClearErrHandlers(w.ctx);
  const auto n = static_cast<sunindextype>(neq);
  w.y = N_VNew_Serial(n, w.ctx);
  w.yp = N_VNew_Serial(n, w.ctx);
  w.id = N_VNew_Serial(n, w.ctx);
  etr::ass<"ida: N_VNew_Serial failed">(w.y != nullptr && w.yp != nullptr && w.id != nullptr);
  w.yS = N_VCloneVectorArray(1, w.y);
  w.ypS = N_VCloneVectorArray(1, w.y);
  etr::ass<"ida: N_VCloneVectorArray failed">(w.yS != nullptr && w.ypS != nullptr);
  w.ns = 1;
  values_to_nvector(id, w.id, neq);
  check_ida_id(w.id, neq);
  sunrealtype *y_ptr = N_VGetArrayPointer(w.y);
  sunrealtype *yp_ptr = N_VGetArrayPointer(w.yp);
  sunrealtype *s_ptr = N_VGetArrayPointer(w.yS[0]);
  sunrealtype *sp_ptr = N_VGetArrayPointer(w.ypS[0]);
  for (std::size_t i = 0; i < neq; ++i) {
    etr::Dual yi = y0.get(i);
    etr::Dual ypi = yp0.get(i);
    etr::ass<"ida: NA in y0, yp0 or id">(!yi.isNA() && !ypi.isNA());
    y_ptr[i] = yi.val;
    s_ptr[i] = yi.dot;
    yp_ptr[i] = ypi.val;
    sp_ptr[i] = ypi.dot;
  }

  if constexpr (std::is_same_v<JAC, no_jac>) {
    idas_setup(w, &dual_res_trampoline<Ctx>, IDALsJacFn{nullptr}, &ctx,
               etr::get_val(times.get(0)), etr::get_val(opt.reltol), etr::get_val(opt.abstol),
               max_steps, linear_solver);
  } else {
    idas_setup(w, &dual_res_trampoline<Ctx>, &dual_ida_jac_trampoline<Ctx>, &ctx,
               etr::get_val(times.get(0)), etr::get_val(opt.reltol), etr::get_val(opt.abstol),
               max_steps, linear_solver);
  }
  idas_sens_init(w, &sens_res_trampoline<Ctx>);
  idas_consistent_ic(w, etr::get_val(times.get(1)));

  etr::Array<etr::Dual, etr::Buffer<etr::Dual, etr::RBufferTrait>> res(etr::SI{neq * nt});
  res.dim = etr::Dim{nt, neq};
  for (std::size_t j = 0; j < neq; ++j) res.set(j * nt, etr::Dual(y_ptr[j], s_ptr[j]));
  for (std::size_t k = 1; k < nt; ++k) {
    idas_step(w, etr::get_val(times.get(k)));
    for (std::size_t j = 0; j < neq; ++j) res.set(j * nt + k, etr::Dual(y_ptr[j], s_ptr[j]));
  }
  if (etr::get_val(opt.stats)) print_ida_stats(w.mem);
  return res;
}

// ---------- reverse mode (ReverseDouble) ------------------------------------------
// ida is one Custom block on the tape. Forward with res_t<Double>; derivatives either
// - forward sensitivities: one per active input (y0 or params) with res_t<Dual>,
//   input_bar_i = sum_k S_i(t_k)^T ybar(t_k), no backward solve
// - adjoint: backward DAE (M^T lambda)' - F_y^T lambda = 0, M = F_yp, written with
//   nu = (M^T lambda)_d so that only first derivatives of F appear:
//     nu' = (F_y^T lambda)_d,  (M^T lambda)_d = nu,  (F_y^T lambda)_a = 0
//   lambda algebraic, nu differential; index 1 (matrix w.r.t. lambda = E^T).
//   Jump at every t_k projects ybar onto the algebraic components.
//   hessian: the same system differentiated along a direction (second-order adjoint).
//   Theory: development/Plans/ode_sensitivities.md, step 5.
template <typename RES_R, typename JAC_R> struct ida_sens_state {
  using RES_D = typename rebind_scalar<RES_R, etr::Double>::type;
  using JAC_D = typename rebind_scalar<JAC_R, etr::Double>::type;
  using RES_Dual = typename rebind_scalar<RES_R, etr::Dual>::type;
  using JAC_Dual = typename rebind_scalar<JAC_R, etr::Dual>::type;
  using DualVec = etr::Array<etr::Dual, etr::Buffer<etr::Dual>>;
  using Ctx = fct_ctx<RES_D, DoubleVec, JAC_D>;
  using DualCtx = dual_res_ctx<RES_Dual, DualVec>;
  using DualJacCtx = dual_res_ctx<RES_Dual, DualVec, JAC_Dual>;

  idas_workspace w;
  RES_D res_d{};
  JAC_D jac_d{};
  RES_Dual res_dual{};
  RES_R res_r{};
  DoubleVec p_double;
  DualVec p_dual;
  Ctx ctx{};
  DualCtx dual_ctx{};
  std::vector<double> p_vals;
  std::vector<int> y0_ids, p_ids;
  std::vector<int> active;   // input index (0..neq-1: y0, neq..: p)
  std::vector<double> sens;  // S_i(t_k)[j] at (k * Ns + i) * neq + j
  std::size_t neq = 0, nt = 0;

  // adjoint
  std::vector<double> times, id_vals, y0_vals, yp0_vals;
  std::vector<int> diff_idx;    // variable index of nu_m
  std::vector<int> diff_pos;    // position of variable i in nu, -1 if algebraic
  std::vector<double> ys, yps;  // y(t_k), y'(t_k) at k * neq + j
  double reltol = 0.0, abstol = 0.0;
  long int max_steps = 0;
  int linear_solver = 1;
  N_Vector yB = nullptr, ypB = nullptr, qB = nullptr, idB = nullptr;
  SUNMatrix AB = nullptr;
  SUNLinearSolver LSB = nullptr;
  int which = -1;

  ~ida_sens_state() {
    if (w.mem) IDAFree(&w.mem);
    if (LSB) SUNLinSolFree(LSB);
    if (AB) SUNMatDestroy(AB);
    for (N_Vector v : {yB, ypB, qB, idB}) {
      if (v) N_VDestroy(v);
    }
  }

  void set_layout() {
    diff_idx.clear();
    diff_pos.assign(neq, -1);
    for (std::size_t i = 0; i < neq; ++i) {
      if (id_vals[i] == 1.0) {
        diff_pos[i] = static_cast<int>(diff_idx.size());
        diff_idx.push_back(static_cast<int>(i));
      }
    }
  }
  std::size_t nd() const { return diff_idx.size(); }

  // x = (y, yp, p) as leaves on a fresh tape segment, residual recorded; returns start
  std::vector<int> x_ids, out_ids;
  std::size_t record(double t, const double *y, const double *yp) {
    const std::size_t np = p_vals.size();
    auto &tape = etr::TAPE_INTERN_ref();
    const std::size_t start = tape.size();
    x_ids.resize(2 * neq + np);
    std::vector<etr::ReverseDouble> y_leaves(neq), yp_leaves(neq), r(neq);
    for (std::size_t i = 0; i < neq; ++i) {
      x_ids[i] = tape.push_var(y[i]);
      y_leaves[i] = etr::ReverseDouble(etr::from_tape_id_t{}, x_ids[i], 0);
      x_ids[neq + i] = tape.push_var(yp[i]);
      yp_leaves[i] = etr::ReverseDouble(etr::from_tape_id_t{}, x_ids[neq + i], 0);
    }
    RevVec p_local(etr::SI{np});
    for (std::size_t i = 0; i < np; ++i) {
      x_ids[2 * neq + i] = tape.push_var(p_vals[i]);
      p_local.set(i, etr::ReverseDouble(etr::from_tape_id_t{}, x_ids[2 * neq + i], 0));
    }
    etr::ReverseDouble tt(t);
    RevBorrowVec yv(y_leaves.data(), neq, etr::Dim{neq});
    RevBorrowVec ypv(yp_leaves.data(), neq, etr::Dim{neq});
    RevBorrowVec rv(r.data(), neq, etr::Dim{neq});
    res_r(tt, yv, ypv, rv, p_local);
    out_ids.resize(neq);
    for (std::size_t i = 0; i < neq; ++i) out_ids[i] = r[i].id;
    return start;
  }

  // vjp seeded wgt: adj_y = F_y^T wgt, adj_yp = M^T wgt, adj_p = F_p^T wgt
  std::vector<double> adj_y, adj_yp, adj_p;
  std::vector<double> vjp_key;
  bool vjp_valid = false;

  void vjp(double t, const double *y, const double *yp, const double *wgt) {
    std::vector<double> key;
    key.reserve(1 + 3 * neq);
    key.push_back(t);
    key.insert(key.end(), y, y + neq);
    key.insert(key.end(), yp, yp + neq);
    key.insert(key.end(), wgt, wgt + neq);
    if (vjp_valid && key == vjp_key) return;
    const std::size_t np = p_vals.size();
    auto &tape = etr::TAPE_INTERN_ref();
    const std::size_t nblocks = tape.blocks.size();
    const std::size_t start = record(t, y, yp);
    const std::size_t end = tape.size();
    std::fill(tape.adj.begin() + static_cast<std::ptrdiff_t>(start), tape.adj.end(), 0.0);
    for (std::size_t i = 0; i < neq; ++i) {
      const int id = out_ids[i];
      if (id >= static_cast<int>(start)) tape.adj[static_cast<std::size_t>(id)] += wgt[i];
    }
    tape.reverse_range(end, start);
    adj_y.resize(neq);
    adj_yp.resize(neq);
    adj_p.resize(np);
    for (std::size_t i = 0; i < neq; ++i) {
      adj_y[i] = tape.adj[static_cast<std::size_t>(x_ids[i])];
      adj_yp[i] = tape.adj[static_cast<std::size_t>(x_ids[neq + i])];
    }
    for (std::size_t i = 0; i < np; ++i) adj_p[i] = tape.adj[static_cast<std::size_t>(x_ids[2 * neq + i])];
    tape.rewind(start, nblocks);
    vjp2_valid = false;
    vjp_key = std::move(key);
    vjp_valid = true;
  }

  // forward-over-reverse along (y: s, yp: sp, p: pdot), seeds (wgt, wgt_dot):
  // adj_* as vjp, adj_dot_x = F_x^T wgt_dot + (d/de F_x)^T wgt
  std::vector<double> adj_dot_y, adj_dot_yp, adj_dot_p;
  std::vector<double> vjp2_key;
  bool vjp2_valid = false;

  void vjp2(double t, const double *y, const double *yp, const double *s, const double *sp,
            const double *pdot, const double *wgt, const double *wgt_dot) {
    const std::size_t np = p_vals.size();
    std::vector<double> key;
    key.reserve(1 + 6 * neq + np);
    key.push_back(t);
    for (const double *v : {y, yp, s, sp, wgt, wgt_dot}) key.insert(key.end(), v, v + neq);
    key.insert(key.end(), pdot, pdot + np);
    if (vjp2_valid && key == vjp2_key) return;
    auto &tape = etr::TAPE_INTERN_ref();
    const std::size_t nblocks = tape.blocks.size();
    const std::size_t start = record(t, y, yp);
    std::vector<double> dir(2 * neq + np);
    std::copy(s, s + neq, dir.begin());
    std::copy(sp, sp + neq, dir.begin() + static_cast<std::ptrdiff_t>(neq));
    std::copy(pdot, pdot + np, dir.begin() + static_cast<std::ptrdiff_t>(2 * neq));
    {
      etr::HessianSweep hs(tape, start);
      hs.forward(x_ids, dir.data());
      hs.reverse_seeded(out_ids, wgt, wgt_dot);
      auto at = [&](std::size_t x) { return static_cast<std::size_t>(x_ids[x]) - start; };
      adj_y.resize(neq);
      adj_yp.resize(neq);
      adj_p.resize(np);
      adj_dot_y.resize(neq);
      adj_dot_yp.resize(neq);
      adj_dot_p.resize(np);
      for (std::size_t i = 0; i < neq; ++i) {
        adj_y[i] = hs.adj[at(i)];
        adj_yp[i] = hs.adj[at(neq + i)];
        adj_dot_y[i] = hs.adj_dot[at(i)];
        adj_dot_yp[i] = hs.adj_dot[at(neq + i)];
      }
      for (std::size_t i = 0; i < np; ++i) {
        adj_p[i] = hs.adj[at(2 * neq + i)];
        adj_dot_p[i] = hs.adj_dot[at(2 * neq + i)];
      }
    }
    tape.rewind(start, nblocks);
    vjp_valid = false;
    vjp2_key = std::move(key);
    vjp2_valid = true;
  }

  // residual of [lambda; nu] from gy = F_y^T lambda, gyp = M^T lambda
  void adjoint_residual(const double *gy, const double *gyp, const double *nu,
                        const double *nu_p, double *out) const {
    for (std::size_t i = 0; i < neq; ++i) {
      const int m = diff_pos[i];
      out[i] = m >= 0 ? gyp[i] - nu[m] : gy[i];
    }
    for (std::size_t m = 0; m < nd(); ++m) {
      out[neq + m] = nu_p[m] - gy[static_cast<std::size_t>(diff_idx[m])];
    }
  }

  static int res_b(sunrealtype t, N_Vector y, N_Vector yp, N_Vector zB, N_Vector zpB,
                   N_Vector rrB, void *user_data) {
    auto *s = static_cast<ida_sens_state *>(user_data);
    const sunrealtype *z = N_VGetArrayPointer(zB);
    const sunrealtype *zp = N_VGetArrayPointer(zpB);
    s->vjp(t, N_VGetArrayPointer(y), N_VGetArrayPointer(yp), z);
    s->adjoint_residual(s->adj_y.data(), s->adj_yp.data(), z + s->neq, zp + s->neq,
                        N_VGetArrayPointer(rrB));
    return 0;
  }

  // qB(t0) = -int F_p^T lambda dt
  static int quad_b(sunrealtype t, N_Vector y, N_Vector yp, N_Vector zB, N_Vector,
                    N_Vector qBdot, void *user_data) {
    auto *s = static_cast<ida_sens_state *>(user_data);
    s->vjp(t, N_VGetArrayPointer(y), N_VGetArrayPointer(yp), N_VGetArrayPointer(zB));
    sunrealtype *out = N_VGetArrayPointer(qBdot);
    for (std::size_t i = 0; i < s->p_vals.size(); ++i) out[i] = s->adj_p[i];
    return 0;
  }

  // E = M D + F_y A at (t, y, yp), column-major; one res_t<Dual> evaluation per column.
  // Also checks M A = 0.
  std::vector<double> E;
  std::vector<int> E_piv;

  void build_E(double t, const double *y, const double *yp) {
    const std::size_t np = p_vals.size();
    for (std::size_t m = 0; m < np; ++m) p_dual.set(m, etr::Dual(p_vals[m], 0.0));
    std::vector<double> y_val(neq), yp_val(neq), y_dot(neq), yp_dot(neq), r_val(neq), r_dot(neq);
    auto column = [&](std::size_t j, bool wrt_yp) {
      std::copy(y, y + neq, y_val.begin());
      std::copy(yp, yp + neq, yp_val.begin());
      std::fill(y_dot.begin(), y_dot.end(), 0.0);
      std::fill(yp_dot.begin(), yp_dot.end(), 0.0);
      std::fill(r_dot.begin(), r_dot.end(), 0.0);
      (wrt_yp ? yp_dot : y_dot)[j] = 1.0;
      DualBorrowVec yv(y_val.data(), y_dot.data(), neq, etr::Dim{neq});
      DualBorrowVec ypv(yp_val.data(), yp_dot.data(), neq, etr::Dim{neq});
      DualBorrowVec rv(r_val.data(), r_dot.data(), neq, etr::Dim{neq});
      etr::Dual tt(t, 0.0);
      res_dual(tt, yv, ypv, rv, p_dual);
    };
    E.assign(neq * neq, 0.0);
    for (std::size_t j = 0; j < neq; ++j) {
      const bool differential = id_vals[j] == 1.0;
      column(j, true);
      if (differential) {
        for (std::size_t i = 0; i < neq; ++i) E[j * neq + i] = r_dot[i];
      } else {
        for (std::size_t i = 0; i < neq; ++i) {
          etr::ass<"ida (adjoint): an algebraic variable (id = 0) appears with its derivative yp in the residual">(
            r_dot[i] == 0.0);
        }
        column(j, false);
        for (std::size_t i = 0; i < neq; ++i) E[j * neq + i] = r_dot[i];
      }
    }
    int n = static_cast<int>(neq);
    int info = 0;
    E_piv.resize(neq);
    F77_CALL(dgetrf)(&n, &n, E.data(), &n, E_piv.data(), &info);
    etr::ass<"ida (adjoint): singular matrix [F_yp (differential) | F_y (algebraic)]; is the DAE of index 1?">(
      info == 0);
  }

  // b <- E^-T b
  void solve_Et(std::vector<double> &b) {
    int n = static_cast<int>(neq);
    int nrhs = 1;
    int info = 0;
    F77_CALL(dgetrs)("T", &n, &nrhs, E.data(), &n, E_piv.data(), b.data(), &n, &info FCONE);
    etr::ass<"ida (adjoint): dgetrs failed">(info == 0);
  }

  // jump at t_k: E^T r = A ybar, nu += (ybar - F_y^T r)_d, pbar_direct -= F_p^T r;
  // lambda is made consistent afterwards by consistent_first
  void jump(std::size_t k, const std::vector<double> &ybar, double *nu,
            std::vector<double> &pbar_direct) {
    const double *y = ys.data() + k * neq;
    const double *yp = yps.data() + k * neq;
    build_E(times[k], y, yp);
    std::vector<double> r(neq);
    for (std::size_t j = 0; j < neq; ++j) r[j] = id_vals[j] == 1.0 ? 0.0 : ybar[j * nt + k];
    solve_Et(r);
    vjp(times[k], y, yp, r.data());
    for (std::size_t m = 0; m < nd(); ++m) {
      const std::size_t i = static_cast<std::size_t>(diff_idx[m]);
      nu[m] += ybar[i * nt + k] - adj_y[i];
    }
    for (std::size_t m = 0; m < p_vals.size(); ++m) pbar_direct[m] -= adj_p[m];
  }

  // consistent [lambda; nu] and [lambda'; nu'] at t_k (E of t_k is factorised by jump):
  // E^T lambda = [nu; 0], nu' = (F_y^T lambda)_d.
  // NOTE: not IDACalcICB: its difference-quotient Newton fails for tight abstol
  void consistent_first(std::size_t k, double *z, double *zp) {
    const double *y = ys.data() + k * neq;
    const double *yp = yps.data() + k * neq;
    for (std::size_t i = 0; i < neq; ++i) {
      const int m = diff_pos[i];
      z[i] = m >= 0 ? z[neq + static_cast<std::size_t>(m)] : 0.0;
    }
    std::vector<double> lam(z, z + neq);
    solve_Et(lam);
    std::copy(lam.begin(), lam.end(), z);
    vjp(times[k], y, yp, z);
    std::fill(zp, zp + neq + nd(), 0.0);
    for (std::size_t m = 0; m < nd(); ++m) zp[neq + m] = adj_y[static_cast<std::size_t>(diff_idx[m])];
  }

  // backward vectors; nb unknowns, idB: lambda 0, nu 1 (repeated per block)
  void new_backward_vectors(SUNContext sctx, std::size_t nb, std::size_t nq, std::size_t blocks,
                            N_Vector &zB, N_Vector &zpB, N_Vector &id_out, N_Vector &q_out) {
    zB = N_VNew_Serial(static_cast<sunindextype>(nb), sctx);
    zpB = N_VNew_Serial(static_cast<sunindextype>(nb), sctx);
    id_out = N_VNew_Serial(static_cast<sunindextype>(nb), sctx);
    q_out = N_VNew_Serial(static_cast<sunindextype>(nq), sctx);
    etr::ass<"ida (adjoint): N_VNew_Serial failed">(zB && zpB && id_out && q_out);
    N_VConst(0.0, zB);
    N_VConst(0.0, zpB);
    N_VConst(0.0, q_out);
    sunrealtype *idp = N_VGetArrayPointer(id_out);
    const std::size_t block = neq + nd();
    for (std::size_t b = 0; b < blocks; ++b) {
      for (std::size_t i = 0; i < block; ++i) idp[b * block + i] = i < neq ? 0.0 : 1.0;
    }
  }

  void setup_backward(void *mem, int which_, N_Vector zB, N_Vector idv, SUNMatrix &A_out,
                      SUNLinearSolver &LS_out, SUNContext sctx) {
    const auto nb = NV_LENGTH_S(zB);
    etr::ass<"ida (adjoint): IDASStolerancesB failed">(
      IDASStolerancesB(mem, which_, reltol, abstol) == IDA_SUCCESS);
    etr::ass<"ida (adjoint): IDASetUserDataB failed">(
      IDASetUserDataB(mem, which_, this) == IDA_SUCCESS);
    etr::ass<"ida (adjoint): IDASetMaxNumStepsB failed">(
      IDASetMaxNumStepsB(mem, which_, max_steps) == IDA_SUCCESS);
    etr::ass<"ida (adjoint): IDASetIdB failed">(IDASetIdB(mem, which_, idv) == IDA_SUCCESS);
    // NOTE: lambda follows the interpolated forward yp, which has kinks at every
    // forward step (M depending on yp) -> repeated error test failures otherwise
    etr::ass<"ida (adjoint): IDASetSuppressAlgB failed">(
      IDASetSuppressAlgB(mem, which_, SUNTRUE) == IDA_SUCCESS);
    // no jacB: difference quotients of the backward residual
    A_out = SUNDenseMatrix(nb, nb, sctx);
    etr::ass<"ida (adjoint): SUNDenseMatrix failed">(A_out != nullptr);
    LS_out = dense_solver(linear_solver, zB, A_out, sctx);
    etr::ass<"ida (adjoint): creating the dense linear solver failed">(LS_out != nullptr);
    etr::ass<"ida (adjoint): IDASetLinearSolverB failed">(
      IDASetLinearSolverB(mem, which_, LS_out, A_out) == IDA_SUCCESS);
    etr::ass<"ida (adjoint): IDAQuadSStolerancesB failed">(
      IDAQuadSStolerancesB(mem, which_, reltol, abstol) == IDA_SUCCESS);
    etr::ass<"ida (adjoint): IDASetQuadErrConB failed">(
      IDASetQuadErrConB(mem, which_, SUNTRUE) == IDA_SUCCESS);
  }

  void init_backward() {
    new_backward_vectors(w.ctx, neq + nd(), p_vals.size(), 1, yB, ypB, idB, qB);
    etr::ass<"ida (adjoint): IDACreateB failed">(IDACreateB(w.mem, &which) == IDA_SUCCESS);
    etr::ass<"ida (adjoint): IDAInitB failed">(
      IDAInitB(w.mem, which, &res_b, times[nt - 1], yB, ypB) == IDA_SUCCESS);
    etr::ass<"ida (adjoint): IDAQuadInitB failed">(
      IDAQuadInitB(w.mem, which, &quad_b, qB) == IDA_SUCCESS);
    setup_backward(w.mem, which, yB, idB, AB, LSB, w.ctx);
  }

  void check_backward_flag(int flag, const char *what, double t) {
    etr::ass(flag >= 0, std::string("ida (adjoint): ") + what + " failed at t = " +
                          std::to_string(t) + ": " + flag_name(IDAGetReturnFlagName(flag)));
  }

  // ybar: column-major nt x neq; y0bar = nu(t0) (differential), pbar = qB(t0) - sum_k F_p^T r_k
  void backward_adjoint(const std::vector<double> &ybar, std::vector<double> &y0bar,
                        std::vector<double> &pbar) {
    const std::size_t np = p_vals.size();
    if (which < 0) init_backward();
    sunrealtype *z = N_VGetArrayPointer(yB);
    N_VConst(0.0, yB);
    N_VConst(0.0, qB);
    std::vector<double> pbar_direct(np, 0.0);
    jump(nt - 1, ybar, z + neq, pbar_direct);
    for (std::size_t k = nt - 1; k >= 1; --k) {
      consistent_first(k, z, N_VGetArrayPointer(ypB));
      vjp_valid = false;
      etr::ass<"ida (adjoint): IDAReInitB failed">(
        IDAReInitB(w.mem, which, times[k], yB, ypB) == IDA_SUCCESS);
      etr::ass<"ida (adjoint): IDAQuadReInitB failed">(
        IDAQuadReInitB(w.mem, which, qB) == IDA_SUCCESS);
      check_backward_flag(IDASolveB(w.mem, times[k - 1], IDA_NORMAL), "backward integration", times[k]);
      sunrealtype tret = 0.0;
      etr::ass<"ida (adjoint): IDAGetB failed">(IDAGetB(w.mem, which, &tret, yB, ypB) == IDA_SUCCESS);
      etr::ass<"ida (adjoint): IDAGetQuadB failed">(
        IDAGetQuadB(w.mem, which, &tret, qB) == IDA_SUCCESS);
      jump(k - 1, ybar, z + neq, pbar_direct);
    }
    y0bar.assign(neq, 0.0);
    for (std::size_t m = 0; m < nd(); ++m) y0bar[static_cast<std::size_t>(diff_idx[m])] = z[neq + m];
    const sunrealtype *q = N_VGetArrayPointer(qB);
    pbar.resize(np);
    for (std::size_t m = 0; m < np; ++m) pbar[m] = q[m] + pbar_direct[m];
  }

  // ---------- second order (HessianSweep), one direction d = (y0dot, pdot) ----------
  // tangent: forward solve with one sensitivity s = dy/dd (res_t<Dual>, p dot = pdot).
  // backward2: [lambda; nu; mu; eta] with mu = d lambda/dd, eta = d nu/dd:
  //   the first-order system, and the same system with F^T mu + (dF/dd)^T lambda
  //   (one forward-over-reverse sweep, vjp2). Jumps differentiated along d.
  struct second_order {
    idas_workspace w;
    N_Vector yB = nullptr, ypB = nullptr, qB = nullptr, idB = nullptr;
    SUNMatrix AB = nullptr;
    SUNLinearSolver LSB = nullptr;
    int which = -1;
    RES_Dual res{};
    JAC_Dual jac{};
    DualVec p_dual;
    DualJacCtx ctx{};
    std::vector<double> pdot, ys, yps, ss, sps;  // at k * neq + j
    ~second_order() {
      if (w.mem) IDAFree(&w.mem);
      if (LSB) SUNLinSolFree(LSB);
      if (AB) SUNMatDestroy(AB);
      for (N_Vector v : {yB, ypB, qB, idB}) {
        if (v) N_VDestroy(v);
      }
    }
  };
  std::unique_ptr<second_order> so;

  // in_dot = (y0dot, pdot); out_dot = s(t_k), column-major nt x neq
  void tangent(const std::vector<double> &in_dot, std::vector<double> &out_dot) {
    const std::size_t np = p_vals.size();
    so = std::make_unique<second_order>();
    second_order &o = *so;
    o.pdot.assign(in_dot.begin() + static_cast<std::ptrdiff_t>(neq), in_dot.end());
    o.p_dual = DualVec(etr::SI{np});
    for (std::size_t m = 0; m < np; ++m) o.p_dual.set(m, etr::Dual(p_vals[m], o.pdot[m]));
    o.ctx.f = &o.res;
    o.ctx.params = &o.p_dual;
    constexpr bool with_jac = !std::is_same_v<JAC_R, no_jac>;
    if constexpr (with_jac) o.ctx.jac = &o.jac;
    dual_res_ctx_init(o.ctx, neq, with_jac);

    idas_workspace &v = o.w;
    etr::ass<"hessian (ida): SUNContext_Create failed">(SUNContext_Create(SUN_COMM_NULL, &v.ctx) == 0);
    SUNContext_ClearErrHandlers(v.ctx);
    const auto n = static_cast<sunindextype>(neq);
    v.y = N_VNew_Serial(n, v.ctx);
    v.yp = N_VNew_Serial(n, v.ctx);
    v.id = N_VNew_Serial(n, v.ctx);
    etr::ass<"hessian (ida): N_VNew_Serial failed">(v.y && v.yp && v.id);
    v.yS = N_VCloneVectorArray(1, v.y);
    v.ypS = N_VCloneVectorArray(1, v.y);
    etr::ass<"hessian (ida): N_VCloneVectorArray failed">(v.yS && v.ypS);
    v.ns = 1;
    std::copy(y0_vals.begin(), y0_vals.end(), N_VGetArrayPointer(v.y));
    std::copy(yp0_vals.begin(), yp0_vals.end(), N_VGetArrayPointer(v.yp));
    std::copy(id_vals.begin(), id_vals.end(), N_VGetArrayPointer(v.id));
    std::copy(in_dot.begin(), in_dot.begin() + static_cast<std::ptrdiff_t>(neq),
              N_VGetArrayPointer(v.yS[0]));
    N_VConst(0.0, v.ypS[0]);
    if constexpr (with_jac) {
      idas_setup(v, &dual_res_trampoline<DualJacCtx>, &dual_ida_jac_trampoline<DualJacCtx>, &o.ctx,
                 times[0], reltol, abstol, max_steps, linear_solver);
    } else {
      idas_setup(v, &dual_res_trampoline<DualJacCtx>, IDALsJacFn{nullptr}, &o.ctx, times[0],
                 reltol, abstol, max_steps, linear_solver);
    }
    idas_sens_init(v, &sens_res_trampoline<DualJacCtx>);
    etr::ass<"hessian (ida): IDAAdjInit failed">(IDAAdjInit(v.mem, 100, IDA_HERMITE) == IDA_SUCCESS);
    idas_consistent_ic(v, times[1]);

    o.ys.resize(nt * neq);
    o.yps.resize(nt * neq);
    o.ss.resize(nt * neq);
    o.sps.resize(nt * neq);
    auto store = [&](std::size_t k) {
      const auto off = static_cast<std::ptrdiff_t>(k * neq);
      const sunrealtype *a = N_VGetArrayPointer(v.y);
      const sunrealtype *b = N_VGetArrayPointer(v.yp);
      const sunrealtype *c = N_VGetArrayPointer(v.yS[0]);
      const sunrealtype *d = N_VGetArrayPointer(v.ypS[0]);
      std::copy(a, a + neq, o.ys.begin() + off);
      std::copy(b, b + neq, o.yps.begin() + off);
      std::copy(c, c + neq, o.ss.begin() + off);
      std::copy(d, d + neq, o.sps.begin() + off);
    };
    store(0);
    for (std::size_t k = 1; k < nt; ++k) {
      sunrealtype tret = 0.0;
      int ncheck = 0;
      int flag = IDASolveF(v.mem, times[k], &tret, v.y, v.yp, IDA_NORMAL, &ncheck);
      etr::ass(flag == IDA_SUCCESS, std::string("hessian (ida): integration failed at t = ") +
                                      std::to_string(tret) + ": " +
                                      flag_name(IDAGetReturnFlagName(flag)));
      etr::ass<"hessian (ida): IDAGetSens failed">(IDAGetSens(v.mem, &tret, v.yS) == IDA_SUCCESS);
      etr::ass<"hessian (ida): IDAGetSensDky failed">(
        IDAGetSensDky(v.mem, tret, 1, v.ypS) == IDA_SUCCESS);
      store(k);
    }
    out_dot.assign(neq * nt, 0.0);
    for (std::size_t k = 0; k < nt; ++k) {
      for (std::size_t j = 0; j < neq; ++j) out_dot[j * nt + k] = o.ss[k * neq + j];
    }
  }

  static int res_bs(sunrealtype t, N_Vector y, N_Vector yp, N_Vector *yS, N_Vector *ypS,
                    N_Vector zB, N_Vector zpB, N_Vector rrBS, void *user_data) {
    auto *s = static_cast<ida_sens_state *>(user_data);
    const std::size_t block = s->neq + s->nd();
    const sunrealtype *z = N_VGetArrayPointer(zB);
    const sunrealtype *zp = N_VGetArrayPointer(zpB);
    s->vjp2(t, N_VGetArrayPointer(y), N_VGetArrayPointer(yp), N_VGetArrayPointer(yS[0]),
            N_VGetArrayPointer(ypS[0]), s->so->pdot.data(), z, z + block);
    sunrealtype *out = N_VGetArrayPointer(rrBS);
    s->adjoint_residual(s->adj_y.data(), s->adj_yp.data(), z + s->neq, zp + s->neq, out);
    s->adjoint_residual(s->adj_dot_y.data(), s->adj_dot_yp.data(), z + block + s->neq,
                        zp + block + s->neq, out + block);
    return 0;
  }

  static int quad_bs(sunrealtype t, N_Vector y, N_Vector yp, N_Vector *yS, N_Vector *ypS,
                     N_Vector zB, N_Vector, N_Vector qBdot, void *user_data) {
    auto *s = static_cast<ida_sens_state *>(user_data);
    const std::size_t block = s->neq + s->nd();
    const std::size_t np = s->p_vals.size();
    const sunrealtype *z = N_VGetArrayPointer(zB);
    s->vjp2(t, N_VGetArrayPointer(y), N_VGetArrayPointer(yp), N_VGetArrayPointer(yS[0]),
            N_VGetArrayPointer(ypS[0]), s->so->pdot.data(), z, z + block);
    sunrealtype *out = N_VGetArrayPointer(qBdot);
    for (std::size_t m = 0; m < np; ++m) {
      out[m] = s->adj_p[m];
      out[np + m] = s->adj_dot_p[m];
    }
    return 0;
  }

  // jump at t_k along d: E^T r_dot = A ybar_dot - D (dM/dd)^T r - A (dF_y/dd)^T r,
  // eta += (ybar_dot - F_y^T r_dot - (dF_y/dd)^T r)_d, pbar_dot_direct -= d/dd (F_p^T r)
  void jump2(std::size_t k, const std::vector<double> &ybar, const std::vector<double> &ybar_dot,
             double *nu, double *eta, std::vector<double> &pbar_direct,
             std::vector<double> &pbar_dot_direct) {
    second_order &o = *so;
    const double t = times[k];
    const double *y = o.ys.data() + k * neq;
    const double *yp = o.yps.data() + k * neq;
    const double *s = o.ss.data() + k * neq;
    const double *sp = o.sps.data() + k * neq;
    build_E(t, y, yp);
    std::vector<double> r(neq), r_dot(neq);
    const std::vector<double> zeros(neq, 0.0);
    for (std::size_t j = 0; j < neq; ++j) r[j] = id_vals[j] == 1.0 ? 0.0 : ybar[j * nt + k];
    solve_Et(r);
    vjp2(t, y, yp, s, sp, o.pdot.data(), r.data(), zeros.data());
    for (std::size_t j = 0; j < neq; ++j) {
      r_dot[j] = id_vals[j] == 1.0 ? -adj_dot_yp[j] : ybar_dot[j * nt + k] - adj_dot_y[j];
    }
    solve_Et(r_dot);
    vjp2(t, y, yp, s, sp, o.pdot.data(), r.data(), r_dot.data());
    for (std::size_t m = 0; m < nd(); ++m) {
      const std::size_t i = static_cast<std::size_t>(diff_idx[m]);
      nu[m] += ybar[i * nt + k] - adj_y[i];
      eta[m] += ybar_dot[i * nt + k] - adj_dot_y[i];
    }
    for (std::size_t m = 0; m < p_vals.size(); ++m) {
      pbar_direct[m] -= adj_p[m];
      pbar_dot_direct[m] -= adj_dot_p[m];
    }
  }

  // as consistent_first for [lambda; nu; mu; eta]:
  // E^T mu = [eta - (dM/dd)^T lambda; -(dF_y/dd)^T lambda], eta' = (F_y^T mu + (dF_y/dd)^T lambda)_d
  void consistent_second(std::size_t k, double *z, double *zp) {
    second_order &o = *so;
    const std::size_t block = neq + nd();
    const double t = times[k];
    const double *y = o.ys.data() + k * neq;
    const double *yp = o.yps.data() + k * neq;
    const double *s = o.ss.data() + k * neq;
    const double *sp = o.sps.data() + k * neq;
    std::vector<double> lam(neq), mu(neq);
    for (std::size_t i = 0; i < neq; ++i) {
      const int m = diff_pos[i];
      lam[i] = m >= 0 ? z[neq + static_cast<std::size_t>(m)] : 0.0;
    }
    solve_Et(lam);
    const std::vector<double> zeros(neq, 0.0);
    vjp2(t, y, yp, s, sp, o.pdot.data(), lam.data(), zeros.data());
    for (std::size_t i = 0; i < neq; ++i) {
      const int m = diff_pos[i];
      mu[i] = m >= 0 ? z[block + neq + static_cast<std::size_t>(m)] - adj_dot_yp[i] : -adj_dot_y[i];
    }
    solve_Et(mu);
    std::copy(lam.begin(), lam.end(), z);
    std::copy(mu.begin(), mu.end(), z + block);
    vjp2(t, y, yp, s, sp, o.pdot.data(), lam.data(), mu.data());
    std::fill(zp, zp + 2 * block, 0.0);
    for (std::size_t m = 0; m < nd(); ++m) {
      const std::size_t i = static_cast<std::size_t>(diff_idx[m]);
      zp[neq + m] = adj_y[i];
      zp[block + neq + m] = adj_dot_y[i];
    }
  }

  void init_backward2(second_order &o) {
    new_backward_vectors(o.w.ctx, 2 * (neq + nd()), 2 * p_vals.size(), 2, o.yB, o.ypB, o.idB, o.qB);
    etr::ass<"hessian (ida): IDACreateB failed">(IDACreateB(o.w.mem, &o.which) == IDA_SUCCESS);
    etr::ass<"hessian (ida): IDAInitBS failed">(
      IDAInitBS(o.w.mem, o.which, &res_bs, times[nt - 1], o.yB, o.ypB) == IDA_SUCCESS);
    etr::ass<"hessian (ida): IDAQuadInitBS failed">(
      IDAQuadInitBS(o.w.mem, o.which, &quad_bs, o.qB) == IDA_SUCCESS);
    setup_backward(o.w.mem, o.which, o.yB, o.idB, o.AB, o.LSB, o.w.ctx);
  }

  // out_bar / out_bar_dot: column-major nt x neq; in_* = (y0 part, p part)
  void backward2(const std::vector<double> &out_bar, const std::vector<double> &out_bar_dot,
                 std::vector<double> &in_bar, std::vector<double> &in_bar_dot) {
    etr::ass<"hessian (ida): backward pass without a tangent pass">(so != nullptr);
    second_order &o = *so;
    const std::size_t np = p_vals.size();
    const std::size_t block = neq + nd();
    if (o.which < 0) init_backward2(o);
    sunrealtype *z = N_VGetArrayPointer(o.yB);
    N_VConst(0.0, o.yB);
    N_VConst(0.0, o.qB);
    std::vector<double> pbar_direct(np, 0.0), pbar_dot_direct(np, 0.0);
    jump2(nt - 1, out_bar, out_bar_dot, z + neq, z + block + neq, pbar_direct, pbar_dot_direct);
    for (std::size_t k = nt - 1; k >= 1; --k) {
      consistent_second(k, z, N_VGetArrayPointer(o.ypB));
      vjp2_valid = false;
      etr::ass<"hessian (ida): IDAReInitB failed">(
        IDAReInitB(o.w.mem, o.which, times[k], o.yB, o.ypB) == IDA_SUCCESS);
      etr::ass<"hessian (ida): IDAQuadReInitB failed">(
        IDAQuadReInitB(o.w.mem, o.which, o.qB) == IDA_SUCCESS);
      check_backward_flag(IDASolveB(o.w.mem, times[k - 1], IDA_NORMAL), "backward integration",
                          times[k]);
      sunrealtype tret = 0.0;
      etr::ass<"hessian (ida): IDAGetB failed">(
        IDAGetB(o.w.mem, o.which, &tret, o.yB, o.ypB) == IDA_SUCCESS);
      etr::ass<"hessian (ida): IDAGetQuadB failed">(
        IDAGetQuadB(o.w.mem, o.which, &tret, o.qB) == IDA_SUCCESS);
      jump2(k - 1, out_bar, out_bar_dot, z + neq, z + block + neq, pbar_direct, pbar_dot_direct);
    }
    const sunrealtype *q = N_VGetArrayPointer(o.qB);
    for (std::size_t i = 0; i < neq; ++i) {
      const int m = diff_pos[i];
      in_bar[i] = m >= 0 ? z[neq + static_cast<std::size_t>(m)] : 0.0;
      in_bar_dot[i] = m >= 0 ? z[block + neq + static_cast<std::size_t>(m)] : 0.0;
    }
    for (std::size_t m = 0; m < np; ++m) {
      in_bar[neq + m] = q[m] + pbar_direct[m];
      in_bar_dot[neq + m] = q[np + m] + pbar_dot_direct[m];
    }
  }

  static int fwd_res(sunrealtype t, N_Vector y, N_Vector yp, N_Vector r, void *user_data) {
    return res_trampoline<Ctx>(t, y, yp, r, &static_cast<ida_sens_state *>(user_data)->ctx);
  }
  static int fwd_jac(sunrealtype t, sunrealtype cj, N_Vector y, N_Vector yp, N_Vector r,
                     SUNMatrix J, void *user_data, N_Vector tmp1, N_Vector tmp2, N_Vector tmp3) {
    return ida_jac_trampoline<Ctx>(t, cj, y, yp, r, J,
                                   &static_cast<ida_sens_state *>(user_data)->ctx, tmp1, tmp2, tmp3);
  }
  static int fwd_sens(int Ns, sunrealtype t, N_Vector y, N_Vector yp, N_Vector r, N_Vector *yS,
                      N_Vector *ypS, N_Vector *resS, void *user_data, N_Vector tmp1,
                      N_Vector tmp2, N_Vector tmp3) {
    auto *s = static_cast<ida_sens_state *>(user_data);
    const std::size_t np = s->p_vals.size();
    for (int i = 0; i < Ns; ++i) {
      const int a = s->active[static_cast<std::size_t>(i)];
      for (std::size_t m = 0; m < np; ++m) {
        const double d = a == static_cast<int>(s->neq + m) ? 1.0 : 0.0;
        s->p_dual.set(m, etr::Dual(s->p_vals[m], d));
      }
      const int flag = sens_res_trampoline<DualCtx>(1, t, y, yp, r, &yS[i], &ypS[i], &resS[i],
                                                    &s->dual_ctx, tmp1, tmp2, tmp3);
      if (flag != 0) return flag;
    }
    return 0;
  }

  void store(std::vector<double> &vals, std::size_t k) {
    const sunrealtype *y_ptr = N_VGetArrayPointer(w.y);
    for (std::size_t j = 0; j < neq; ++j) vals[j * nt + k] = y_ptr[j];
    const std::size_t ns = active.size();
    for (std::size_t i = 0; i < ns; ++i) {
      const sunrealtype *sp = N_VGetArrayPointer(w.yS[i]);
      std::copy(sp, sp + neq, sens.begin() + static_cast<std::ptrdiff_t>((k * ns + i) * neq));
    }
  }

  // in_bar_i = sum_k S_i(t_k)^T ybar(t_k), ybar column-major nt x neq
  void backward(const std::vector<double> &ybar, std::vector<double> &in_bar) const {
    const std::size_t ns = active.size();
    in_bar.assign(ns, 0.0);
    for (std::size_t k = 0; k < nt; ++k) {
      for (std::size_t i = 0; i < ns; ++i) {
        const double *S = sens.data() + (k * ns + i) * neq;
        double acc = 0.0;
        for (std::size_t j = 0; j < neq; ++j) acc += S[j] * ybar[j * nt + k];
        in_bar[i] += acc;
      }
    }
  }
};

template <typename JAC, typename RES, typename Y, typename YP, typename T, typename P, typename ID>
requires(
etr::IsArray<etr::Decayed<Y>> && etr::IsArray<etr::Decayed<YP>> &&
etr::IsArray<etr::Decayed<T>> && etr::IsArray<etr::Decayed<ID>>
)
inline auto idas_reverse_impl(const RES &, const JAC *, const Y &y0, const YP &yp0,
                              const T &times, const P &params, const ID &id,
                              const etr::ida_options &opt) {
  static_assert(etr::IsArray<etr::Decayed<P>>,
                "ida: in reverse mode params has to be a double vector");
  using State = ida_sens_state<RES, JAC>;
  const std::size_t neq = y0.size();
  const std::size_t nt = times.size();
  const std::size_t np = params.size();
  etr::ass<"ida: need at least one initial state">(neq > 0);
  etr::ass<"ida: y0, yp0 and id must have the same length">(
    yp0.size() == neq && id.size() == neq);
  etr::ass<"ida: need at least two time points">(nt >= 2);
  etr::ass<"ida: in reverse mode params needs at least one element">(np > 0);
  check_tolerances(opt, "ida");
  const long int max_steps = checked_max_steps(opt, "ida");
  etr::ass<"ida: stats must be TRUE or FALSE">(!opt.stats.isNA());
  const int linear_solver = checked_linear_solver(opt, "ida");

  auto s = std::make_shared<State>();
  s->neq = neq;
  s->nt = nt;
  s->p_vals.resize(np);
  s->p_ids.resize(np);
  s->p_double = DoubleVec(etr::SI{np});
  s->p_dual = typename State::DualVec(etr::SI{np});
  for (std::size_t i = 0; i < np; ++i) {
    etr::ReverseDouble pi = params.get(i);
    etr::ass<"ida: NA in params">(!pi.isNA());
    s->p_vals[i] = etr::get_val(pi);
    s->p_ids[i] = pi.id;
    s->p_double.set(i, etr::Double(s->p_vals[i]));
    s->p_dual.set(i, etr::Dual(s->p_vals[i], 0.0));
  }
  s->ctx.f = &s->res_d;
  s->ctx.params = &s->p_double;
  if constexpr (!std::is_same_v<JAC, no_jac>) s->ctx.jac = &s->jac_d;
  s->dual_ctx.f = &s->res_dual;
  s->dual_ctx.params = &s->p_dual;
  dual_res_ctx_init(s->dual_ctx, neq, false);

  idas_workspace &w = s->w;
  etr::ass<"ida: SUNContext_Create failed">(SUNContext_Create(SUN_COMM_NULL, &w.ctx) == 0);
  SUNContext_ClearErrHandlers(w.ctx);
  const auto n = static_cast<sunindextype>(neq);
  w.y = N_VNew_Serial(n, w.ctx);
  w.yp = N_VNew_Serial(n, w.ctx);
  w.id = N_VNew_Serial(n, w.ctx);
  etr::ass<"ida: N_VNew_Serial failed">(w.y != nullptr && w.yp != nullptr && w.id != nullptr);
  values_to_nvector(y0, w.y, neq);
  values_to_nvector(yp0, w.yp, neq);
  values_to_nvector(id, w.id, neq);
  check_ida_id(w.id, neq);
  s->y0_ids.resize(neq);
  for (std::size_t i = 0; i < neq; ++i) {
    etr::ReverseDouble yi = y0.get(i);
    s->y0_ids[i] = yi.id;
  }

  // inputs that are tape constants get no sensitivity
  const auto &tape_ops = etr::TAPE_INTERN_ref().op;
  auto is_active = [&tape_ops](int tape_id) {
    return tape_id >= 0 && tape_ops[static_cast<std::size_t>(tape_id)] != etr::ROp::Const;
  };
  for (std::size_t i = 0; i < neq; ++i) {
    if (is_active(s->y0_ids[i])) s->active.push_back(static_cast<int>(i));
  }
  for (std::size_t i = 0; i < np; ++i) {
    if (is_active(s->p_ids[i])) s->active.push_back(static_cast<int>(neq + i));
  }
  const int mode = etr::get_val(opt.sensitivity);
  const bool adjoint = mode == etr::sensitivity_adjoint ||
    (mode == etr::sensitivity_auto && s->active.size() > etr::forward_sens_max_inputs);
  if (adjoint) s->active.clear();
  // also needed by the hessian (tangent / backward2) in both modes
  s->times.resize(nt);
  for (std::size_t k = 0; k < nt; ++k) s->times[k] = etr::get_val(times.get(k));
  s->id_vals.assign(N_VGetArrayPointer(w.id), N_VGetArrayPointer(w.id) + neq);
  s->y0_vals.assign(N_VGetArrayPointer(w.y), N_VGetArrayPointer(w.y) + neq);
  s->yp0_vals.assign(N_VGetArrayPointer(w.yp), N_VGetArrayPointer(w.yp) + neq);
  s->set_layout();
  s->reltol = etr::get_val(opt.reltol);
  s->abstol = etr::get_val(opt.abstol);
  s->max_steps = max_steps;
  s->linear_solver = linear_solver;
  const int ns = static_cast<int>(s->active.size());
  if (ns > 0) {
    w.yS = N_VCloneVectorArray(ns, w.y);
    w.ypS = N_VCloneVectorArray(ns, w.y);
    etr::ass<"ida: N_VCloneVectorArray failed">(w.yS != nullptr && w.ypS != nullptr);
    w.ns = ns;
    for (int i = 0; i < ns; ++i) {
      N_VConst(0.0, w.yS[i]);
      N_VConst(0.0, w.ypS[i]);
      const int a = s->active[static_cast<std::size_t>(i)];
      if (a < static_cast<int>(neq)) N_VGetArrayPointer(w.yS[i])[a] = 1.0;
    }
  }

  if constexpr (std::is_same_v<JAC, no_jac>) {
    idas_setup(w, &State::fwd_res, IDALsJacFn{nullptr}, s.get(), etr::get_val(times.get(0)),
               etr::get_val(opt.reltol), etr::get_val(opt.abstol), max_steps, linear_solver);
  } else {
    idas_setup(w, &State::fwd_res, &State::fwd_jac, s.get(), etr::get_val(times.get(0)),
               etr::get_val(opt.reltol), etr::get_val(opt.abstol), max_steps, linear_solver);
  }
  if (ns > 0) idas_sens_init(w, &State::fwd_sens);
  // checkpoint every 100 steps, Hermite interpolation of the forward solution
  if (adjoint) {
    etr::ass<"ida: IDAAdjInit failed">(IDAAdjInit(w.mem, 100, IDA_HERMITE) == IDA_SUCCESS);
  }
  idas_consistent_ic(w, etr::get_val(times.get(1)));

  std::vector<double> vals(neq * nt);
  std::vector<uint8_t> na(neq * nt, 0);
  s->sens.assign(nt * static_cast<std::size_t>(ns) * neq, 0.0);
  if (adjoint) {
    s->ys.resize(nt * neq);
    s->yps.resize(nt * neq);
  }
  auto store = [&](std::size_t k) {
    s->store(vals, k);
    if (!adjoint) return;
    const sunrealtype *y_ptr = N_VGetArrayPointer(w.y);
    const sunrealtype *yp_ptr = N_VGetArrayPointer(w.yp);
    std::copy(y_ptr, y_ptr + neq, s->ys.begin() + static_cast<std::ptrdiff_t>(k * neq));
    std::copy(yp_ptr, yp_ptr + neq, s->yps.begin() + static_cast<std::ptrdiff_t>(k * neq));
  };
  store(0);
  for (std::size_t k = 1; k < nt; ++k) {
    if (adjoint) {
      sunrealtype tret = 0.0;
      int ncheck = 0;
      int flag = IDASolveF(w.mem, etr::get_val(times.get(k)), &tret, w.y, w.yp, IDA_NORMAL, &ncheck);
      etr::ass(flag == IDA_SUCCESS, std::string("ida: integration failed at t = ") +
                                      std::to_string(tret) + ": " +
                                      flag_name(IDAGetReturnFlagName(flag)));
    } else {
      idas_step(w, etr::get_val(times.get(k)));
    }
    store(k);
  }
  if (etr::get_val(opt.stats)) print_ida_stats(w.mem);

  std::vector<int> input_ids = s->y0_ids;
  input_ids.insert(input_ids.end(), s->p_ids.begin(), s->p_ids.end());
  const int head = etr::TAPE_INTERN_ref().push_custom(vals, na, std::move(input_ids),
    [s, adjoint](etr::ReverseTape &tape, int out0) {
      const std::size_t n_out = s->neq * s->nt;
      std::vector<double> ybar(n_out);
      bool any = false;
      for (std::size_t i = 0; i < n_out; ++i) {
        const std::size_t tape_id = static_cast<std::size_t>(out0) + i;
        ybar[i] = tape.is_na[tape_id] ? 0.0 : tape.adj[tape_id];
        if (ybar[i] != 0.0) any = true;
      }
      if (!any) return;
      if (adjoint) {
        std::vector<double> y0bar, pbar;
        s->backward_adjoint(ybar, y0bar, pbar);
        for (std::size_t i = 0; i < s->neq; ++i) {
          if (s->y0_ids[i] >= 0) tape.adj[static_cast<std::size_t>(s->y0_ids[i])] += y0bar[i];
        }
        for (std::size_t i = 0; i < pbar.size(); ++i) {
          if (s->p_ids[i] >= 0) tape.adj[static_cast<std::size_t>(s->p_ids[i])] += pbar[i];
        }
        return;
      }
      std::vector<double> in_bar;
      s->backward(ybar, in_bar);
      for (std::size_t i = 0; i < in_bar.size(); ++i) {
        const std::size_t a = static_cast<std::size_t>(s->active[i]);
        const int tape_id = a < s->neq ? s->y0_ids[a] : s->p_ids[a - s->neq];
        tape.adj[static_cast<std::size_t>(tape_id)] += in_bar[i];
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

template <typename JAC, typename RES, typename Y, typename YP, typename T, typename P, typename ID>
inline auto ida_dispatch(const RES &res_fct, const JAC *jac, const Y &y0, const YP &yp0,
                         const T &times, const P &params, const ID &id,
                         const etr::ida_options &opt) {
  using TY = typename etr::ExtractDataType<etr::Decayed<Y>>::value_type;
  if constexpr (etr::IS<TY, etr::ReverseDouble>) {
    return idas_reverse_impl(res_fct, jac, y0, yp0, times, params, id, opt);
  } else if constexpr (etr::IS<TY, etr::Dual>) {
    return idas_forward_impl(res_fct, jac, y0, yp0, times, params, id, opt);
  } else {
    return ida_impl(res_fct, jac, y0, yp0, times, params, id, opt);
  }
}

template <typename RES, typename Y, typename YP, typename T, typename P, typename ID>
inline auto ida_solve(const RES &res_fct, const Y &y0, const YP &yp0, const T &times,
                      const P &params, const ID &id, const etr::ida_options &opt) {
  return ida_dispatch<no_jac>(res_fct, nullptr, y0, yp0, times, params, id, opt);
}

template <typename RES, typename Y, typename YP, typename T, typename P, typename ID,
          typename JAC>
inline auto ida_solve(const RES &res_fct, const Y &y0, const YP &yp0, const T &times,
                      const P &params, const ID &id, const etr::ida_options &opt,
                      const JAC &jac) {
  return ida_dispatch(res_fct, &jac, y0, yp0, times, params, id, opt);
}

} // namespace a2a

#endif
