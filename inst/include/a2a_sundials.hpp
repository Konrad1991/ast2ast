#ifndef A2A_SUNDIALS_HPP
#define A2A_SUNDIALS_HPP

#include <cvodes/cvodes.h>
#include <idas/idas.h>
#include <nvector/nvector_serial.h>
#include <sunlinsol/sunlinsol_dense.h>
#include <sunmatrix/sunmatrix_dense.h>

#include <vector>

namespace a2a {

using BorrowVec = etr::Array<etr::Double, etr::Borrow<etr::Double>>;

// RHS: functor f(t, y, ydot, params). params is an opaque passthrough (double
// vector, scalar, or a new_type struct) -- cvode_solve never inspects it.
template <typename F, typename P> struct fct_ctx {
  const F *f;
  P *params;
};

template <typename F, typename P>
int rhs_trampoline(sunrealtype t, N_Vector y, N_Vector ydot, void *user_data) {
  auto *ctx = static_cast<fct_ctx<F, P> *>(user_data);
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

// cvode(rhs, y0, times, params, reltol, abstol, stiff) -> length(times) x
// length(y0) matrix, row k = state at times[k] (row 0 = y0, times[0] = t0).
// stiff != 0 -> BDF, else ADAMS. Dense Newton, difference-quotient Jacobian.
template <typename RHS, typename Y, typename T, typename P, typename StiffT>
requires(
etr::IsArray<etr::Decayed<Y>> && etr::IsArray<etr::Decayed<T>> &&
etr::IsScalarLike<etr::Decayed<StiffT>>
)
inline auto cvode_solve(const RHS &rhs, const Y &y0, const T &times,
                        const P &params, etr::Double reltol, etr::Double abstol,
                        StiffT stiff) {
  const std::size_t neq = y0.size();
  const std::size_t nt = times.size();
  etr::ass<"cvode: need at least one initial state">(neq > 0);
  etr::ass<"cvode: need at least two time points">(nt >= 2);
  etr::ass<"cvode: reltol and abstol must be positive">(
    !reltol.isNA() && !abstol.isNA() && etr::get_val(reltol) > 0.0 &&
    etr::get_val(abstol) > 0.0);

  P p_local = params; // fixed for the whole integration
  fct_ctx<RHS, P> ctx{&rhs, &p_local};

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

  const int lmm = (etr::get_val(stiff) != 0) ? CV_BDF : CV_ADAMS;
  w.mem = CVodeCreate(lmm, w.ctx);
  etr::ass<"cvode: CVodeCreate failed">(w.mem != nullptr);

  double t0 = etr::get_val(times.get(0));
  etr::ass<"cvode: CVodeInit failed">(CVodeInit(w.mem, &rhs_trampoline<RHS, P>, t0, w.y) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSStolerances failed">(CVodeSStolerances(w.mem, etr::get_val(reltol), etr::get_val(abstol)) == CV_SUCCESS);
  etr::ass<"cvode: CVodeSetUserData failed">(CVodeSetUserData(w.mem, &ctx) == CV_SUCCESS);

  w.A = SUNDenseMatrix(static_cast<sunindextype>(neq),
                       static_cast<sunindextype>(neq), w.ctx);
  etr::ass<"cvode: SUNDenseMatrix failed">(w.A != nullptr);
  w.LS = SUNLinSol_Dense(w.y, w.A, w.ctx);
  etr::ass<"cvode: SUNLinSol_Dense failed">(w.LS != nullptr);
  etr::ass<"cvode: CVodeSetLinearSolver failed">(
    CVodeSetLinearSolver(w.mem, w.LS, w.A) == CV_SUCCESS);

  etr::Array<etr::Double, etr::Buffer<etr::Double, etr::RBufferTrait>> res(etr::SI{neq * nt});
  res.dim = etr::Dim{nt, neq};
  for (std::size_t j = 0; j < neq; ++j) {
    res.set(j * nt + 0, etr::Double(yp[j]));
  }
  for (std::size_t k = 1; k < nt; ++k) {
    sunrealtype tret = 0.0;
    int flag = CVode(w.mem, etr::get_val(times.get(k)), w.y, &tret, CV_NORMAL);
    etr::ass<"cvode: integration failed before an output time">(flag == CV_SUCCESS);
    for (std::size_t j = 0; j < neq; ++j) {
      res.set(j * nt + k, etr::Double(yp[j]));
    }
  }
  return res;
}

template <typename F, typename P>
int res_trampoline(sunrealtype t, N_Vector y, N_Vector yp, N_Vector r, void *user_data) {
  auto *ctx = static_cast<fct_ctx<F, P> *>(user_data);
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

template <typename Vec>
void copy_to_nvector(const Vec &src, N_Vector dst, std::size_t n) {
  sunrealtype *ptr = N_VGetArrayPointer(dst);
  for (std::size_t i = 0; i < n; ++i) {
    etr::Double v = src.get(i);
    etr::ass<"ida: NA in y0, yp0 or id">(!v.isNA());
    ptr[i] = etr::get_val(v);
  }
}

// ida(res, y0, yp0, times, params, reltol, abstol, id) -> length(times) x
// length(y0) matrix of y. id: 1 = differential, 0 = algebraic.
// yp0 (and the algebraic part of y0) is made consistent via IDACalcIC.
template <typename RES, typename Y, typename YP, typename T, typename P, typename ID>
requires(
etr::IsArray<etr::Decayed<Y>> && etr::IsArray<etr::Decayed<YP>> &&
etr::IsArray<etr::Decayed<T>> && etr::IsArray<etr::Decayed<ID>>
)
inline auto ida_solve(const RES &res_fct, const Y &y0, const YP &yp0, const T &times,
                      const P &params, etr::Double reltol, etr::Double abstol,
                      const ID &id) {
  const std::size_t neq = y0.size();
  const std::size_t nt = times.size();
  etr::ass<"ida: need at least one initial state">(neq > 0);
  etr::ass<"ida: y0, yp0 and id must have the same length">(
    yp0.size() == neq && id.size() == neq);
  etr::ass<"ida: need at least two time points">(nt >= 2);
  etr::ass<"ida: reltol and abstol must be positive">(
    !reltol.isNA() && !abstol.isNA() && etr::get_val(reltol) > 0.0 &&
    etr::get_val(abstol) > 0.0);

  P p_local = params;
  fct_ctx<RES, P> ctx{&res_fct, &p_local};

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
  etr::ass<"ida: IDAInit failed">(IDAInit(w.mem, &res_trampoline<RES, P>, t0, w.y, w.yp) == IDA_SUCCESS);
  etr::ass<"ida: IDASStolerances failed">(IDASStolerances(w.mem, etr::get_val(reltol), etr::get_val(abstol)) == IDA_SUCCESS);
  etr::ass<"ida: IDASetUserData failed">(IDASetUserData(w.mem, &ctx) == IDA_SUCCESS);
  etr::ass<"ida: IDASetId failed">(IDASetId(w.mem, w.id) == IDA_SUCCESS);

  w.A = SUNDenseMatrix(n, n, w.ctx);
  etr::ass<"ida: SUNDenseMatrix failed">(w.A != nullptr);
  w.LS = SUNLinSol_Dense(w.y, w.A, w.ctx);
  etr::ass<"ida: SUNLinSol_Dense failed">(w.LS != nullptr);
  etr::ass<"ida: IDASetLinearSolver failed">(IDASetLinearSolver(w.mem, w.LS, w.A) == IDA_SUCCESS);

  etr::ass<"ida: IDACalcIC failed (no consistent initial values found)">(
    IDACalcIC(w.mem, IDA_YA_YDP_INIT, etr::get_val(times.get(1))) == IDA_SUCCESS);
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
    etr::ass<"ida: integration failed before an output time">(flag == IDA_SUCCESS);
    for (std::size_t j = 0; j < neq; ++j) {
      res.set(j * nt + k, etr::Double(y_ptr[j]));
    }
  }
  return res;
}

} // namespace a2a

#endif
