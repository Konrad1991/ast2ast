#ifndef A2A_SUNDIALS_HPP
#define A2A_SUNDIALS_HPP

#ifndef SUNDIALS_AVAILABLE
#error "cvode() requires the 'sundials' package (compile() adds -DSUNDIALS_AVAILABLE)"
#endif

#include <cvode/cvode.h>
#include <nvector/nvector_serial.h>
#include <sunlinsol/sunlinsol_dense.h>
#include <sunmatrix/sunmatrix_dense.h>

#include <functional>
#include <vector>

namespace a2a {

using BorrowVec = etr::Array<etr::Double, etr::Borrow<etr::Double>>;

// RHS: f(t, y, ydot, params). params is an opaque passthrough (double vector,
// scalar, or a new_type struct) -- cvode_solve never inspects it, so it is
// templated on P and type-erases the callable. The RHS need only be callable
// as (Double&, BorrowVec&, BorrowVec&, P&); ref()/const() spelling is free.
template <typename P>
using ode_rhs_t = std::function<void(etr::Double &, BorrowVec &, BorrowVec &, P &)>;

template <typename P> struct rhs_ctx {
  ode_rhs_t<P> f;
  P *params;
};

template <typename P>
int rhs_trampoline(sunrealtype t, N_Vector y, N_Vector ydot, void *user_data) {
  auto *ctx = static_cast<rhs_ctx<P> *>(user_data);
  sunrealtype *yp = N_VGetArrayPointer(y);
  sunrealtype *ydp = N_VGetArrayPointer(ydot);
  if (yp == nullptr || ydp == nullptr) return -1;
  const std::size_t n = static_cast<std::size_t>(NV_LENGTH_S(y));
  BorrowVec yv(yp, n, std::vector<std::size_t>{n});
  BorrowVec ydv(ydp, n, std::vector<std::size_t>{n});
  etr::Double tt = static_cast<double>(t);
  ctx->f(tt, yv, ydv, *ctx->params);
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
  rhs_ctx<P> ctx;
  ctx.f = rhs;
  ctx.params = &p_local;

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
  etr::ass<"cvode: CVodeInit failed">(CVodeInit(w.mem, &rhs_trampoline<P>, t0, w.y) == CV_SUCCESS);
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
  res.dim = std::vector<std::size_t>{nt, neq};
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

} // namespace a2a

#endif
