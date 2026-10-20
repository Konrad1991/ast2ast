#ifndef DERIVATIVES_ETR_HPP
#define DERIVATIVES_ETR_HPP

namespace etr {

template<std::size_t YIdx, std::size_t XIdx, typename F, typename... Args>
struct BoundCall {
  std::tuple<F, Args...> t;

  template<class FF, class... AA> BoundCall(FF&& f, AA&&... args) : t(std::forward<FF>(f), std::forward<AA>(args)...) {}

  decltype(auto) operator()() {
    return std::apply([](auto& f, auto&... args) -> decltype(auto) {
      return std::invoke(f, args...);
    }, t);
  }

  decltype(auto) x()       { return std::get<XIdx + 1>(t); }
  decltype(auto) y()       { return std::get<YIdx + 1>(t); }
  decltype(auto) x() const { return std::get<XIdx + 1>(t); }
  decltype(auto) y() const { return std::get<YIdx + 1>(t); }
};

template<std::size_t YIdx, std::size_t XIdx, typename F, typename... Args>
auto make_bound_call(F&& f, Args&&... args) {
  using FF = Decayed<F>;
  using BC = BoundCall<YIdx, XIdx, FF, Decayed<Args>...>;
  return BC(std::forward<F>(f), std::forward<Args>(args)...);
}

template<typename I> inline std::size_t calc_idx_seed(const I& idx) {
  using DecayedI = Decayed<I>;
  if constexpr (IsScalarLike<DecayedI>) {
    return static_cast<std::size_t>(get_val(idx)) - 1;
  } else if constexpr (IsArray<DecayedI>) {
    ass<"Size of idx in seed has to be 1L">(idx.size() == 1);
    return static_cast<std::size_t>(get_val(idx.get(0))) - 1;
  }
}

template<typename T, typename I> requires IsArray<T> inline void seed(T& obj, const I& idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  const std::size_t i = calc_idx_seed(idx);
  obj.d.set_dot(i, 1.0);
}
template<typename T, typename I> requires IsArray<T> inline void unseed(T& obj, const I& idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  const std::size_t i = calc_idx_seed(idx);
  obj.d.set_dot(i, 0.0);
}
template<typename T, typename I> requires IsDual<T> inline void seed(T& obj, const I& idx) {
  const std::size_t i = calc_idx_seed(idx);
  ass<"Wrong index in seed">(i == 0);
  obj.dot = 1.0;
}
template<typename T, typename I> requires IsDual<T> inline void unseed(T& obj, const I& idx) {
  const std::size_t i = calc_idx_seed(idx);
  ass<"Wrong index in seed">(i == 0);
  obj.dot = 0.0;
}

template<typename T> requires IsDual<T> inline auto get_dot(const T& obj) {
  Array<Double, Buffer<Double, RBufferTrait>> res(SI{1});
  res.dim = Dim{1};
  res.set(0, Double(obj.dot));
  return res;
}
// [[ / at on a Dual array yields a DualRef, not a Dual -- template deduction
// pins T to DualRef before its operator Dual() conversion could apply, so
// the IsDual<T> overload above never matches it. Convert explicitly instead.
inline auto get_dot(const DualRef& obj) {
  return get_dot(Dual(obj));
}
template<typename T> requires IsArray<T> inline auto get_dot(const T& obj) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  Array<Double, Buffer<Double, RBufferTrait>> res(SI{obj.size()});
  for (std::size_t i = 0; i < res.size(); i++) {
    res.set(i, obj.d.get_dot(i));
  }
  res.dim = Dim{obj.size()};
  return res;
}

template<typename T> requires IsArray<T> void seed(T& obj, std::size_t idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  obj.d.set_dot(idx, 1.0);
}
template<typename T> requires IsArray<T> void unseed(T& obj, std::size_t idx) {
  using DataType = typename ExtractDataType<Decayed<T>>::value_type;
  static_assert(IS<DataType, Dual>, "data type of obj has to be Dual");
  obj.d.set_dot(idx, 0.0);
}

template<std::size_t YIdx, std::size_t XIdx, typename J, typename Fun, typename... Args> requires (IsArray<J>)
void jacobian_forward(J& jac, const Fun& fct, Args&&... args) {
  auto bc = make_bound_call<YIdx, XIdx>(fct, args...);
  auto x = bc.x().get();
  auto y = bc.y().get();
  const std::size_t nrow = y.size();
  const std::size_t ncol = x.size();
  jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
  for (std::size_t i = 0; i < ncol; i++) {
    seed(x, i);
    bc();
    for (std::size_t j = 0; j < nrow; j++) {
      jac.set( i * nrow + j, y.d.get_dot(j));
    }
    unseed(x, i);
  }
}

// deriv(of, wrt): reverse-mode Jacobian extraction from the live tape.
// Each TAPE_INTERN.reverse(out_id) zeros all adjoints, seeds adj[out_id]=1,
// and propagates — yielding one full row of the Jacobian (one output vs. all
// inputs) per sweep. The tape itself is built during ordinary evaluation
// before this call; deriv() does not push any new nodes.
//
// Required invariant: for an Array<ReverseDouble, ...> argument, .size() and
// .get(i) must be pure reads of buffer state — they must not push new nodes
// onto TAPE_INTERN. If they did, the tape would grow between successive
// reverse() calls, which would (a) shift node indices the previously cached
// .id values still refer to, and (b) make adj[] reads land in nodes that
// were not part of the computation we're differentiating. Today
// Buffer<ReverseDouble> stores std::vector<ReverseDouble> and get(i) returns a
// reference to the stored handle (a plain read), so the invariant holds. If a
// future Buffer specialization lazily materializes nodes on access, snapshot
// the ids into a local std::vector<int> before the reverse-sweep loops below.
template<typename Of, typename Wrt> inline auto deriv(const Of& of, const Wrt& w_inp) {
  using DecayedOf = Decayed<Of>;
  using DecayedWrt = Decayed<Wrt>;
  if constexpr (IsReverseDouble<DecayedOf> && IsReverseDouble<DecayedWrt>) {
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{1});
    res.dim = Dim{1};
    TAPE_INTERN.reverse(of.id);
    res.set(0, Double(TAPE_INTERN.adj[static_cast<std::size_t>(w_inp.id)]));
    return res;
  }
  else if constexpr (IsArray<DecayedOf> && IsReverseDouble<DecayedWrt>) {
    using DataTypeOf = typename ExtractDataType<DecayedOf>::value_type;
    static_assert(IsReverseDouble<DataTypeOf>, "data type of of has to be ReverseDouble");
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{of.size()});
    res.dim = Dim{of.size()};
    for (std::size_t j = 0; j < res.size(); j++) {
      TAPE_INTERN.reverse(of.get(j).id);
      res.set(j, Double(TAPE_INTERN.adj[static_cast<std::size_t>(w_inp.id)]));
    }
    return res;
  }
  else if constexpr (IsReverseDouble<DecayedOf> && IsArray<DecayedWrt>) {
    using DataTypeWrt = typename ExtractDataType<DecayedWrt>::value_type;
    static_assert(IsReverseDouble<DataTypeWrt>, "data type of wrt has to be ReverseDouble");
    Array<Double, Buffer<Double, RBufferTrait>> res(SI{w_inp.size()});
    res.dim = w_inp.get_dim(); // keep wrt's shape: deriv(scalar, matrix) -> matrix
    TAPE_INTERN.reverse(of.id);
    for (std::size_t i = 0; i < res.size(); i++) {
      res.set(i, Double(TAPE_INTERN.adj[static_cast<std::size_t>(w_inp.get(i).id)]));
    }
    return res;
  }
  else if constexpr (IsArray<DecayedOf> && IsArray<DecayedWrt>) {
    using DataTypeWrt = typename ExtractDataType<DecayedWrt>::value_type;
    static_assert(IsReverseDouble<DataTypeWrt>, "data type of wrt has to be ReverseDouble");
    using DataTypeOf = typename ExtractDataType<DecayedOf>::value_type;
    static_assert(IsReverseDouble<DataTypeOf>, "data type of of has to be ReverseDouble");

    const std::size_t nrow = of.size();
    const std::size_t ncol = w_inp.size();
    Array<Double, Buffer<Double, RBufferTrait>> res = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
    for (std::size_t j = 0; j < nrow; j++) {
      TAPE_INTERN.reverse(of.get(j).id);
      for (std::size_t i = 0; i < ncol; i++) {
        res.set(i * nrow + j, Double(TAPE_INTERN.adj[static_cast<std::size_t>(w_inp.get(i).id)]));
      }
    }
    return res;
  }
}

template<std::size_t YIdx, std::size_t XIdx, typename J, typename Fun, typename... Args> requires (IsArray<J>)
void jacobian_backward(J& jac, const Fun& fct, Args&&... args) {
  auto bc = make_bound_call<YIdx, XIdx>(fct, args...);
  const auto x = bc.x().get();
  auto res = bc();
  const std::size_t nrow = res.size();
  const std::size_t ncol = x.size();
  jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
  for (std::size_t j = 0; j < nrow; j++) {
    TAPE_INTERN.reverse(res.get(j).id);
    for (std::size_t i = 0; i < ncol; i++) {
      jac.set(i * nrow + j, Double(TAPE_INTERN.adj[static_cast<std::size_t>(x.get(i).id)]));
    }
  }
}

// jacobian(f, x[, extra]): m-by-n Jacobian of f at x, column-major like R.
// Dispatch on x's scalar type -- forward mode for Dual, reverse mode for
// ReverseDouble. `extra` is threaded through to f unchanged (as in uniroot).
// A scalar-valued f is allowed too: the result is the 1-by-n gradient row
// (used by lbfgsb for the objective's gradient).
template<typename X, typename Call>
inline auto jacobian_impl(const X& x, Call&& call_f) {
  using XT = typename ExtractDataType<Decayed<X>>::value_type;
  const std::size_t ncol = x.size();

  if constexpr (IsReverseDouble<XT>) {
    Array<ReverseDouble, Buffer<ReverseDouble>> xr(SI{ncol});
    for (std::size_t i = 0; i < ncol; i++) xr.set(i, ReverseDouble::Var(get_val(x.get(i))));

    auto y = call_f(xr);
    // scalar-valued f -> one gradient row (1 x ncol); vector-valued f -> full m x n
    if constexpr (IsArray<Decayed<decltype(y)>>) {
      const std::size_t nrow = y.size();
      auto jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
      for (std::size_t j = 0; j < nrow; j++) {
        TAPE_INTERN.reverse(y.get(j).id);
        for (std::size_t i = 0; i < ncol; i++) {
          jac.set(i * nrow + j, Double(TAPE_INTERN.adj[static_cast<std::size_t>(xr.get(i).id)]));
        }
      }
      return jac;
    } else {
      auto jac = matrix(Double(0.0), Integer(1), Integer(static_cast<int>(ncol)));
      TAPE_INTERN.reverse(y.id);
      for (std::size_t i = 0; i < ncol; i++) {
        jac.set(i, Double(TAPE_INTERN.adj[static_cast<std::size_t>(xr.get(i).id)]));
      }
      return jac;
    }
  } else {
    Array<Dual, Buffer<Dual>> xd(SI{ncol});
    for (std::size_t i = 0; i < ncol; i++) xd.set(i, Dual(get_val(x.get(i)), 0.0));

    if constexpr (IsArray<Decayed<decltype(call_f(xd))>>) {
      // seed column 0 first to learn f's output length, then the rest
      seed(xd, std::size_t{0});
      auto y0 = call_f(xd);
      const std::size_t nrow = y0.size();
      auto jac = matrix(Double(0.0), Integer(static_cast<int>(nrow)), Integer(static_cast<int>(ncol)));
      for (std::size_t j = 0; j < nrow; j++) jac.set(j, y0.d.get_dot(j));
      unseed(xd, std::size_t{0});

      for (std::size_t i = 1; i < ncol; i++) {
        seed(xd, i);
        auto y = call_f(xd);
        for (std::size_t j = 0; j < nrow; j++) jac.set(i * nrow + j, y.d.get_dot(j));
        unseed(xd, i);
      }
      return jac;
    } else {
      auto jac = matrix(Double(0.0), Integer(1), Integer(static_cast<int>(ncol)));
      for (std::size_t i = 0; i < ncol; i++) {
        seed(xd, i);
        jac.set(i, Double(call_f(xd).dot));
        unseed(xd, i);
      }
      return jac;
    }
  }
}

// Sparsity pattern + column colouring of f, built at translation
// (R/Colouring.R) for length(x) == n. Per column (CSC, 0-based): its colour
// and the rows it touches. Columns of one colour share no row.
struct Colouring {
  std::size_t n;
  std::size_t m;
  std::size_t ncolours;
  std::vector<int> colour;
  std::vector<int> col_ptr;
  std::vector<int> row_idx;
  // rows taken by reverse sweeps (runtime only): values row-wise, n per row
  std::vector<int> dense_rows;
  std::vector<double> dense_vals;
};

// one forward sweep per colour: seed all its columns at once, then hand every
// pattern entry (column j, CSC position p, row i, value) to store
template<typename X, typename Call, typename Store>
inline void coloured_sweeps(const X& x, const Colouring& c, Call&& call_f, Store&& store) {
  ass<"jacobian: length(x) differs from n given for colouring">(x.size() == c.n);
  Array<Dual, Buffer<Dual>> xd(SI{c.n});
  for (std::size_t i = 0; i < c.n; i++) xd.set(i, Dual(get_val(x.get(i)), 0.0));

  std::vector<std::vector<std::size_t>> cols_of(c.ncolours);
  // dense rows come from reverse sweeps (colouring_from_tape)
  std::vector<char> is_dense(c.m, 0);
  for (int r : c.dense_rows) is_dense[static_cast<std::size_t>(r)] = 1;

  for (std::size_t j = 0; j < c.n; j++) {
    if (c.colour[j] >= 0) cols_of[static_cast<std::size_t>(c.colour[j])].push_back(j);
  }

  for (const auto& cols : cols_of) {
    for (std::size_t j : cols) xd.d.set_dot(j, 1.0);
    auto y = call_f(xd);
    ass<"jacobian: f returned another length than the colouring assumes">(y.size() == c.m);
    for (std::size_t j : cols) {
      for (int p = c.col_ptr[j]; p < c.col_ptr[j + 1]; p++) {
        const std::size_t i = static_cast<std::size_t>(c.row_idx[static_cast<std::size_t>(p)]);
        if (is_dense[i]) continue;
        store(j, static_cast<std::size_t>(p), i, y.d.get_dot(i));
      }
    }
    for (std::size_t j : cols) xd.d.set_dot(j, 0.0);
  }
}

template<typename X, typename Call>
inline auto jacobian_coloured_impl(const X& x, const Colouring& c, Call&& call_f) {
  auto jac = matrix(Double(0.0), Integer(static_cast<int>(c.m)), Integer(static_cast<int>(c.n)));
  coloured_sweeps(x, c, call_f, [&](std::size_t j, std::size_t, std::size_t i, const Double& v) {
    jac.set(j * c.m + i, v);
  });
  for (std::size_t k = 0; k < c.dense_rows.size(); k++) {
    const std::size_t i = static_cast<std::size_t>(c.dense_rows[k]);
    for (std::size_t j = 0; j < c.n; j++) jac.set(j * c.m + i, Double(c.dense_vals[k * c.n + j]));
  }
  return jac;
}

// CSC of the pattern + greedy colouring, natural column order: smallest colour
// no column sharing a row has. Columns without entries get no colour (-1).
inline Colouring colour_columns(std::size_t n, const std::vector<std::vector<int>>& rows) {
  const std::size_t m = rows.size();
  std::vector<int> col_ptr(n + 1, 0);
  for (const auto& rw : rows) for (int j : rw) col_ptr[static_cast<std::size_t>(j) + 1]++;
  for (std::size_t j = 0; j < n; j++) col_ptr[j + 1] += col_ptr[j];
  std::vector<int> row_idx(static_cast<std::size_t>(col_ptr[n]));
  std::vector<int> fill(col_ptr.begin(), col_ptr.end() - 1);
  for (std::size_t r = 0; r < m; r++) {
    for (int j : rows[r]) row_idx[static_cast<std::size_t>(fill[static_cast<std::size_t>(j)]++)] = static_cast<int>(r);
  }

  std::vector<int> colour(n, -1);
  std::vector<std::size_t> forbidden(n + 1, n);
  std::size_t ncolours = 0;
  for (std::size_t j = 0; j < n; j++) {
    if (col_ptr[j] == col_ptr[j + 1]) continue;
    for (int p = col_ptr[j]; p < col_ptr[j + 1]; p++) {
      for (int k : rows[static_cast<std::size_t>(row_idx[static_cast<std::size_t>(p)])]) {
        const int ck = colour[static_cast<std::size_t>(k)];
        if (ck >= 0) forbidden[static_cast<std::size_t>(ck)] = j;
      }
    }
    std::size_t c = 0;
    while (forbidden[c] == j) c++;
    colour[j] = static_cast<int>(c);
    if (c + 1 > ncolours) ncolours = c + 1;
  }
  return Colouring{n, m, ncolours, std::move(colour), std::move(col_ptr), std::move(row_idx), {}, {}};
}

// Bidirectional split (#54): drop the k densest rows (reverse sweeps) where
// k + #colours of the rest is smallest. Cost >= k stops the search; a
// colouring is only tried if k + nnz of the densest remaining row can win.
inline std::vector<int> choose_dense_rows(std::size_t n, const std::vector<std::vector<int>>& rows) {
  const std::size_t m = rows.size();
  std::vector<int> order(m);
  std::iota(order.begin(), order.end(), 0);
  std::stable_sort(order.begin(), order.end(), [&rows](int a, int b) {
    return rows[static_cast<std::size_t>(a)].size() > rows[static_cast<std::size_t>(b)].size();
  });

  std::vector<std::vector<int>> rest = rows;
  std::size_t best_cost = colour_columns(n, rest).ncolours;
  std::size_t best_k = 0;
  for (std::size_t k = 1; k <= m && k < best_cost; k++) {
    rest[static_cast<std::size_t>(order[k - 1])].clear();
    const std::size_t densest_left = k < m ? rows[static_cast<std::size_t>(order[k])].size() : 0;
    if (k + densest_left >= best_cost) continue;
    const std::size_t cost = k + colour_columns(n, rest).ncolours;
    if (cost < best_cost) {
      best_cost = cost;
      best_k = k;
    }
  }
  return std::vector<int>(order.begin(), order.begin() + static_cast<std::ptrdiff_t>(best_k));
}

// Runtime pattern (development/Plans/sparsity_tracer.md): record f once on the
// reverse tape (f_rev = f translated with ReverseDouble), walk back from every
// output to the inputs it reaches. Exact for the branch taken at x. Block ops
// (matmul, solve, ...) count as depending on all of their inputs.
// Then greedy column colouring. The tape is rewound afterwards.
// split_dense: only for the dense result; sparse/banded need the full pattern
template<typename X, typename CallRev>
inline Colouring colouring_from_tape(const X& x, CallRev&& call_rev, bool split_dense = false) {
  auto& tape = TAPE_INTERN;
  const std::size_t tape_start = tape.size();
  const std::size_t blocks_start = tape.blocks.size();
  const std::size_t n = x.size();

  Array<ReverseDouble, Buffer<ReverseDouble>> xr(SI{n});
  for (std::size_t j = 0; j < n; j++) xr.set(j, ReverseDouble::Var(get_val(x.get(j))));
  auto y = call_rev(xr);
  const std::size_t m = y.size();

  std::vector<int> col_of(tape.size(), -1);
  for (std::size_t j = 0; j < n; j++) col_of[static_cast<std::size_t>(xr.get(j).id)] = static_cast<int>(j);

  // stamp = row being walked, so the visited marks need no reset per row
  std::vector<int> stamp(tape.size(), -1);
  std::vector<int> block_stamp(tape.blocks.size(), -1);
  std::vector<int> stack;
  std::vector<std::vector<int>> rows(m);
  auto push = [&](int id) { if (id >= 0) stack.push_back(id); };
  for (std::size_t r = 0; r < m; r++) {
    const int row = static_cast<int>(r);
    push(y.get(r).id);
    while (!stack.empty()) {
      const std::size_t id = static_cast<std::size_t>(stack.back());
      stack.pop_back();
      if (stamp[id] == row) continue;
      stamp[id] = row;
      if (col_of[id] >= 0) {
        rows[r].push_back(col_of[id]);
        continue;
      }
      const ROp op = tape.op[id];
      const bool is_block = op == ROp::MatMul || op == ROp::BlockSlot || op == ROp::Chol ||
                            op == ROp::Solve || op == ROp::CrossProd || op == ROp::TriSolve ||
                            op == ROp::Det;
      if (is_block) {
        // a[] holds the block index here, not a tape id
        const std::size_t k = static_cast<std::size_t>(tape.a[id]);
        if (block_stamp[k] == row) continue;
        block_stamp[k] = row;
        for (int in : tape.blocks[k].a_ids) push(in);
        for (int in : tape.blocks[k].b_ids) push(in);
      } else {
        push(tape.a[id]);
        push(tape.b[id]);
      }
    }
    std::sort(rows[r].begin(), rows[r].end());
  }

  // dense rows: VJP with e_r on the tape just recorded, values exact at x
  std::vector<int> dense;
  if (split_dense) dense = choose_dense_rows(n, rows);
  std::vector<double> dense_vals(dense.size() * n);
  for (std::size_t k = 0; k < dense.size(); k++) {
    tape.reverse(y.get(static_cast<std::size_t>(dense[k])).id);
    for (std::size_t j = 0; j < n; j++) {
      dense_vals[k * n + j] = tape.adj[static_cast<std::size_t>(xr.get(j).id)];
    }
  }
  tape.rewind(tape_start, blocks_start);

  // full pattern in the CSC, colours only from the rows left for the sweeps
  Colouring c = colour_columns(n, rows);
  if (!dense.empty()) {
    for (int r : dense) rows[static_cast<std::size_t>(r)].clear();
    Colouring rest = colour_columns(n, rows);
    c.ncolours = rest.ncolours;
    c.colour = std::move(rest.colour);
  }
  c.dense_rows = std::move(dense);
  c.dense_vals = std::move(dense_vals);
  return c;
}

template<typename F, typename X> requires (IsArray<Decayed<X>>)
inline auto jacobian(const F& f, const X& x) {
  return jacobian_impl(x, [&f](const auto& xv) { return f(xv); });
}

// second parameter type of f_rev's operator(), without const/&
template <typename F> struct SecondArgOf : SecondArgOf<decltype(&F::operator())> {};
template <typename C, typename R, typename A1, typename A2> struct SecondArgOf<R (C::*)(A1, A2) const> {
  using type = std::decay_t<A2>;
};
template <typename C, typename R, typename A1, typename A2> struct SecondArgOf<R (C::*)(A1, A2)> {
  using type = std::decay_t<A2>;
};

template<bool SplitDense = false, typename FRev, typename X>
inline Colouring runtime_pattern(const FRev& f_rev, const X& x) {
  return colouring_from_tape(x, [&f_rev](const auto& xv) { return f_rev(xv); }, SplitDense);
}

template<bool SplitDense = false, typename FRev, typename X, typename E>
inline Colouring runtime_pattern(const FRev& f_rev, const X& x, const E& extra) {
  // NOTE: extra is converted (Dual -> ReverseDouble: scalar, array, struct,
  // collection) inside the tape pass: its constants are rewound with the rest
  return colouring_from_tape(x, [&f_rev, &extra](const auto& xv) {
    typename SecondArgOf<FRev>::type extra_rev;
    extra_rev = extra;
    return f_rev(xv, extra_rev);
  }, SplitDense);
}

// coloured jacobian with the pattern from colouring_from_tape
template<typename F, typename FRev, typename X> requires (IsArray<Decayed<X>>)
inline auto jacobian_runtime_coloured(const F& f, const FRev& f_rev, const X& x) {
  return jacobian_coloured_impl(x, runtime_pattern<true>(f_rev, x), [&f](const auto& xv) { return f(xv); });
}

template<typename F, typename FRev, typename X, typename E> requires (IsArray<Decayed<X>>)
inline auto jacobian_runtime_coloured(const F& f, const FRev& f_rev, const X& x, const E& extra) {
  return jacobian_coloured_impl(x, runtime_pattern<true>(f_rev, x, extra),
                                [&f, &extra](const auto& xv) { return f(xv, extra); });
}

// jacobian(f, x, colouring = "runtime"): f_rev = the ReverseDouble copy of f
template<typename FRev>
struct RuntimeColouring {
  const FRev& f_rev;
};

// helper instead of CTAD: aggregate CTAD is missing in older Apple Clang
template<typename FRev>
inline RuntimeColouring<FRev> runtime_colouring(const FRev& f_rev) {
  return RuntimeColouring<FRev>{f_rev};
}

template<typename F, typename X, typename FRev> requires (IsArray<Decayed<X>>)
inline auto jacobian(const F& f, const X& x, const RuntimeColouring<FRev>& rc) {
  return jacobian_runtime_coloured(f, rc.f_rev, x);
}

template<typename F, typename X, typename E, typename FRev> requires (IsArray<Decayed<X>>)
inline auto jacobian(const F& f, const X& x, const E& extra, const RuntimeColouring<FRev>& rc) {
  return jacobian_runtime_coloured(f, rc.f_rev, x, extra);
}

template<typename F, typename X> requires (IsArray<Decayed<X>>)
inline auto jacobian(const F& f, const X& x, const Colouring& c) {
  return jacobian_coloured_impl(x, c, [&f](const auto& xv) { return f(xv); });
}

template<typename F, typename X, typename E> requires (IsArray<Decayed<X>>)
inline auto jacobian(const F& f, const X& x, const E& extra, const Colouring& c) {
  return jacobian_coloured_impl(x, c, [&f, &extra](const auto& xv) { return f(xv, extra); });
}

template<typename F, typename X, typename E> requires (IsArray<Decayed<X>>)
inline auto jacobian(const F& f, const X& x, const E& extra) {
  return jacobian_impl(x, [&f, &extra](const auto& xv) { return f(xv, extra); });
}

// jacobian_sparse / jacobian_banded: values only in the colouring's pattern,
// stored as SUNDIALS does, so the result can later go to SUNMatrix as is:
// sparse = CSC (0-based), banded = LAPACK band without LU scratch rows
// (A(i, j) at data[j * ldim + mu + i - j], ldim = mu + ml + 1)

#ifndef STANDALONE_ETR
// Matrix::sparseMatrix from CSC; R_tryEvalSilent: an R error must not longjmp through C++
inline SEXP csc_to_dgCMatrix(int m, int n, SEXP p, SEXP i, SEXP x) {
  SEXP fn = PROTECT(Rf_lang3(Rf_install("::"), Rf_install("Matrix"), Rf_install("sparseMatrix")));
  SEXP dims = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(dims)[0] = m;
  INTEGER(dims)[1] = n;
  SEXP index1 = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP args = PROTECT(Rf_list5(i, p, x, dims, index1));
  SEXP call = PROTECT(Rf_lcons(fn, args));
  SEXP arg = args;
  for (const char* tag : {"i", "p", "x", "dims", "index1"}) {
    SET_TAG(arg, Rf_install(tag));
    arg = CDR(arg);
  }
  int error = 0;
  SEXP res = R_tryEvalSilent(call, R_GlobalEnv, &error);
  UNPROTECT(5);
  ass<"converting the jacobian to a Matrix::dgCMatrix failed (is the Matrix package installed?)">(error == 0);
  return res;
}
#endif

struct jacobian_sparse_result {
  Integer m;
  Integer n;
  Array<Integer, Buffer<Integer>> indexptrs;
  Array<Integer, Buffer<Integer>> indexvals;
  Array<Double, Buffer<Double>> data;

  jacobian_sparse_result() = default;
#ifndef STANDALONE_ETR
  SEXP to_SEXP() const {
    SEXP p = PROTECT(etr::Cast(indexptrs));
    SEXP i = PROTECT(etr::Cast(indexvals));
    SEXP x = PROTECT(etr::Cast(data));
    SEXP res = csc_to_dgCMatrix(get_val(m), get_val(n), p, i, x);
    UNPROTECT(3);
    return res;
  }
#endif
};

struct jacobian_banded_result {
  Integer n;
  Integer mu;
  Integer ml;
  Integer ldim;
  Array<Double, Buffer<Double>> data;

  jacobian_banded_result() = default;
#ifndef STANDALONE_ETR
  SEXP to_SEXP() const {
    const int nn = get_val(n), up = get_val(mu), lo = get_val(ml), ld = get_val(ldim);
    SEXP p = PROTECT(Rf_allocVector(INTSXP, nn + 1));
    INTEGER(p)[0] = 0;
    for (int j = 0; j < nn; j++) {
      INTEGER(p)[j + 1] = INTEGER(p)[j] + (std::min(nn - 1, j + lo) - std::max(0, j - up) + 1);
    }
    SEXP i = PROTECT(Rf_allocVector(INTSXP, INTEGER(p)[nn]));
    SEXP x = PROTECT(Rf_allocVector(REALSXP, INTEGER(p)[nn]));
    int k = 0;
    for (int j = 0; j < nn; j++) {
      for (int r = std::max(0, j - up); r <= std::min(nn - 1, j + lo); r++, k++) {
        INTEGER(i)[k] = r;
        REAL(x)[k] = get_val(data.get(static_cast<std::size_t>(j * ld + up + r - j)));
      }
    }
    SEXP res = csc_to_dgCMatrix(nn, nn, p, i, x);
    UNPROTECT(3);
    return res;
  }
#endif
};

inline void print(const jacobian_sparse_result& x) {
  PRINT_STREAM << "jacobian_sparse_result: " << x.m << " x " << x.n << ", "
               << x.data.size() << " entries (CSC)" << std::endl;
}

inline void print(const jacobian_banded_result& x) {
  PRINT_STREAM << "jacobian_banded_result: " << x.n << " x " << x.n << ", mu = " << x.mu
               << ", ml = " << x.ml << std::endl;
}

template<typename X, typename Call>
inline jacobian_sparse_result jacobian_sparse_impl(const X& x, const Colouring& c, Call&& call_f) {
  jacobian_sparse_result res;
  res.m = Integer(static_cast<int>(c.m));
  res.n = Integer(static_cast<int>(c.n));
  res.indexptrs = Array<Integer, Buffer<Integer>>(SI{c.col_ptr.size()});
  for (std::size_t k = 0; k < c.col_ptr.size(); k++) res.indexptrs.set(k, Integer(c.col_ptr[k]));
  res.indexvals = Array<Integer, Buffer<Integer>>(SI{c.row_idx.size()});
  for (std::size_t k = 0; k < c.row_idx.size(); k++) res.indexvals.set(k, Integer(c.row_idx[k]));
  res.data = Array<Double, Buffer<Double>>(SI{c.row_idx.size()});
  coloured_sweeps(x, c, call_f, [&](std::size_t, std::size_t p, std::size_t, const Double& v) {
    res.data.set(p, v);
  });
  if (!c.dense_rows.empty()) {
    std::vector<int> slot(c.m, -1);
    for (std::size_t k = 0; k < c.dense_rows.size(); k++) slot[static_cast<std::size_t>(c.dense_rows[k])] = static_cast<int>(k);
    for (std::size_t j = 0; j < c.n; j++) {
      for (int p = c.col_ptr[j]; p < c.col_ptr[j + 1]; p++) {
        const int k = slot[static_cast<std::size_t>(c.row_idx[static_cast<std::size_t>(p)])];
        if (k >= 0) res.data.set(static_cast<std::size_t>(p), Double(c.dense_vals[static_cast<std::size_t>(k) * c.n + j]));
      }
    }
  }
  return res;
}

template<typename X, typename Call>
inline jacobian_banded_result jacobian_banded_impl(const X& x, const Colouring& c, Call&& call_f) {
  ass<"jacobian_banded: f has to return a vector as long as x (square jacobian)">(c.m == c.n);
  int up = 0, lo = 0;
  for (std::size_t j = 0; j < c.n; j++) {
    for (int p = c.col_ptr[j]; p < c.col_ptr[j + 1]; p++) {
      const int d = c.row_idx[static_cast<std::size_t>(p)] - static_cast<int>(j);
      if (d < 0) up = std::max(up, -d);
      if (d > 0) lo = std::max(lo, d);
    }
  }
  const std::size_t ldim = static_cast<std::size_t>(up + lo + 1);
  jacobian_banded_result res;
  res.n = Integer(static_cast<int>(c.n));
  res.mu = Integer(up);
  res.ml = Integer(lo);
  res.ldim = Integer(static_cast<int>(ldim));
  res.data = Array<Double, Buffer<Double>>(SI{ldim * c.n});
  for (std::size_t k = 0; k < ldim * c.n; k++) res.data.set(k, Double(0.0));
  coloured_sweeps(x, c, call_f, [&](std::size_t j, std::size_t, std::size_t i, const Double& v) {
    res.data.set(j * ldim + static_cast<std::size_t>(up) + i - j, v);
  });
  return res;
}

template<typename F, typename X> requires (IsArray<Decayed<X>>)
inline auto jacobian_sparse(const F& f, const X& x, const Colouring& c) {
  return jacobian_sparse_impl(x, c, [&f](const auto& xv) { return f(xv); });
}
template<typename F, typename X, typename E> requires (IsArray<Decayed<X>>)
inline auto jacobian_sparse(const F& f, const X& x, const E& extra, const Colouring& c) {
  return jacobian_sparse_impl(x, c, [&f, &extra](const auto& xv) { return f(xv, extra); });
}
template<typename F, typename X, typename FRev> requires (IsArray<Decayed<X>>)
inline auto jacobian_sparse(const F& f, const X& x, const RuntimeColouring<FRev>& rc) {
  return jacobian_sparse_impl(x, runtime_pattern<true>(rc.f_rev, x), [&f](const auto& xv) { return f(xv); });
}
template<typename F, typename X, typename E, typename FRev> requires (IsArray<Decayed<X>>)
inline auto jacobian_sparse(const F& f, const X& x, const E& extra, const RuntimeColouring<FRev>& rc) {
  return jacobian_sparse_impl(x, runtime_pattern<true>(rc.f_rev, x, extra),
                              [&f, &extra](const auto& xv) { return f(xv, extra); });
}

template<typename F, typename X> requires (IsArray<Decayed<X>>)
inline auto jacobian_banded(const F& f, const X& x, const Colouring& c) {
  return jacobian_banded_impl(x, c, [&f](const auto& xv) { return f(xv); });
}
template<typename F, typename X, typename E> requires (IsArray<Decayed<X>>)
inline auto jacobian_banded(const F& f, const X& x, const E& extra, const Colouring& c) {
  return jacobian_banded_impl(x, c, [&f, &extra](const auto& xv) { return f(xv, extra); });
}
template<typename F, typename X, typename FRev> requires (IsArray<Decayed<X>>)
inline auto jacobian_banded(const F& f, const X& x, const RuntimeColouring<FRev>& rc) {
  return jacobian_banded_impl(x, runtime_pattern(rc.f_rev, x), [&f](const auto& xv) { return f(xv); });
}
template<typename F, typename X, typename E, typename FRev> requires (IsArray<Decayed<X>>)
inline auto jacobian_banded(const F& f, const X& x, const E& extra, const RuntimeColouring<FRev>& rc) {
  return jacobian_banded_impl(x, runtime_pattern(rc.f_rev, x, extra),
                              [&f, &extra](const auto& xv) { return f(xv, extra); });
}

} // namespace etr

#endif
