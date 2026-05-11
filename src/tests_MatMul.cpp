#include <Rcpp.h>
#include "../inst/include/etr.hpp"
using namespace etr;

// End-to-end tests for the user-facing mat_mul(...) function.
//
// All sections share the same numerical example (column-major):
//   A (2x3) = [[1,2,3],[4,5,6]]      buffer: [1,4,2,5,3,6]
//   B (3x2) = [[7,8],[9,10],[11,12]] buffer: [7,9,11,8,10,12]
//   C = A*B = [[58,64],[139,154]]    buffer: [58,139,64,154]
//
// The tape (TAPE_INTERN) is cleared at the start of every ReverseDouble
// section so id arithmetic is predictable.

static bool close(double l, double r, double tol = 1e-9) {
  return std::abs(l - r) < tol;
}

// [[Rcpp::export]]
void test_mat_mul() {
  const std::vector<double>      Avals{1.0, 4.0, 2.0, 5.0, 3.0, 6.0};
  const std::vector<double>      Bvals{7.0, 9.0, 11.0, 8.0, 10.0, 12.0};
  const std::vector<double>      expC {58.0, 139.0, 64.0, 154.0};
  const std::vector<std::size_t> dimA{2, 3};
  const std::vector<std::size_t> dimB{3, 2};

  // ---- 1. plain Double x Double, both LBuffer --------------------------------
  {
    Array<Double, Buffer<Double, LBufferTrait>> A(SI{6});
    for (std::size_t k = 0; k < 6; ++k) A.set(k, Double(Avals[k]));
    A.dim = dimA;
    Array<Double, Buffer<Double, LBufferTrait>> B(SI{6});
    for (std::size_t k = 0; k < 6; ++k) B.set(k, Double(Bvals[k]));
    B.dim = dimB;

    auto C = mat_mul(A, B);
    ass<"plain DxD: dim rank">(C.get_dim().size() == 2);
    ass<"plain DxD: dim[0]=2">(C.get_dim()[0] == 2);
    ass<"plain DxD: dim[1]=2">(C.get_dim()[1] == 2);
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"plain DxD: value">(close(get_val(C.get(k)), expC[k]));
    }
  }

  // ---- 2. type promotion: Integer x Double -> Array<Double> ------------------
  {
    Array<Integer, Buffer<Integer, LBufferTrait>> A(SI{6});
    for (std::size_t k = 0; k < 6; ++k) A.set(k, Integer(static_cast<int>(Avals[k])));
    A.dim = dimA;
    Array<Double, Buffer<Double, LBufferTrait>> B(SI{6});
    for (std::size_t k = 0; k < 6; ++k) B.set(k, Double(Bvals[k]));
    B.dim = dimB;

    auto C = mat_mul(A, B);
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"IntxDouble: value">(close(get_val(C.get(k)), expC[k]));
    }
  }

  // ---- 3. type promotion: Logical x Integer -> Array<Double> ----------------
  // Use A_logical with all-true entries so A behaves as the all-ones 2x3.
  // Expected: row sums of B → [27, 27, 30, 30] in col-major (since all A[i,k] = 1).
  {
    Array<Logical, Buffer<Logical, LBufferTrait>> A(SI{6});
    for (std::size_t k = 0; k < 6; ++k) A.set(k, Logical(true));
    A.dim = dimA;
    Array<Integer, Buffer<Integer, LBufferTrait>> B(SI{6});
    for (std::size_t k = 0; k < 6; ++k) B.set(k, Integer(static_cast<int>(Bvals[k])));
    B.dim = dimB;

    auto C = mat_mul(A, B);
    const std::vector<double> exp_logint{27.0, 27.0, 30.0, 30.0};
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"LogicalxInteger: value">(close(get_val(C.get(k)), exp_logint[k]));
    }
  }

  // ---- 4. mixed buffer traits: LBuffer x RBuffer ----------------------------
  {
    Array<Double, Buffer<Double, LBufferTrait>> A(SI{6});
    for (std::size_t k = 0; k < 6; ++k) A.set(k, Double(Avals[k]));
    A.dim = dimA;
    Array<Double, Buffer<Double, RBufferTrait>> B(SI{6});
    for (std::size_t k = 0; k < 6; ++k) B.set(k, Double(Bvals[k]));
    B.dim = dimB;

    auto C = mat_mul(A, B);
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"LxR: value">(close(get_val(C.get(k)), expC[k]));
    }
  }

   // ---- 5. borrow buffers on both sides --------------------------------------
   {
     std::vector<double> Aown = Avals;
     std::vector<double> Bown = Bvals;
     Array<Double, Borrow<Double, BorrowTrait>> A(Aown.data(), Aown.size(), dimA);
     Array<Double, Borrow<Double, BorrowTrait>> B(Bown.data(), Bown.size(), dimB);
     auto C = mat_mul(A, B);
     for (std::size_t k = 0; k < 4; ++k) {
       ass<"BorrowxBorrow: value">(close(get_val(C.get(k)), expC[k]));
     }
   }

  // ---- 6. row vector x matrix:  1x3 * 3x2 = 1x2 -----------------------------
  // v = [1,2,3] as 1x3, * B → [58, 64].
  {
    Array<Double, Buffer<Double, LBufferTrait>> v(SI{3});
    v.set(0, Double(1.0)); v.set(1, Double(2.0)); v.set(2, Double(3.0));
    // 1-D vector: dim is left as the {3} the SI ctor produced.
    Array<Double, Buffer<Double, LBufferTrait>> B(SI{6});
    for (std::size_t k = 0; k < 6; ++k) B.set(k, Double(Bvals[k]));
    B.dim = dimB;

    auto C = mat_mul(v, B);
    ass<"vec x mat: dim rank">(C.get_dim().size() == 2);
    ass<"vec x mat: dim[0]=1">(C.get_dim()[0] == 1);
    ass<"vec x mat: dim[1]=2">(C.get_dim()[1] == 2);
    ass<"vec x mat: C[0,0]=58">(close(get_val(C.get(0)), 58.0));
    ass<"vec x mat: C[0,1]=64">(close(get_val(C.get(1)), 64.0));
  }

  // ---- 7. matrix x column vector: 2x3 * 3x1 = 2x1 ---------------------------
  // A * [1,2,3]^T = [1+4+9, 4+10+18] = [14, 32].
  {
    Array<Double, Buffer<Double, LBufferTrait>> A(SI{6});
    for (std::size_t k = 0; k < 6; ++k) A.set(k, Double(Avals[k]));
    A.dim = dimA;
    Array<Double, Buffer<Double, LBufferTrait>> v(SI{3});
    v.set(0, Double(1.0)); v.set(1, Double(2.0)); v.set(2, Double(3.0));

    auto C = mat_mul(A, v);
    ass<"mat x vec: dim rank">(C.get_dim().size() == 2);
    ass<"mat x vec: dim[0]=2">(C.get_dim()[0] == 2);
    ass<"mat x vec: dim[1]=1">(C.get_dim()[1] == 1);
    ass<"mat x vec: C[0,0]=14">(close(get_val(C.get(0)), 14.0));
    ass<"mat x vec: C[1,0]=32">(close(get_val(C.get(1)), 32.0));
  }

  // ---- 8. scalar x scalar -> 1x1 --------------------------------------------
  {
    Double a(3.0), b(5.0);
    auto C = mat_mul(a, b);
    ass<"scalar x scalar: dim rank">(C.get_dim().size() == 2);
    ass<"scalar x scalar: dim[0]=1">(C.get_dim()[0] == 1);
    ass<"scalar x scalar: dim[1]=1">(C.get_dim()[1] == 1);
    ass<"scalar x scalar: 3*5=15">(close(get_val(C.get(0)), 15.0));
  }

  // ---- 9. forward AD: Dual x Dual -------------------------------------------
  // A.dot and B.dot are all 1s. C.dot[i,j] = colSum(B,j) + rowSum(A,i).
  //   col0 B = 27, col1 B = 30; row0 A = 6, row1 A = 15.
  //   → C.dot col-major: [33, 42, 36, 45]
  {
    Array<Dual, Buffer<Dual, LBufferTrait>> A(SI{6});
    for (std::size_t k = 0; k < 6; ++k) {
      Dual d; d.val = Avals[k]; d.dot = 1.0; d.is_na = false; d.is_na_dot = false;
      A.set(k, d);
    }
    A.dim = dimA;
    Array<Dual, Buffer<Dual, LBufferTrait>> B(SI{6});
    for (std::size_t k = 0; k < 6; ++k) {
      Dual d; d.val = Bvals[k]; d.dot = 1.0; d.is_na = false; d.is_na_dot = false;
      B.set(k, d);
    }
    B.dim = dimB;

    auto C = mat_mul(A, B);
    const std::vector<double> exp_dot{33.0, 42.0, 36.0, 45.0};
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"Dual x Dual: val">(close(C.get(k).val, expC[k]));
      ass<"Dual x Dual: dot">(close(C.get(k).dot, exp_dot[k]));
    }
  }

  // ---- 10. forward AD mixed: Dual x Double ----------------------------------
  // B has no dot (treated as 0). C.dot[i,j] = sum_k A.dot[i,k] * B[k,j].
  // With A.dot all 1s, that's just colSum(B,j): [27, 27, 30, 30].
  {
    Array<Dual, Buffer<Dual, LBufferTrait>> A(SI{6});
    for (std::size_t k = 0; k < 6; ++k) {
      Dual d; d.val = Avals[k]; d.dot = 1.0; d.is_na = false; d.is_na_dot = false;
      A.set(k, d);
    }
    A.dim = dimA;
    Array<Double, Buffer<Double, LBufferTrait>> B(SI{6});
    for (std::size_t k = 0; k < 6; ++k) B.set(k, Double(Bvals[k]));
    B.dim = dimB;

    auto C = mat_mul(A, B);
    const std::vector<double> exp_dot{27.0, 27.0, 30.0, 30.0};
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"Dual x Double: val">(close(C.get(k).val, expC[k]));
      ass<"Dual x Double: dot">(close(C.get(k).dot, exp_dot[k]));
    }
  }

  // ---- 11. reverse AD: ReverseDouble x ReverseDouble ------------------------
  // After mat_mul, call reverse() on C[0,0] and check input adjoints match
  //   dA[0,k] = B[k,0] = (7, 9, 11);  dA[1,*] = 0
  //   dB[k,0] = A[0,k] = (1, 2, 3);   dB[*,1] = 0
  {
    TAPE_INTERN.clear();
    Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> A(SI{6});
    std::vector<int> a_ids(6);
    for (std::size_t k = 0; k < 6; ++k) {
      auto rd = ReverseDouble::Var(Avals[k]);
      a_ids[k] = rd.id;
      A.set(k, rd);
    }
    A.dim = dimA;
    Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> B(SI{6});
    std::vector<int> b_ids(6);
    for (std::size_t k = 0; k < 6; ++k) {
      auto rd = ReverseDouble::Var(Bvals[k]);
      b_ids[k] = rd.id;
      B.set(k, rd);
    }
    B.dim = dimB;

    auto C = mat_mul(A, B);
    ass<"R x R: dim rank">(C.get_dim().size() == 2);
    ass<"R x R: dim[0]=2">(C.get_dim()[0] == 2);
    ass<"R x R: dim[1]=2">(C.get_dim()[1] == 2);
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"R x R: forward">(close(C.get(k).get_val_from_tape(), expC[k]));
    }

    TAPE_INTERN.reverse(C.get(0).id);  // reverse on C[0,0]
    // a_ids[k*2 + m] = id of A[m,k]
    ass<"R x R: dA[0,0]=7">(close(TAPE_INTERN.adj[a_ids[0 * 2 + 0]], 7.0));
    ass<"R x R: dA[0,1]=9">(close(TAPE_INTERN.adj[a_ids[1 * 2 + 0]], 9.0));
    ass<"R x R: dA[0,2]=11">(close(TAPE_INTERN.adj[a_ids[2 * 2 + 0]], 11.0));
    ass<"R x R: dA[1,0]=0">(close(TAPE_INTERN.adj[a_ids[0 * 2 + 1]], 0.0));
    ass<"R x R: dA[1,1]=0">(close(TAPE_INTERN.adj[a_ids[1 * 2 + 1]], 0.0));
    ass<"R x R: dA[1,2]=0">(close(TAPE_INTERN.adj[a_ids[2 * 2 + 1]], 0.0));
    // b_ids[k*3 + m] = id of B[m,k]   (rows_b = K = 3)
    ass<"R x R: dB[0,0]=1">(close(TAPE_INTERN.adj[b_ids[0 * 3 + 0]], 1.0));
    ass<"R x R: dB[1,0]=2">(close(TAPE_INTERN.adj[b_ids[0 * 3 + 1]], 2.0));
    ass<"R x R: dB[2,0]=3">(close(TAPE_INTERN.adj[b_ids[0 * 3 + 2]], 3.0));
    ass<"R x R: dB[0,1]=0">(close(TAPE_INTERN.adj[b_ids[1 * 3 + 0]], 0.0));
  }

  // ---- 12. reverse AD mixed: ReverseDouble x Double -------------------------
  // B is plain Double (no tape ids), so only A's adjoints update.
  {
    TAPE_INTERN.clear();
    Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> A(SI{6});
    std::vector<int> a_ids(6);
    for (std::size_t k = 0; k < 6; ++k) {
      auto rd = ReverseDouble::Var(Avals[k]);
      a_ids[k] = rd.id;
      A.set(k, rd);
    }
    A.dim = dimA;
    Array<Double, Buffer<Double, LBufferTrait>> B(SI{6});
    for (std::size_t k = 0; k < 6; ++k) B.set(k, Double(Bvals[k]));
    B.dim = dimB;

    auto C = mat_mul(A, B);
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"R x D: forward">(close(C.get(k).get_val_from_tape(), expC[k]));
    }

    TAPE_INTERN.reverse(C.get(0).id);
    ass<"R x D: dA[0,0]=7">(close(TAPE_INTERN.adj[a_ids[0 * 2 + 0]], 7.0));
    ass<"R x D: dA[0,1]=9">(close(TAPE_INTERN.adj[a_ids[1 * 2 + 0]], 9.0));
    ass<"R x D: dA[0,2]=11">(close(TAPE_INTERN.adj[a_ids[2 * 2 + 0]], 11.0));
    ass<"R x D: dA[1,*]=0">(close(TAPE_INTERN.adj[a_ids[0 * 2 + 1]], 0.0)
                        && close(TAPE_INTERN.adj[a_ids[1 * 2 + 1]], 0.0)
                        && close(TAPE_INTERN.adj[a_ids[2 * 2 + 1]], 0.0));
  }

  // ---- 13. reverse AD with borrowed B: ReverseDouble x Borrow<Double> -------
  {
    TAPE_INTERN.clear();
    Array<ReverseDouble, Buffer<ReverseDouble, LBufferTrait>> A(SI{6});
    std::vector<int> a_ids(6);
    for (std::size_t k = 0; k < 6; ++k) {
      auto rd = ReverseDouble::Var(Avals[k]);
      a_ids[k] = rd.id;
      A.set(k, rd);
    }
    A.dim = dimA;
    std::vector<double> Bown = Bvals;
    Array<Double, Borrow<Double, BorrowTrait>> B(Bown.data(), Bown.size(), dimB);

    auto C = mat_mul(A, B);
    for (std::size_t k = 0; k < 4; ++k) {
      ass<"R x Borrow: forward">(close(C.get(k).get_val_from_tape(), expC[k]));
    }

    TAPE_INTERN.reverse(C.get(0).id);
    ass<"R x Borrow: dA[0,0]=7">(close(TAPE_INTERN.adj[a_ids[0 * 2 + 0]], 7.0));
    ass<"R x Borrow: dA[1,0]=0">(close(TAPE_INTERN.adj[a_ids[0 * 2 + 1]], 0.0));
  }

  // ---- 14. reverse AD scalar x scalar: routes through the block op too -----
  // ReverseDouble scalar a × Double scalar b. Result is a 1x1 Array<ReverseDouble>
  // with value a*b. After reverse(), adj[a.id] = b.
  {
    TAPE_INTERN.clear();
    auto a = ReverseDouble::Var(3.0);
    const int aid = a.id;
    Double b(5.0);
    auto C = mat_mul(a, b);
    ass<"scalar R x scalar D: dim">(C.get_dim()[0] == 1 && C.get_dim()[1] == 1);
    ass<"scalar R x scalar D: 15">(close(C.get(0).get_val_from_tape(), 15.0));
    TAPE_INTERN.reverse(C.get(0).id);
    ass<"scalar R x scalar D: dA=5">(close(TAPE_INTERN.adj[aid], 5.0));
  }
}
