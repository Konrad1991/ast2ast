#include "etr.hpp"
#include <stdexcept>
using namespace etr;

// [[Rcpp::export]]
void test_concepts() {
  Vec<double> v = c(1.1, 2.2, 3.3);
  size_t size = 10;
  double *ptr = new double[size];
  Vec<double, Borrow<double>> vb(ptr, size);
  bool b = true;
  int i = 1;
  double d = 3.14;
  Buffer<double> buf(3);
  Mat<double> m = matrix(0, 3, 5);
  Mat<double, Borrow<double>> mb(ptr, 2, 5);

  // Arithmetic
  {
    static_assert(IsBool<decltype(b)>, "Expect bool");
    static_assert(IsInteger<decltype(i)>, "Expect integer");
    static_assert(IsInteger<decltype(b)>, "Expect integer");
    static_assert(IsFloat<decltype(d)>, "Expect float");
    static_assert(IsArithV<decltype(b)>, "Expect arithmetic value");
    static_assert(IsArithV<decltype(i)>, "Expect arithmetic value");
    static_assert(IsArithV<decltype(d)>, "Expect arithmetic value");
  }

  // R
  {
    using binary_op = decltype((v + v).d);
    static_assert(IsBinary<binary_op>, "Expect binary operation");
    using unary_op = decltype((sinus(v)).d);
    static_assert(IsUnary<unary_op>, "Expect unary operation");
    using comparison_op = decltype((v != v).d);
    static_assert(IsComparison<comparison_op>, "Expect comparison");
    using subset_op = decltype(subset(c(1, 2, 3), 1).d);
    static_assert(IsSubsetClass<subset_op>, "Expect subset class");
    using lbuffer = decltype(buf);
    static_assert(IsLBuffer<lbuffer>, "Expect l buffer");
    using rbuffer = decltype(Buffer<double, RBufferTrait>(3));
    static_assert(IsRBuffer<rbuffer>, "Expect r buffer");
    using borrow_t = decltype(vb.d);
    static_assert(IsBorrow<borrow_t>, "Expect borrow class");
  }

  // Vector
  {
    using binary_vec_op = decltype(v + v);
    static_assert(IsOperationVec<binary_vec_op>, "Expect operation vector");
    static_assert(IsBinaryVec<binary_vec_op>, "Expect binary operation");
    using unary_vec_op = decltype(sinus(v));
    static_assert(IsOperationVec<unary_vec_op>, "Expect operation vector");
    static_assert(IsUnaryVec<unary_vec_op>, "Expect unary operation vec");
    using comparison_vec_op = decltype(v < v);
    static_assert(IsComparisonVec<comparison_vec_op>, "Expect comparison vector");
    static_assert(IsOperationVec<comparison_vec_op>, "Expect operation vector");

    static_assert(IsROrCalculationVec<binary_vec_op>, "Expect r or calc vector");
    static_assert(IsROrCalculationVec<unary_vec_op>, "Expect r or calc vector");
    static_assert(IsROrCalculationVec<comparison_vec_op>, "Expect r or calc vector");
    using r_type = decltype(c(1, 2, 3));
    static_assert(IsROrCalculationVec<r_type>, "Expect r vector");

    using vec_t1 = decltype(v);
    using vec_t2 = decltype(vb);
    static_assert(IsVec<vec_t1>, "Expect vector");
    static_assert(IsVec<vec_t2>, "Expect vector");
    static_assert(IsBufferVec<vec_t1>, "Expect buffer vector");
    static_assert(IsRVec<decltype(c(1, 2, 3))>, "Expect R vector");
    static_assert(IsBorrowVec<vec_t2>, "Expect borrow vector");
    using subset_t = decltype(subset(v, 1));
    static_assert(IsSubsetVec<subset_t>, "Expect subset vector");
    using comp_vec = decltype(v == v);
    static_assert(IsComparisonVec<comp_vec>, "Expect comparison vector");

    using subset_op = decltype(subset(v + v, 1));
    static_assert(IsSubsetVec<subset_op>, "Expect subset of operation to be subset vec");

    using vec_t1 = decltype(v);
    static_assert(!IsROrCalculationVec<vec_t1>, "LVec must not be R/calc vec");
    using binary_vec_op = decltype(v + v);
    static_assert(!IsBufferVec<binary_vec_op>, "Op vec must not be buffer vec");
  }

  // Matrix
  {
    using m_t1 = decltype(m);
    using m_t2 = decltype(mb);
    static_assert(IsMat<m_t1>, "Expect Mat");
    static_assert(IsBufferMat<m_t1>, "Expect Mat");
    static_assert(IsMat<m_t2>, "Expect Mat");
    static_assert(IsBorrowMat<m_t2>, "Expect borrow Mat");
    static_assert(IsRMat<decltype(matrix(0, 2, 4))>, "Expect R Mat");
    using subset_m1 = decltype(subset(matrix(0, 2, 2), 1, 1));
    static_assert(IsSubsetMat<subset_m1>, "Expect subsetted Mat");
    using subset_m2 = decltype(subset(matrix(0, 2, 2), 1));
    static_assert(IsSubsetVec<subset_m2>, "Expect subsetted Vec");

    static_assert(IsArrayLike<decltype(v)>, "Expect array like");
    static_assert(IsArrayLike<decltype(vb)>, "Expect array like");
    static_assert(IsArrayLike<decltype(m)>, "Expect array like");
    static_assert(IsArrayLike<decltype(mb)>, "Expect array like");
    static_assert(IsRArrayLike<decltype(c(1, 2, 3))>, "Expect R array like");
    static_assert(IsRArrayLike<decltype(matrix(1, 2, 3))>, "Expect R array like");
  }
  // Move stuff
  {
    static_assert(std::is_move_constructible_v<decltype(v.d)>);
    static_assert(std::is_move_constructible_v<decltype(vb.d)>);
    static_assert(std::is_nothrow_move_constructible_v<decltype(v.d)>);
    static_assert(std::is_nothrow_move_constructible_v<decltype(vb.d)>);
    static_assert(std::is_move_constructible_v<decltype(m.d)>);
    static_assert(std::is_move_constructible_v<decltype(mb.d)>);
  }
  delete[] ptr;
}
