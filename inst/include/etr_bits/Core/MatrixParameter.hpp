#ifndef MATRIXPARAMETER_ETR_H
#define MATRIXPARAMETER_ETR_H

namespace etr {
struct MatrixParameter {
  bool ismatrix = false;
  std::size_t rows = 0;
  std::size_t cols = 0;
  MatrixParameter() {}
  MatrixParameter(std::size_t rows_, std::size_t cols_)
      : ismatrix(true), rows(rows_), cols(cols_) {}
  MatrixParameter(const MatrixParameter &other)
      : ismatrix(other.im()), rows(other.nr()), cols(other.nc()) {}
  bool im() const { return this->ismatrix; }
  std::size_t nc() const { return cols; }
  std::size_t nr() const { return rows; }
  void setMatrix(bool i, std::size_t nrow, std::size_t ncol) {
    this->ismatrix = i;
    this->cols = ncol;
    this->rows = nrow;
  }
  void setMatrix(const MatrixParameter &mp_) {
    this->setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  friend std::ostream &operator<<(std::ostream &os, const MatrixParameter &m) {
    os << std::boolalpha << m.ismatrix << " nrows = " << m.rows
       << " ncols = " << m.cols << std::endl;
    return os;
  }
};

template <typename L, typename R>
void defineMatrix(const L &l, const R &r, MatrixParameter &mp) {
  using typeTraitL = std::remove_reference<decltype(convert(l))>::type::Type;
  using typeTraitR = std::remove_reference<decltype(convert(r))>::type::Type;
  using isDoubleL = std::is_same<typeTraitL, DoubleTrait>;
  using isDoubleR = std::is_same<typeTraitR, DoubleTrait>;
  mp.setMatrix(false, 0, 0);
  if constexpr (!isDoubleL::value && isDoubleR::value) {
    if (l.im()) {
      mp.setMatrix(true, l.nr(), l.nc());
    }
  } else if constexpr (isDoubleL::value && !isDoubleR::value) {
    if (r.im()) {
      mp.setMatrix(true, r.nr(), r.nc());
    }
  } else {
    if (l.im() && r.im()) {
      std::size_t nrows = (l.nr() > r.nr()) ? l.nr() : r.nr();
      std::size_t ncols = (l.nc() > r.nc()) ? l.nc() : r.nc();
      mp.setMatrix(true, nrows, ncols);
    } else if (!l.im() && r.im()) {
      std::size_t nrows = r.nr();
      std::size_t ncols = r.nc();
      mp.setMatrix(true, nrows, ncols);
    } else if (l.im() && !r.im()) {
      std::size_t nrows = l.nr();
      std::size_t ncols = l.nc();
      mp.setMatrix(true, nrows, ncols);
    }
  }
}



} // namespace etr

#endif