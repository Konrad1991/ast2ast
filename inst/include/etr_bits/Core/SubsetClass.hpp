#ifndef SUBSETCLASS_ETR_H
#define SUBSETCLASS_ETR_H

namespace etr {
// Points to a Variable and stores indicces in ind
template <typename T, typename SubsetTrait> struct Subset {
  using RetType = BaseType;
  using Type = T;
  using TypeTrait = SubsetTrait;
  using CaseTrait = SubsetTrait;
  Indices ind;
  T *p = nullptr;
  using CurrentBaseType = std::remove_reference<decltype(*p)>::type::Type;
  MatrixParameter mp;

  std::size_t size() const { return ind.size(); }
  bool im() const { return mp.im(); }
  std::size_t nc() const { return mp.nc(); }
  std::size_t nr() const { return mp.nr(); }
  void setMatrix(bool i, std::size_t nrow, std::size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void resize(std::size_t newSize) { p->resize(newSize); }
  void realloc(std::size_t newSize) { p->realloc(newSize); }

  Subset(const Subset &other) : p(other.p) {
    this->setMatrix(other.mp);
    this->ind = other.ind;
  }
  Subset(const Subset &&other) : p(other.p) {
    this->p = other.p;
    this->setMatrix(other.mp);
    this->ind = other.ind;
  }
  template <typename T2, typename R2> Subset(Vec<T2, R2> &other) {
    this->p = &other.d;
  }
  template <typename T2, typename R2, typename TraitOther>
  Subset(const Vec<T2, R2, TraitOther> &other) {
    this->p = &other.d;
  }
#ifdef STANDALONE_ETR
#else
  Subset(SEXP) = delete;
#endif
  Subset(std::size_t i) = delete;
  Subset(int i) = delete;
  Subset() = delete;
  Subset(std::size_t r, std::size_t c) = delete;
  Subset(std::size_t r, std::size_t c, const double value) = delete;

  void resizeInd(int newSize) { ind.resize(newSize); }
  void setInd(int idx, int val) { ind[idx] = val; }
  void setPtr(const T *pOther) { this->p = pOther; }

  CurrentBaseType &operator[](std::size_t pos) {
    ass(this->p != nullptr, "Subset is pointing to nothing!");
    return this->p->operator[](ind[pos % p->size()]);
  }

  CurrentBaseType operator[](std::size_t pos) const {
    ass(p != nullptr, "Subset is pointing to nothing!");
    return this->p->operator[](ind[pos % p->size()]);
  }

  ~Subset() {}
};

template <typename T, typename SubsetTrait>
struct Subset<const T, SubsetTrait> {
  using RetType = BaseType;
  using Type = T;
  using TypeTrait = SubsetTrait;
  using CaseTrait = SubsetTrait;
  Indices ind;
  const T *p;
  using CurrentBaseType =
      typename std::remove_reference<decltype(*p)>::type::RetType;
  MatrixParameter mp;

  std::size_t size() const { return ind.size(); }
  bool im() const { return mp.im(); }
  std::size_t nc() const { return mp.nc(); }
  std::size_t nr() const { return mp.nr(); }
  void setMatrix(bool i, std::size_t nrow, std::size_t ncol) {
    mp.setMatrix(i, nrow, ncol);
  }
  void setMatrix(MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void setMatrix(const MatrixParameter &mp_) {
    mp.setMatrix(mp_.ismatrix, mp_.rows, mp_.cols);
  }
  void resize(std::size_t newSize) { p->resize(newSize); }
  void realloc(std::size_t newSize) { p->realloc(newSize); }

  Subset(const Subset &other) : p(other.p) {
    this->setMatrix(other.mp);
    this->ind = other.ind;
  }
  Subset(const Subset &&other) : p(other.p) {
    this->p = other.p;
    this->setMatrix(other.mp);
    this->ind = other.ind;
  }
  template <typename T2, typename R2>
  Subset(Vec<T2, R2> &other) : p(&other.d) {}
  template <typename T2, typename R2, typename TraitOther>
  Subset(const Vec<T2, R2, TraitOther> &other) : p(&other.d) {}
#ifdef STANDALONE_ETR
#else
  Subset(SEXP) = delete;
#endif
  Subset(std::size_t i) = delete;
  Subset(int i) = delete;
  Subset() = delete;
  Subset(std::size_t r, std::size_t c) = delete;
  Subset(std::size_t r, std::size_t c, const double value) = delete;

  void resizeInd(int newSize) { ind.resize(newSize); }
  void setInd(int idx, int val) { ind[idx] = val; }
  void setPtr(const T *pOther) { this->p = pOther; }

  const CurrentBaseType operator[](std::size_t pos) const {
    ass(p != nullptr, "Subset is pointing to nothing!");
    return this->p->operator[](ind[pos % p->size()]);
  }

  ~Subset() {}
};

} // namespace etr

#endif
