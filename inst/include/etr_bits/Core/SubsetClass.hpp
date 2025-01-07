#ifndef SUBSETCLASS_ETR_H
#define SUBSETCLASS_ETR_H

#include "BaseStore.hpp"

namespace etr {
// Points to a Variable and stores indicces in ind
template <typename T, typename SubsetTrait> struct Subset {
  using RetType = BaseType;
  using Type = T;
  using TypeTrait = SubsetTrait;
  Indices ind;

  T *p = nullptr;
#ifdef DERIV_ETR
  BaseStore<double> *deriv_p = nullptr;
#endif
  using CurrentBaseType =
      typename ReRef<decltype(*p)>::type::Type;
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

#ifdef DERIV_ETR
  Subset(const Subset &other) : p(other.p), deriv_p(other.deriv_p) {
    this->setMatrix(other.mp);
    this->ind = other.ind;
  }
  Subset(const Subset &&other) : p(other.p), deriv_p(other.deriv_p) {
    this->setMatrix(other.mp);
    this->ind = other.ind;
  }
  template <typename T2, typename R2> Subset(Vec<T2, R2> &other) {
    this->p = &other.d;
    this->deriv_p = &other.deriv;
  }
  template <typename T2, typename R2, typename TraitOther>
  Subset(const Vec<T2, R2, TraitOther> &other) {
    this->p = &other.d;
    this->deriv_p = &other.deriv;
  }
  template <typename T2, typename R2> Subset(Vec<T2, R2> &other, bool deriv) {
    this->p = &other.d;
    this->deriv_p = &other.deriv;
  }
  template <typename T2, typename R2, typename TraitOther>
  Subset(const Vec<T2, R2, TraitOther> &other, bool deriv) {
    this->p = &other.d;
    this->deriv_p = &other.deriv;
  }

#else
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
  template <typename T2, typename R2>
  Subset(const Vec<T2, R2> &other) {
    this->p = &other.d;
  }
  template <typename T2, typename R2> Subset(Vec<T2, R2> &other, bool deriv) {
    this->p = &other.deriv;
  }
  template <typename T2, typename R2>
  Subset(const Vec<T2, R2> &other, bool deriv) {
    this->p = &other.deriv;
  }

#endif

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
    ass<"Subset is pointing to nothing!">(this->p != nullptr);
    return this->p->operator[](ind[pos % p->size()]);
  }

  CurrentBaseType operator[](std::size_t pos) const {
    ass<"Subset is pointing to nothing!">(p != nullptr);
    return this->p->operator[](ind[pos % p->size()]);
  }

#ifdef DERIV_ETR

  double &get_deriv(std::size_t pos) {
    ass<"Subset is pointing to nothing!">(this->deriv_p != nullptr);
    ass<"Error: out of boundaries --> value below 1">(pos >= 0);
    ass<"Error: out of boundaries">(
        pos < (this->deriv_p->size()));

    return this->deriv_p->operator[](ind[pos % deriv_p->size()]);
  }

  double get_deriv(std::size_t pos) const {
    ass<"Subset is pointing to nothing!">(deriv_p != nullptr);
    ass<"Error: out of boundaries --> value below 1">(pos >= 0);
    ass<"Error: out of boundaries">(
        pos < (this->deriv_p->size()));
    return this->deriv_p->operator[](ind[pos % deriv_p->size()]);
  }
#endif
  ~Subset() {}
};

template <typename T, typename SubsetTrait>
struct Subset<const T, SubsetTrait> {
  using RetType = BaseType;
  using Type = T;
  using TypeTrait = SubsetTrait;
  Indices ind;
  const T *p;

#ifdef DERIV_ETR
  const BaseStore<double> *deriv_p = nullptr;
#endif

  using CurrentBaseType =
      typename ReRef<decltype(*p)>::type::RetType;
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

#ifdef DERIV_ETR
  Subset(const Subset &other) : p(other.p), deriv_p(other.deriv_p) {
    this->setMatrix(other.mp);
    this->ind = other.ind;
  }
  Subset(const Subset &&other) : p(other.p), deriv_p(other.deriv_p) {
    this->p = other.p;
    this->setMatrix(other.mp);
    this->ind = other.ind;
  }
  template <typename T2, typename R2>
  Subset(Vec<T2, R2> &other) : p(&other.d), deriv_p(other.deriv) {}
  template <typename T2, typename R2, typename TraitOther>
  Subset(const Vec<T2, R2, TraitOther> &other)
      : p(&other.d), deriv_p(&other.deriv) {}
#else
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
  template <typename T2, typename R2>
  Subset(const Vec<T2, R2> &other) : p(&other.d) {}
#endif
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
    ass<"Subset is pointing to nothing!">(p != nullptr);
    return this->p->operator[](ind[pos % p->size()]);
  }

#ifdef DERIV_ETR

  const double get_deriv(std::size_t pos) const {
    ass<"Subset is pointing to nothing!">(deriv_p != nullptr);
    ass<"Error: out of boundaries --> value below 1">(pos >= 0);
    ass<"Error: out of boundaries">(
        pos < (this->deriv_p->size()));

    return this->deriv_p->operator[](ind[pos % deriv_p->size()]);
  }
#endif

  ~Subset() {}
};

} // namespace etr

#endif
