#ifndef COLLECTION_ETR_HPP
#define COLLECTION_ETR_HPP

#include <vector>

namespace etr {

#ifdef STANDALONE_ETR
#else

// A dynamic, homogeneous container of a single ast2ast custom (new_type)
// value, backed by std::vector<T>. T is expected to provide its own
// SEXP-constructor and to_SEXP() (both generated per custom type), which
// Collection reuses element-wise for its own R<->C++ boundary conversion.
template <typename T>
class Collection {
  static_assert(!IsScalarLike<T> && !IsArray<T>,
    "Collection can only hold a custom (new_type) type, not a primitive scalar or array");
public:
  Collection() = default;
  explicit Collection(std::size_t n) : data_(n) {}
  // Accepts the DSL's scalar wrapper types (etr::Integer, etr::Double, ...)
  // directly, matching how vector(mode = "Point", n) generates its call.
  template <typename N>
    requires IsScalarLike<N>
  explicit Collection(N n) : data_(static_cast<std::size_t>(get_val(n))) {}
  // n's type isn't required to be a strict scalar (e.g. an untyped arg
  // defaults to matrix(double)) -- same length-1-narrows-to-scalar
  // tolerance at() already gives index arguments via ExtractIndex().
  template <typename N>
    requires IsArray<Decayed<N>>
  explicit Collection(const N& n) {
    ass<"vector()'s length argument must be a scalar or a length-1 vector">(n.size() == 1);
    data_.resize(static_cast<std::size_t>(get_val(n.get(0))));
  }

  explicit Collection(SEXP arg) {
    if (!Rf_isNewList(arg)) {
      Rf_error("Expected a list to construct a Collection");
    }
    const R_xlen_t n = Rf_xlength(arg);
    data_.reserve(static_cast<std::size_t>(n));
    for (R_xlen_t i = 0; i < n; ++i) {
      data_.emplace_back(VECTOR_ELT(arg, i));
    }
  }

  SEXP to_SEXP() const {
    SEXP res = PROTECT(Rf_allocVector(VECSXP, static_cast<R_xlen_t>(data_.size())));
    for (std::size_t i = 0; i < data_.size(); ++i) {
      SET_VECTOR_ELT(res, static_cast<R_xlen_t>(i), data_[i].to_SEXP());
    }
    UNPROTECT(1);
    return res;
  }

  T& operator[](std::size_t i) {
    ass<"Error: out of boundaries">(i < data_.size());
    return data_[i];
  }
  const T& operator[](std::size_t i) const {
    ass<"Error: out of boundaries">(i < data_.size());
    return data_[i];
  }

  std::size_t size() const { return data_.size(); }

  auto begin() { return data_.begin(); }
  auto end() { return data_.end(); }
  auto begin() const { return data_.begin(); }
  auto end() const { return data_.end(); }

private:
  std::vector<T> data_;
};

template <typename T> struct is_any_collection : std::false_type {};
template <typename T> struct is_any_collection<Collection<T>> : std::true_type {};
template <typename T> inline constexpr bool is_any_collection_v = is_any_collection<T>::value;
template <typename T> concept IsCollection = is_any_collection_v<T>;

// 1-based index, matching [[ ]] in the DSL
template <typename T>
inline T& collection_at(Collection<T>& c, long i) {
  return c[static_cast<std::size_t>(i - 1)];
}
template <typename T>
inline const T& collection_at(const Collection<T>& c, long i) {
  return c[static_cast<std::size_t>(i - 1)];
}
// Accepts the DSL's scalar wrapper types (etr::Integer, ...) directly,
// matching how pts[[i]] generates its call.
template <typename T, typename I>
  requires IsScalarLike<I>
inline T& collection_at(Collection<T>& c, I i) {
  return c[static_cast<std::size_t>(get_val(i) - 1)];
}
template <typename T, typename I>
  requires IsScalarLike<I>
inline const T& collection_at(const Collection<T>& c, I i) {
  return c[static_cast<std::size_t>(get_val(i) - 1)];
}
// Same length-1-narrows-to-scalar tolerance as at()'s ExtractIndex(), for
// an index whose type isn't a strict scalar (e.g. an untyped arg defaults
// to matrix(double)).
template <typename T, typename I>
  requires IsArray<Decayed<I>>
inline T& collection_at(Collection<T>& c, const I& i) {
  ass<"collection index must be a scalar or a length-1 vector">(i.size() == 1);
  return c[static_cast<std::size_t>(get_val(i.get(0)) - 1)];
}
template <typename T, typename I>
  requires IsArray<Decayed<I>>
inline const T& collection_at(const Collection<T>& c, const I& i) {
  ass<"collection index must be a scalar or a length-1 vector">(i.size() == 1);
  return c[static_cast<std::size_t>(get_val(i.get(0)) - 1)];
}

template <typename T>
inline Integer length(const Collection<T>& c) {
  return Integer(static_cast<int>(c.size()));
}

#endif
} // namespace etr

#endif
