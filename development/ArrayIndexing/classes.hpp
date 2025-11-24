template <typename T>
class Holder {
  std::optional<T> val;
  T* ptr = nullptr;

public:
  // Construct from lvalue
  Holder(T& ref) : ptr(&ref) {}
  // Construct from rvalue
  Holder(T&& r) {
    val.emplace(std::move(r));
    ptr = &val.value();
  }
  // Copy constructor
  Holder(const Holder& other) {
    if (other.val.has_value()) {
      val.emplace(*other.val);
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }
  // Copy assignment
  Holder& operator=(const Holder& other) {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }
  // Move constructor
  Holder(Holder&& other) noexcept {
    if (other.val.has_value()) {
      val.emplace(std::move(*other.val));
      ptr = &val.value();
    } else {
      ptr = other.ptr;
    }
  }
  // Move assignment
  Holder& operator=(Holder&& other) noexcept {
    if (this != &other) {
      if (other.val.has_value()) {
        val.emplace(std::move(*other.val));  // avoid operator=
        ptr = &val.value();
      } else {
        val.reset();
        ptr = other.ptr;
      }
    }
    return *this;
  }

  T& get() const {
    return *ptr;
  }
};

struct Array{
  std::vector<int> data;
  std::vector<std::size_t> dim;

  template<std::size_t N>
  Array(std::vector<int> data_, const std::array<std::size_t, N>& dim_)
  : data(std::move(data_)), dim(dim_.begin(), dim_.end()) {}

  Array() = default;
  Array(std::vector<int> data_, std::vector<std::size_t> dim_)
  : data(std::move(data_)), dim(std::move(dim_)) {}

  decltype(auto) operator[](std::size_t idx) { return data[idx]; }
  decltype(auto) operator[](std::size_t idx) const { return data[idx]; }
  std::size_t size() const {return data.size();}

};

struct OneBased {};
struct ZeroBased {};
template<typename O, std::size_t N, typename Mode = ZeroBased> struct SubsetView;

template<typename O, std::size_t N, typename Mode>
struct SubsetView {
  Holder<O> obj;
  Holder<std::vector<std::size_t>> indices;
  std::array<std::size_t, N> dim;

  SubsetView(O& obj_, std::vector<std::size_t>&& indices_,
             const std::array<std::size_t, N>& dim_) :
    obj(obj_), indices(std::move(indices_)), dim(dim_) {}
  SubsetView(O& obj_, std::vector<std::size_t>& indices_,
             const std::array<std::size_t, N>& dim_) :
    obj(obj_), indices(indices_), dim(dim_) {}

  auto operator[](std::size_t i) const {
    if constexpr (std::is_same_v<Mode, OneBased>) {
      i--;
      return obj.get()[safe_modulo(((indices.get()[
                                   safe_modulo(i, indices.get().size())
                                   ])), obj.get().size()
                                   )];
    } else {
      return obj.get()[safe_modulo(((indices.get()[
                                   safe_modulo(i, indices.get().size())
                                   ])), obj.get().size()
                                   )];
    }
  }
  auto& operator[](std::size_t i) {
    if constexpr (std::is_same_v<Mode, OneBased>) {
      i--;
      return obj.get()[safe_modulo(((indices.get()[
                                   safe_modulo(i, indices.get().size())
                                   ])), obj.get().size()
                                   )];
    } else {
      return obj.get()[safe_modulo(((indices.get()[
                                   safe_modulo(i, indices.get().size())
                                   ])), obj.get().size()
                                   )];
    }
  }
};
