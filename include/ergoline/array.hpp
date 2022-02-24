#ifndef __ERGOLINE_ARRAY_HPP__
#define __ERGOLINE_ARRAY_HPP__

#include <cstdlib>
#include <hypercomm/serialization/pup.hpp>

namespace ergoline {

namespace detail {
template <std::size_t N>
inline std::size_t reduce(const std::array<std::size_t, N> &shape) {
  if (N == 0) {
    return 0;
  } else if (N == 1) {
    return shape[0];
  } else {
    auto tmp = shape[0];
    for (auto i = 1; i < N; i++) {
      tmp *= shape[i];
    }
    return tmp;
  }
}
}  // namespace detail

template <typename T, std::size_t N>
struct nd_span {
  static_assert(N >= 0, "dimensionality must be positive");

  using self_type = nd_span<T, N>;
  using shape_type = std::array<std::size_t, N>;

  static constexpr auto non_trivial = !hypercomm::is_bytes<T>::value;
  static constexpr auto shape_size = sizeof(shape_type);
  static constexpr auto pad_amount = ALIGN_DEFAULT(shape_size) - shape_size;

  shape_type shape;
  char padding[pad_amount];

  ~nd_span() {
    if (non_trivial) {
      for (auto &elt : *this) {
        elt.~T();
      }
    }
  }

  inline T *data(void) const { return const_cast<self_type *>(this)->begin(); }

  inline T *begin(void) {
    return reinterpret_cast<T *>(reinterpret_cast<char *>(this) +
                                 sizeof(self_type));
  }

  inline T *end(void) { return this->begin() + this->size(); }

  inline std::size_t size() const { return detail::reduce(this->shape); }

  inline T &operator[](const std::size_t &idx) {
    return *(this->begin() + idx);
  }

  inline static std::size_t total_size(
      const std::array<std::size_t, N> &shape) {
    return sizeof(self_type) + detail::reduce(shape) * sizeof(T);
  }

  static void *operator new(std::size_t,
                            const std::array<std::size_t, N> &shape) {
    return aligned_alloc(alignof(nd_span<T, N>), total_size(shape));
  }

  static void operator delete(void *ptr) { std::free(ptr); }
  static void operator delete(void *ptr, const std::array<std::size_t, N> &) {
    std::free(ptr);
  }

 private:
  nd_span(const std::array<std::size_t, N> &_, const bool &init) : shape(_) {
    if (non_trivial && init) {
      for (auto &elt : *this) {
        hypercomm::reconstruct(&elt);
      }
    }
  }

 public:
  static std::shared_ptr<self_type> instantiate(
      const std::array<std::size_t, N> &shape, const bool &init = true) {
    return std::shared_ptr<self_type>(new (shape) self_type(shape, init));
  }

  template <class... Args>
  static std::shared_ptr<nd_span<T, N>> fill(const std::tuple<Args...> &shape,
                                             const T &value);

  static std::shared_ptr<nd_span<T, 1>> fill(const int &shape, const T &value);
};

template <typename T, std::size_t N>
struct packable_slice;

template <typename T>
struct packable_slice<T, 2> {
  using span_type = nd_span<T, 2>;
  using shape_type = typename span_type::shape_type;
  constexpr static std::size_t pad_amount = span_type::pad_amount;

  shape_type shape;
  char padding[pad_amount];
  std::shared_ptr<T> data;

  packable_slice(const std::shared_ptr<span_type> &span, std::size_t n_rows,
                 std::size_t offset) {
    auto n_cols = span->shape[1];
    this->shape = {n_rows, n_cols};
    auto *start = span->data() + n_cols * offset;
    this->data = std::shared_ptr<T>(span, start);
  }

  inline std::size_t size() const { return this->shape[0] * this->shape[1]; }

  std::size_t total_size(void) const {
    return sizeof(shape_type) + pad_amount + this->size() * sizeof(T);
  }
};

template <>
template <>
std::shared_ptr<nd_span<double, 2>> nd_span<double, 2>::fill<int, int>(
    const std::tuple<int, int> &tup, const double &value) {
  auto span = instantiate(
      {(std::size_t)std::get<0>(tup), (std::size_t)std::get<1>(tup)});
  std::fill(span->begin(), span->end(), value);
  return span;
}

template <>
std::shared_ptr<nd_span<double, 1>> nd_span<double, 1>::fill(
    const int &size, const double &value) {
  auto span = instantiate({(std::size_t)size});
  std::fill(span->begin(), span->end(), value);
  return span;
}

template <typename T, std::size_t N>
using array = nd_span<T, N>;

template <typename T, std::size_t N>
std::size_t total_size(const std::shared_ptr<nd_span<T, N>> &span) {
  return nd_span<T, N>::total_size(span->shape);
}

template <typename T>
constexpr std::size_t offset_for(void) {
  return T::offset;
}
}  // namespace ergoline

namespace hypercomm {

template <class T, std::size_t N>
struct is_idiosyncratic_ptr<ergoline::nd_span<T, N>> {
  static constexpr auto value = true;
};

template <typename T, std::size_t N>
struct is_zero_copyable<ergoline::nd_span<T, N>> {
  static constexpr auto value = is_bytes<T>::value;
};

template <typename T, std::size_t N>
struct quick_sizer<ergoline::nd_span<T, N>,
                   typename std::enable_if<is_bytes<T>::value>::type> {
  inline static std::size_t impl(const ergoline::nd_span<T, N> &t) {
    return ergoline::nd_span<T, N>::total_size(t.shape);
  }
};

template <typename T, std::size_t N>
struct zero_copy_fallback<ergoline::nd_span<T, N>,
                          typename std::enable_if<is_bytes<T>::value>::type> {
  using type = std::shared_ptr<ergoline::nd_span<T, N>>;
  using shape_type = typename ergoline::nd_span<T, N>::shape_type;

  inline static void unpack(serdes &s, type &t) {
    auto *shape = reinterpret_cast<shape_type *>(s.current);
    auto totalSize = ergoline::nd_span<T, N>::total_size(*shape);
    new (&t) type(s.observe_source(),
                  reinterpret_cast<ergoline::nd_span<T, N> *>(s.current));
    s.advanceBytes(totalSize);
  }

  inline static void pack(serdes &s, type &t) {
    auto totalSize = ergoline::nd_span<T, N>::total_size(t->shape);
    auto *raw = reinterpret_cast<char *>(t.get());
    s.copy(raw, totalSize);
  }
};

template <typename T, std::size_t N>
struct puper<std::shared_ptr<ergoline::nd_span<T, N>>,
             typename std::enable_if<!is_bytes<T>::value>::type> {
  using shape_type = typename ergoline::nd_span<T, N>::shape_type;
  inline static void impl(serdes &s,
                          std::shared_ptr<ergoline::nd_span<T, N>> &t) {
    if (s.unpacking()) {
      auto *shape = reinterpret_cast<shape_type *>(s.current);
      s.advanceBytes(sizeof(shape_type));
      new (&t) std::shared_ptr<ergoline::nd_span<T, N>>(
          ergoline::nd_span<T, N>::instantiate(*shape, false));
    } else {
      s | t->shape;
    }
    for (auto &elt : *t) {
      s | elt;
    }
  }
};

template <typename T, std::size_t N>
struct puper<ergoline::packable_slice<T, N>,
             typename std::enable_if<is_bytes<T>::value>::type> {
  inline static void impl(serdes &s, ergoline::packable_slice<T, N> &t) {
    ptr_record *rec;
    if (s.unpacking()) {
      CkAbort("slices must be unpacked as arrays!");
    } else {
      rec = s.get_record(t.data, []() { return 0; });
    }

    pup(s, *rec);

    if (rec->is_instance()) {
      s | t.shape;
      s | t.padding;
      s.copy(t.data.get(), t.size());
    }
  }
};
}  // namespace hypercomm

#endif
