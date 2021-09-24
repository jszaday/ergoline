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
} // namespace detail

template <typename T, std::size_t N> struct nd_span {
  static_assert(N >= 0, "dimensionality must be positive");

  using shape_type = std::array<std::size_t, N>;
  static constexpr auto shape_size = sizeof(shape_type);

  shape_type shape;

  static constexpr auto offset = shape_size + CK_ALIGN(shape_size, 16);

  ~nd_span() {
    for (auto i = 0; i < this->size(); i += 1) {
      (*this)[i].~T();
    }
  }

  inline T *begin(void) {
    return reinterpret_cast<T *>(reinterpret_cast<char *>(this) + offset);
  }

  inline T *end(void) { return this->begin() + this->size(); }

  inline std::size_t size() const { return detail::reduce(this->shape); }

  inline T &operator[](const std::size_t &idx) {
    return *(this->begin() + idx);
  }

  inline static std::size_t
  total_size(const std::array<std::size_t, N> &shape) {
    return offset + detail::reduce(shape) * sizeof(T);
  }

  static void *operator new(std::size_t count,
                            const std::array<std::size_t, N> &shape) {
    return aligned_alloc(alignof(nd_span<T, N>), total_size(shape) * count);
  }

  static void operator delete(void *ptr) { std::free(ptr); }

private:
  nd_span(const std::array<std::size_t, N> &_) : shape(_) {}

public:
  static std::shared_ptr<nd_span<T, N>>
  instantiate(const std::array<std::size_t, N> &shape) {
    return std::shared_ptr<nd_span<T, N>>(new (shape) nd_span<T, N>(shape));
  }

  template <class... Args>
  static std::shared_ptr<nd_span<T, N>> fill(const std::tuple<Args...> &shape,
                                             const T &value);

  static std::shared_ptr<nd_span<T, 1>> fill(const int &shape, const T &value);
};

template <>
template <>
std::shared_ptr<nd_span<double, 2>>
nd_span<double, 2>::fill<int, int>(const std::tuple<int, int> &tup,
                                   const double &value) {
  auto span = instantiate(
      {(std::size_t)std::get<0>(tup), (std::size_t)std::get<1>(tup)});
  std::fill(span->begin(), span->end(), value);
  return span;
}

template <>
std::shared_ptr<nd_span<double, 1>>
nd_span<double, 1>::fill(const int &size, const double &value) {
  auto span = instantiate({(std::size_t)size});
  std::fill(span->begin(), span->end(), value);
  return span;
}

template <typename T, std::size_t N> using array = nd_span<T, N>;
} // namespace ergoline

namespace hypercomm {

template <typename T, std::size_t N>
struct is_zero_copyable<ergoline::nd_span<T, N>> {
  static constexpr auto value = is_bytes<T>();
};

template <typename T, std::size_t N>
struct quick_sizer<ergoline::nd_span<T, N>,
                   typename std::enable_if<is_bytes<T>()>::type> {
  inline static std::size_t impl(const ergoline::nd_span<T, N> &t) {
    return ergoline::nd_span<T, N>::total_size(t.shape);
  }
};

template <typename T, std::size_t N>
struct zero_copy_fallback<ergoline::nd_span<T, N>,
                          typename std::enable_if<is_bytes<T>()>::type> {
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

template <typename T, std::size_t N> struct puper<ergoline::nd_span<T, N>> {
  inline static void impl(serdes &s, ergoline::nd_span<T, N> &t) {
    CkAbort("not implemented!");
    // if (s.unpacking()) {
    //   reconstruct(&t);
    // }

    // pup(s, t.shape);

    // if (s.unpacking()) {
    //   if (PUP::as_bytes<T>::value) {
    //     t.source = s.observe_source();
    //     if ((bool)t.source) {
    //       t.buffer = reinterpret_cast<T*>(s.current);
    //       s.advance<T>(t.size());
    //     } else {
    //       t.alloc(false, false);
    //       s.copy(t.buffer, t.size());
    //     }
    //   } else {
    //     t.alloc(true, true);
    //     for (auto& i : t) {
    //       pup(s, i);
    //     }
    //   }
    // } else if (PUP::as_bytes<T>::value) {
    //   s.copy(t.buffer, t.size());
    // } else {
    //   for (auto& i : t) {
    //     pup(s, i);
    //   }
    // }
  }
};
} // namespace hypercomm

#endif
