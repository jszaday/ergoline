#ifndef __ERGOLINE_ARRAY_HPP__
#define __ERGOLINE_ARRAY_HPP__

#include "object.hpp"

namespace ergoline {

template <typename T, std::size_t N>
struct array : public hashable {
  static_assert(N >= 0, "dimensionality must be positive");

  using buffer_t = T*;
  using source_t = std::shared_ptr<void>;
  using shape_t = std::array<std::size_t, N>;

  source_t source;
  buffer_t buffer;
  shape_t shape;

  array() {}

  array(const shape_t& shape_, bool init = true) : shape(shape_) {
    this->alloc(init, false);
  }

  template <class... Args>
  array(const source_t& src_, buffer_t buffer_, Args... args)
      : source(src_), buffer(buffer_), shape{args...} {}

  array(const std::shared_ptr<T>& buffer_, const shape_t& shape_)
      : source(buffer_), buffer(buffer_.get()), shape(shape_) {}

  inline void alloc(bool init, bool shallow) {
    source = allocate(size(), init, shallow);
    buffer = reinterpret_cast<buffer_t>(source.get());
  }

  inline T& operator[](std::size_t idx) { return buffer[idx]; }

  inline buffer_t begin() { return buffer; }
  inline buffer_t end() { return buffer + size(); }

  // TODO make constexpr
  inline std::size_t size() {
    if (N == 0) {
      return 0;
    } else if (N == 1) {
      return shape[0];
    } else {
      auto tmp = shape[0];
      for (auto i = 1; i < N; i++) {
        tmp *= shape[1];
      }
      return tmp;
    }
  }

  std::size_t hash() override {
    hasher h;
    auto n = this->size();
    for (auto i = 0; i < n; i++) {
      h | (*this)[i];
    }
    return h.hash();
  }

  void pup(PUP::er& p) {
    p | shape;
    auto n = this->size();
    if (p.isUnpacking()) {
      this->alloc(true, true);
    }
    PUParray(p, buffer, n);
  }

  // TODO implement this? idk...
  template <class... Args>
  static std::shared_ptr<array<T, N>> fill(const std::tuple<Args...>& shape,
                                           const T& value);

  static std::shared_ptr<array<T, 1>> fill(const int& shape,
                                           const T& value);

 private:
  static std::shared_ptr<T> allocate(const std::size_t& n, const bool init, bool shallow) {
    if (n == 0) {
      return nullptr;
    } else {
      auto p = static_cast<T*>(malloc(sizeof(T) * n));
      for (auto i = 0; init && i < n; i++) {
        if (shallow) {
          reconstruct(p + i);
        } else {
          new (p + i) T();
        }
      }
      return std::shared_ptr<T>(p, [](void* p) { free(p); });
    }
  }
};

template <>
template <>
std::shared_ptr<array<double, 2>> array<double, 2>::fill<int, int>(
    const std::tuple<int, int>& shape, const double& value) {
  std::array<std::size_t, 2> s = {(std::size_t)std::get<0>(shape),
                                  (std::size_t)std::get<1>(shape)};
  auto a = std::make_shared<array<double, 2>>(s, false);
  std::fill(a->begin(), a->end(), value);
  return a;
}

template <>
std::shared_ptr<array<double, 1>> array<double, 1>::fill(
    const int& shape, const double& value) {
  std::array<std::size_t, 1> s = { (std::size_t) shape };
  auto a = std::make_shared<array<double, 1>>(s, false);
  std::fill(a->begin(), a->end(), value);
  return a;
}
}

#endif
