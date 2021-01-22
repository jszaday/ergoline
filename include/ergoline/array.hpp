#ifndef __ERGOLINE_ARRAY_HPP__
#define __ERGOLINE_ARRAY_HPP__

#include "object.hpp"

namespace ergoline {

template <typename T, std::size_t N>
struct array : public hashable {
  static_assert(N >= 0, "dimensionality must be positive");

  using buffer_t = std::shared_ptr<T>;
  using shape_t = std::array<std::size_t, N>;

  buffer_t buffer;
  shape_t shape;

  array() {}

  array(const shape_t& shape_) : shape(shape_) { buffer = allocate(size()); }

  template <class... Args>
  array(T* buffer_, Args... args)
      : shape{args...} {
    buffer = std::shared_ptr<T>(std::shared_ptr<T>{}, buffer_);
  }

  array(const std::shared_ptr<T>& buffer_, const shape_t& shape_)
      : shape(shape_), buffer(buffer_) {}

  inline T& operator[](std::size_t idx) { return (buffer.get())[idx]; }

  inline T* begin() { return buffer.get(); }
  inline T* end() { return buffer.get() + size(); }

  std::size_t size() {
    if (N == 0)
      return 0;
    else if (N == 1)
      return shape[0];
    else {
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
      buffer = allocate(n);
    }
    PUParray(p, buffer.get(), n);
  }

  // TODO implement this? idk...
  template<class... Args>
  static std::shared_ptr<array<T, N>> fill(const std::tuple<Args...>& shape, const T& value);

 private:
  static buffer_t allocate(const std::size_t& n) {
    if (n == 0) {
      return buffer_t(nullptr);
    } else {
      return buffer_t(static_cast<T*>(malloc(sizeof(T) * n)),
                      [](void* p) { free(p); });
    }
  }
};

template <>
template <>
std::shared_ptr<array<double, 2>> array<double, 2>::fill<int, int>(const std::tuple<int, int>& shape, const double& value) {
  std::array<std::size_t, 2> s = { (std::size_t)std::get<0>(shape), (std::size_t)std::get<1>(shape) };
  auto a = std::make_shared<array<double, 2>>(s);
  std::fill(a->begin(), a->end(), value);
  return a;
}

}

#endif
