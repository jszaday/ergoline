#ifndef __ERGOLINE_OBJECT_HPP__
#define __ERGOLINE_OBJECT_HPP__

#include <memory>
#include <charm++.h>
#include "util.hpp"

namespace ergoline {

template <typename T>
inline typename std::enable_if<
    std::is_constructible<T, PUP::reconstruct>::value>::type
reconstruct(T* p) {
  ::new (p) T(PUP::reconstruct());
}

template <typename T>
inline typename std::enable_if<
    std::is_constructible<T, CkMigrateMessage*>::value &&
    !std::is_constructible<T, PUP::reconstruct>::value>::type
reconstruct(T* p) {
  ::new (p) T(nullptr);
}

template <typename T>
inline typename std::enable_if<
    !std::is_constructible<T, PUP::reconstruct>::value &&
    !std::is_constructible<T, CkMigrateMessage*>::value &&
    std::is_default_constructible<T>::value>::type
reconstruct(T* p) {
  ::new (p) T();
}

template <typename T>
struct temporary {
  typename std::aligned_storage<sizeof(T), alignof(T)>::type data;

  temporary() {
    reconstruct(&(this->value()));
  }

  ~temporary() {
    value().~T();
  }

  const T& value(void) const {
    return *(reinterpret_cast<const T*>(&data));
  }

  T& value(void) {
    return *(reinterpret_cast<T*>(&data));
  }
};

struct hashable {
  virtual std::size_t hash() = 0;
};

struct object: public hashable {
  virtual bool equals(std::shared_ptr<object> other) {
    return this == other.get();
  }
};

template <typename T>
inline bool is_bytes() {
  return PUP::as_bytes<T>::value;
}

template <typename T>
inline std::shared_ptr<T> from_pupable(std::shared_ptr<PUP::able> p) {
  std::shared_ptr<T> q = std::dynamic_pointer_cast<T>(p);
  if (p && !q) CkAbort("cannot cast %s to %s", typeid(*p).name(), typeid(T).name());
  return q;
}

template <typename T>
inline std::shared_ptr<PUP::able> to_pupable(const std::shared_ptr<T>& p) {
  std::shared_ptr<PUP::able> q = std::dynamic_pointer_cast<PUP::able>(p);
  if (p && !q) CkAbort("cannot cast %s to PUP::able", typeid(T).name());
  return q;
}

template <class T, typename Enable = void>
struct equal_to {
  inline bool operator()(const T &lhs, const T &rhs) const {
    return lhs == rhs;
  }
};

template <class T>
struct equal_to<std::shared_ptr<T>,
                typename std::enable_if<std::is_base_of<object, T>::value>::type> {
  inline bool operator()(const std::shared_ptr<T> &lhs, const std::shared_ptr<T> &rhs) const {
    return (!lhs || !rhs) ? (lhs == rhs) : (lhs->equals(rhs));
  }
};

namespace hash_utils {
template <class, typename Enable = void>
struct hash;

template <class T>
inline void hash_combine(std::size_t& seed, const T& v) {
  hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

template <class T>
inline std::size_t hash_iterable(const T& t) {
  std::size_t seed = 0;
  for (const auto& i : t) {
    hash_combine(seed, i);
  }
  return seed;
}
}

template <typename K, typename V>
using hash_map = std::unordered_map<K, V, hash_utils::hash<K>, equal_to<K>>;

}

#include "hash.hpp"

#endif
