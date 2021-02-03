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

struct hashable {
  virtual std::size_t hash() = 0;
};

struct object: public hashable {
  virtual bool equals(std::shared_ptr<object> other) {
    return this == other.get();
  }
};

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

}

#include "hash.hpp"

#endif
