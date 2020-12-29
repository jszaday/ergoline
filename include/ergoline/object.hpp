#ifndef __ERGOLINE_OBJECT_HPP__
#define __ERGOLINE_OBJECT_HPP__

#include <memory>
#include "hash.hpp"
#include "pup.h"

namespace ergoline {
struct object: public hashable {
  virtual bool equals(std::shared_ptr<object> other) {
    return this == other.get();
  }
};

template <typename T, typename U,
          typename std::enable_if<std::is_base_of<object, T>::value &&
                                  std::is_base_of<object, U>::value>::type>
inline bool operator==(const std::shared_ptr<T> &t,
                       const std::shared_ptr<U> &u) {
  return (!t || !u) ? (t.get() == u.get()) : (t->equals(u));
}

template <typename T>
inline std::shared_ptr<T> from_pupable(PUP::able* p) {
  std::shared_ptr<T> q = std::dynamic_pointer_cast<T>(std::shared_ptr<PUP::able>(p));
  if (p && !q) CkAbort("cannot cast %s to %s", typeid(*p).name(), typeid(T).name());
  return q;
}

template <typename T>
inline CkPointer<PUP::able> to_pupable(const std::shared_ptr<T>& p) {
  std::shared_ptr<PUP::able> q = std::dynamic_pointer_cast<PUP::able>(p);
  if (p && !q) CkAbort("cannot cast %s to PUP::able", typeid(T).name());
  return CkPointer<PUP::able>(q.get());
}

}

#endif
