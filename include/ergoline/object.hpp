#ifndef __ERGOLINE_OBJECT_HPP__
#define __ERGOLINE_OBJECT_HPP__

#include <memory>
#include "pup.h"

namespace ergoline {
struct puppable : public PUP::able {
  puppable() {}
  virtual ~puppable() {}
  puppable(CkMigrateMessage *m) : PUP::able(m) {}
};

struct object {
  virtual puppable *toPuppable() = 0;
  virtual std::size_t hash() = 0;
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
}

#endif
