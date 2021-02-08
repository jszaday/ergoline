#ifndef __ERGOLINE__FUTURE_HPP__
#define __ERGOLINE__FUTURE_HPP__

#include <ckfutures.h>

namespace ergoline {
template <typename T>
struct future;
}

#include <ergoline/message.hpp>

namespace ergoline {
template <typename T>
struct future {
  CkFuture f;

  future(void) { this->f = CkCreateFuture(); }
  future(PUP::reconstruct) {}
  future(const future<T>& other): f(other.f) {}

  void set(const T& t) { CkSendToFuture(f, ergoline::pack(t)); }

  T get() {
    auto msg = CkWaitFuture(f);
    CkReleaseFuture(f);
    PUP::detail::TemporaryObjectHolder<T> h;
    ergoline::unpack(msg, h.t);
    return h.t;
  }

  bool isReady() { return static_cast<bool>(CkProbeFuture(f)); }

  void pup(PUP::er& p) { p | this->f; }
};
}

#endif
