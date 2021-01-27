#ifndef __ERGOLINE_REDUCER_HPP__
#define __ERGOLINE_REDUCER_HPP__

#include <map>
#include <memory>
#include <functional>

#include <charm++.h>

namespace ergoline {

namespace {
template <bool B>
using Requires = PUP::Requires<B>;
}

class reducer {
  CkCallback cb_;
  CMK_REFNUM_TYPE flag_;

 public:
  reducer() {}
  reducer(const reducer& r) : cb_(r.cb_), flag_(r.flag_) {}
  reducer(const CkCallback& cb, CMK_REFNUM_TYPE flag = (CMK_REFNUM_TYPE)-1)
      : cb_(cb), flag_(flag) {}

  template <typename T, typename... Args,
            Requires<(sizeof...(Args) > 1)> = nullptr>
  void contribute(T* t, CkReduction::reducerType ty, const Args&... arguments) {
    auto args = std::forward_as_tuple(const_cast<Args&>(arguments)...);
    auto size = PUP::size(args);
    auto buf = new char[size];
    PUP::toMemBuf(args, buf, size);
    t->contribute(size, buf, type_, cb_, flag_);
    delete[] buf;
  }

  template <typename T, typename Value>
  void contribute(T* t, CkReduction::reducerType ty, const Value& value) {
    t->contribute(sizeof(Value), &value, ty, cb_, flag_);
  }

  template <typename T>
  void contribute(T* t, CkReduction::reducerType ty, int size, void* data) {
    t->contribute(size, data, ty, cb_, flag_);
  }

  template <typename T>
  void contribute(T* t) {
    t->contribute(cb_, flag_);
  }

  virtual void pup(PUP::er& p) {
    p | cb_;
    p | flag_;
  }
};
}

#endif
