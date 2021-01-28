#ifndef __ERGOLINE_REDUCER_HPP__
#define __ERGOLINE_REDUCER_HPP__

#include <map>
#include <memory>
#include <functional>

#include <charm++.h>
#include "NDMeshStreamer.h"

namespace ergoline {

namespace {
template <bool B>
using Requires = PUP::Requires<B>;
}

template <typename T, typename... Args,
          Requires<(sizeof...(Args) > 1)> = nullptr>
void contribute(T* t, const Args&... arguments, CkReduction::reducerType ty, const CkCallback& cb) {
  auto args = std::forward_as_tuple(const_cast<Args&>(arguments)...);
  auto size = PUP::size(args);
  auto buf = new char[size];
  PUP::toMemBuf(args, buf, size);
  t->contribute(size, buf, ty, cb);
  delete[] buf;
}

template <typename T, typename Value>
void contribute(T* t, const Value& value, CkReduction::reducerType ty, const CkCallback& cb) {
  if (is_PUPbytes<Value>::value) {
    t->contribute(sizeof(Value), &value, ty, cb);
  } else {
    auto val = const_cast<Value&>(value);
    auto size = PUP::size(val);
    auto buf = new char[size];
    PUP::toMemBuf(val, buf, size);
    t->contribute(size, buf, ty, cb);
    delete[] buf;
  }
}

template <typename T>
void contribute(T* t, const CkCallback& cb) {
  t->contribute(cb);
}
}

#endif
