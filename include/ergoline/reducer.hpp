#ifndef __ERGOLINE_REDUCER_HPP__
#define __ERGOLINE_REDUCER_HPP__

#include <map>
#include <memory>
#include <functional>

#include <charm++.h>

namespace ergoline {
// TODO make this return bool
//      true means unregister the function, false means keep it
using reducer_fn_t = std::function<void(int, void*)>;

namespace {
using reducer_id_t = std::size_t;
using reducer_map_t = std::map<reducer_id_t, reducer_fn_t>;

CkpvDeclare(reducer_id_t, last_reducer_id_);
CkpvDeclare(reducer_map_t, reducer_map_);

void reduction_client_fn(void* param, void* message) {
  auto id = reinterpret_cast<reducer_id_t>(param);
  auto msg = static_cast<CkReductionMsg*>(message);
  CkAssert(CkpvInitialized(reducer_map_));
  CkpvAccess(reducer_map_)[id](msg->getLength(), msg->getData());
}

reducer_id_t get_next_id() {
  if (!CkpvInitialized(last_reducer_id_)) {
    CkpvInitialize(reducer_id_t, last_reducer_id_);
    CkpvAccess(last_reducer_id_) = -1;
  }
  return ++CkpvAccess(last_reducer_id_);
}

reducer_id_t register_reducer(const reducer_fn_t& f) {
  if (!CkpvInitialized(reducer_map_)) {
    CkpvInitialize(reducer_map_t, reducer_map_);
  }
  auto id = get_next_id();
  CkpvAccess(reducer_map_)[id] = f;
  return id;
}

template <bool B>
using Requires = PUP::Requires<B>;
}

class reducer {
  CkCallback cb_;
  CMK_REFNUM_TYPE flag_;
  CkReduction::reducerType type_;

 public:
  reducer() {}
  reducer(const reducer& r) : cb_(r.cb_), type_(r.type_), flag_(r.flag_) {}
  reducer(const CkReduction::reducerType& ty, const reducer_fn_t& action, CMK_REFNUM_TYPE flag = (CMK_REFNUM_TYPE)-1)
      : type_(ty), flag_(flag) {
    auto id = register_reducer(action);
    new (&cb_) CkCallback(&reduction_client_fn, reinterpret_cast<void*>(id));
  }

  template <typename T, typename... Args,
            Requires<(sizeof...(Args) > 1)> = nullptr>
  void contribute(T* t, const Args&... arguments) {
    auto args = std::forward_as_tuple(const_cast<Args&>(arguments)...);
    auto size = PUP::size(args);
    auto buf = new char[size];
    PUP::toMemBuf(args, buf, size);
    t->contribute(size, buf, type_, cb_, flag_);
    delete[] buf;
  }

  template <typename T, typename Value>
  void contribute(T* t, const Value& value) {
    t->contribute(sizeof(Value), &value, type_, cb_, flag_);
  }

  template <typename T>
  void contribute(T* t, int size, void* data) {
    t->contribute(size, data, type_, cb_, flag_);
  }

  template <typename T>
  void contribute(T* t) {
    t->contribute(cb_, flag_);
  }

  virtual void pup(PUP::er& p) {
    p | cb_;
    p | flag_;
    p | type_;
  }
};
}

#endif
