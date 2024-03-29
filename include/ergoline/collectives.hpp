#ifndef __ERGOLINE_COLLECTIVES_HPP__
#define __ERGOLINE_COLLECTIVES_HPP__

#include <hypercomm/messaging/delivery.hpp>
#include <hypercomm/serialization/traits.hpp>

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
  if (hypercomm::is_bytes<Value>::value) {
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

template <typename T>
inline hypercomm::is_valid_endpoint_t<T> broadcast_value(
    const CkArrayID &aid, const T &ep_, hypercomm::value_ptr &&value) {
  hypercomm::deliverable dev(std::move(value), ep_);
  CkEnforce((bool)dev && (bool)dev.endpoint());

  auto *msg = hypercomm::deliverable::to_message(std::move(dev));
  dev.endpoint().export_to(msg);

  auto *env = UsrToEnv(msg);
  env->setMsgtype(ForArrayEltMsg);

  auto *amsg = (CkArrayMessage *)msg;
  amsg->array_setIfNotThere(CkArray_IfNotThere_buffer);

  CProxy_ArrayBase proxy(aid);
  proxy.ckBroadcast(amsg, env->getEpIdx());
}
}

#endif
