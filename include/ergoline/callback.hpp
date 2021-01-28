#ifndef __ERGOLINE_CALLBACK_HPP__
#define __ERGOLINE_CALLBACK_HPP__

#include <map>
#include <memory>
#include <cstring>
#include <functional>

#include <charm++.h>

namespace ergoline {
using callback_fn_t = std::function<bool(CkMarshallMsg*)>;

namespace {
using callback_id_t = std::size_t;
using callback_map_t = std::map<callback_id_t, callback_fn_t>;

CkpvDeclare(callback_id_t, last_callback_id_);
CkpvDeclare(callback_map_t, callback_map_);

CkMarshallMsg* msg_to_marshall_msg(CkMessage* msg);

void callback_client_fn(void* param, void* message) {
  auto id = reinterpret_cast<callback_id_t>(param);
  CkAssert(CkpvInitialized(callback_map_));
  auto msg = msg_to_marshall_msg(static_cast<CkMessage*>(message));
  bool deregister = CkpvAccess(callback_map_)[id](msg);
  if (deregister) {
    CkpvAccess(callback_map_).erase(id);
  }
}

callback_id_t get_next_id() {
  if (!CkpvInitialized(last_callback_id_)) {
    CkpvInitialize(callback_id_t, last_callback_id_);
    CkpvAccess(last_callback_id_) = 0;
  }
  return CkpvAccess(last_callback_id_)++;
}

callback_id_t register_callback(const callback_fn_t& f) {
  if (!CkpvInitialized(callback_map_)) {
    CkpvInitialize(callback_map_t, callback_map_);
  }
  auto id = get_next_id();
  CkpvAccess(callback_map_)[id] = f;
  return id;
}

CkMarshallMsg* msg_to_marshall_msg(CkMessage* msg) {
  auto env = UsrToEnv(msg);
  auto idx = env->getMsgIdx();
  if (idx == CMessage_CkMarshallMsg::__idx) {
    return static_cast<CkMarshallMsg*>(msg);
  } else if (idx == CMessage_CkReductionMsg::__idx) {
    auto red = static_cast<CkReductionMsg*>(msg);
    auto res = CkAllocateMarshallMsg(red->getLength());
    std::memcpy(res->msgBuf, red->getData(), red->getLength());
    delete red;
    return res;
  } else {
    CkAbort("unsure how to handle msg of type %s.", _msgTable[idx]->name);
  }
}
}

CkCallback make_callback(const callback_fn_t& f) {
  auto id = register_callback(f);
  return CkCallback(&callback_client_fn, reinterpret_cast<void*>(id));
}

template <typename T>
CkCallback make_callback(const ck::future<T>& f) {
  auto fn = [f](CkMarshallMsg* msg) {
    CkSendToFuture(f.handle(), msg);
    return true;
  };
  auto id = register_callback(fn);
  return CkCallback(&callback_client_fn, reinterpret_cast<void*>(id));
}
}

#endif
