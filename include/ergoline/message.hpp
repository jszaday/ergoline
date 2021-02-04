#ifndef __ERGOLINE_MESSAGE_HPP__
#define __ERGOLINE_MESSAGE_HPP__

#include <map>
#include <string>
#include <memory>

#include "unpacker.hpp"

namespace ergoline {

namespace {
inline char* get_msg_buffer(void* msg) {
  auto env = UsrToEnv(msg);
  auto idx = env->getMsgIdx();

  if (idx == CMessage_CkMarshallMsg::__idx) {
    return static_cast<CkMarshallMsg*>(msg)->msgBuf;
  } else if (idx == CMessage_CkReductionMsg::__idx) {
    return static_cast<char*>(static_cast<CkReductionMsg*>(msg)->getData());
  } else if (idx == CMessage_CkDataMsg::__idx) {
    return static_cast<char*>(static_cast<CkDataMsg*>(msg)->getData());
  } else {
    CkAbort("unsure how to handle msg of type %s.", _msgTable[idx]->name);
  }
}
}

template <typename... Args>
void unpack(void* msg, Args&... args) {
  auto source = std::shared_ptr<void>(msg, [](void* msg) { CkFreeMsg(msg); });
  auto buffer = get_msg_buffer(msg);
  unpack(source, buffer, std::forward_as_tuple(args...));
}

template <typename... Args>
CkMessage* pack(const Args&... _args) {
  auto args = std::forward_as_tuple(const_cast<Args&>(_args)...);
  auto size = PUP::size(args);
  auto msg = CkAllocateMarshallMsg(size);
  PUP::toMemBuf(args, msg->msgBuf, size);
  return msg;
}
}

#endif
