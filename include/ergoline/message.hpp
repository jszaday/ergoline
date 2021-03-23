#ifndef __ERGOLINE_MESSAGE_HPP__
#define __ERGOLINE_MESSAGE_HPP__

#include <map>
#include <string>
#include <memory>
#include <hypercomm/pup.hpp>

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
  std::shared_ptr<void> src(msg, [](void* msg) { CkFreeMsg(msg); });
  unpack(std::move(src), args...);
}

template <typename... Args>
void unpack(std::shared_ptr<void>&& msg, Args&... args) {
  auto s = hypercomm::serdes::make_unpacker(msg, get_msg_buffer(msg.get()));
  hypercomm::pup(s, std::forward_as_tuple(args...));
}

template <typename... Args>
CkMessage* pack(const Args&... _args) {
  auto args = std::forward_as_tuple(const_cast<Args&>(_args)...);
  auto size = hypercomm::size(args);
  auto msg = CkAllocateMarshallMsg(size);
  auto packer = hypercomm::serdes::make_packer(msg->msgBuf);
  hypercomm::pup(packer, args);
  CkAssert(size == packer.size());
  return msg;
}

template<typename A, typename B, std::size_t N>
CkMessage* repack(const A& a, const std::shared_ptr<ergoline::array<B, N>>& array) {
  const auto& src = array->source;
  auto msg = static_cast<CkMarshallMsg*>(src.get());
  if (src.use_count() == 1 && msg &&
      UsrToEnv(msg)->getMsgIdx() == CMessage_CkMarshallMsg::__idx) {
    PUP::toMem p(msg->msgBuf);
    p | const_cast<A&>(a);
    using src_t = std::shared_ptr<void>;
    // a cruel way to take ownership of the pointer
    ::new (const_cast<src_t*>(&src)) src_t();
    return msg;
  } else {
    return pack(a, array);
  }
}
}

#endif
