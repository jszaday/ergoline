#ifndef __ERGOLINE_MESSAGE_HPP__
#define __ERGOLINE_MESSAGE_HPP__

#include <map>
#include <string>
#include <memory>

#include "array.hpp"

#include <charm++.h>
#include <NDMeshStreamer.h>

namespace ergoline {

namespace {
template <bool B>
using Requires = PUP::Requires<B>;

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

template <typename T>
inline bool is_bytes() {
  return PUP::as_bytes<T>::value;
}

template <typename T>
using is_pupable_t = typename std::is_base_of<PUP::able, T>;

template <typename T>
void unpack(const std::shared_ptr<void>& msg, char*& curr, T& t);

template <typename T, Requires<is_pupable_t<T>::value> = nullptr>
void unpack(const std::shared_ptr<void>& msg, char*& curr,
            std::shared_ptr<T>& t) {
  PUP::able* p = nullptr;
  unpack(msg, curr, p);
  t = std::dynamic_pointer_cast<T>(std::shared_ptr<PUP::able>(p));
}

template <typename T, Requires<!is_pupable_t<T>::value> = nullptr>
void unpack(const std::shared_ptr<void>& msg, char*& curr,
            std::shared_ptr<T>& t) {
  const auto& is_nullptr = *reinterpret_cast<bool*>(curr);
  curr += sizeof(bool);
  if (is_nullptr) {
    t.reset();
  } else if (is_bytes<T>()) {
    t = std::shared_ptr<T>(msg, reinterpret_cast<T*>(curr));
    curr += sizeof(T);
  } else {
    auto p = static_cast<T*>(malloc(sizeof(T)));
    reconstruct(p);
    unpack(msg, curr, *p);
    t = std::shared_ptr<T>(p, [](T* p) { free(p); });
  }
}

namespace {
template <size_t N, typename... Args,
          Requires<(sizeof...(Args) == 0)> = nullptr>
inline void unpack_tuple_impl(const std::shared_ptr<void>&, char*&,
                              std::tuple<Args...>&) {}

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N == 0)> = nullptr>
inline void unpack_tuple_impl(const std::shared_ptr<void>& msg, char*& curr,
                              std::tuple<Args...>& t) {
  unpack(msg, curr, std::get<N>(t));
}

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N > 0)> = nullptr>
inline void unpack_tuple_impl(const std::shared_ptr<void>& msg, char*& curr,
                              std::tuple<Args...>& t) {
  unpack(msg, curr, std::get<N>(t));
  unpack_tuple_impl<N - 1>(msg, curr, t);
}
}

template <typename... Ts>
void unpack(const std::shared_ptr<void>& msg, char*& curr,
            const std::tuple<Ts...>& t) {
  unpack_tuple_impl<sizeof...(Ts)-1>(msg, curr,
                                     const_cast<std::tuple<Ts...>&>(t));
}

template <typename T, std::size_t N>
void unpack(const std::shared_ptr<void>& msg, char*& curr, array<T, N>& t) {
  unpack(msg, curr, t.shape);
  if (is_bytes<T>()) {
    t.source = msg;
    t.buffer = reinterpret_cast<T*>(curr);
  } else {
    t.alloc(true, true);
    for (auto& i : t) {
      unpack(msg, curr, i);
    }
  }
}

template <typename T>
void unpack(const std::shared_ptr<void>& msg, char*& curr, T& t) {
  PUP::fromMem p(curr);
  p | t;
  curr += p.size();
}

template <typename... Args>
void unpack(void* msg, Args&... args) {
  auto source = std::shared_ptr<void>(msg, [](void* msg) { CkFreeMsg(msg); });
  auto buffer = get_msg_buffer(msg);
  unpack(source, buffer, std::forward_as_tuple(args...));
}

template <typename... Args>
void* pack(const Args&... _args) {
  auto args = std::forward_as_tuple(const_cast<Args&>(_args)...);
  auto size = PUP::size(args);
  auto msg = CkAllocateMarshallMsg(size);
  PUP::toMemBuf(args, msg->msgBuf, size);
  return msg;
}
}

#endif
