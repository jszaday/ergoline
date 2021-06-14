#ifndef __ERGOLINE_MESSAGE_HPP__
#define __ERGOLINE_MESSAGE_HPP__

#include <hypercomm/utilities.hpp>

#include "array.hpp"

namespace ergoline {
template <typename A, typename B, std::size_t N>
CkMessage* repack(const A& a,
                  const std::shared_ptr<ergoline::array<B, N>>& array) {
  auto src = std::static_pointer_cast<CkMessage>(std::move(array->source));

  CkAssert(src.use_count() == 1 && "msg replicated");

  auto msg = hypercomm::utilities::unwrap_message(std::move(src));
  auto buf = hypercomm::utilities::get_message_buffer(msg);

  PUP::toMem p(msg);
  p | const_cast<A&>(a);

  return msg;
}
}

#endif
