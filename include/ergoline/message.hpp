#ifndef __ERGOLINE_MESSAGE_HPP__
#define __ERGOLINE_MESSAGE_HPP__

#include <hypercomm/utilities.hpp>

#include "array.hpp"

namespace ergoline {
template <typename A, typename B, std::size_t N>
CkMessage* repack(const A& a,
                  const std::shared_ptr<ergoline::array<B, N>>& array) {
#if CMK_ERROR_CHECKING
  auto n_uses = array->source.use_count();
  if (n_uses != 1) {
    CkAbort("fatal> source of %p has %lu users, expected 1!\n", array.get(), n_uses);
  }
#endif

  auto msg = (CkMessage*)hypercomm::utilities::unwrap_message(std::move(array->source));
  auto buf = hypercomm::utilities::get_message_buffer(msg);

  PUP::toMem p(buf);
  p | const_cast<A&>(a);

  return msg;
}
}

#endif
