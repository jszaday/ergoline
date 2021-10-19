#ifndef __ERGOLINE_MAILBOX_HPP__
#define __ERGOLINE_MAILBOX_HPP__

#include <hypercomm/components/mailbox.hpp>

namespace ergoline {

template<typename... Ts>
using compact_t = std::tuple<std::tuple<Ts...>>;

template<typename... Ts>
struct mailbox : public hypercomm::mailbox<compact_t<Ts...>> {
  using parent_t = hypercomm::mailbox<compact_t<Ts...>>;
  using type = typename parent_t::value_type;

  mailbox(const hypercomm::component_id_t& _1) : parent_t(_1) {
    this->activate();
  }
};

}

#endif
