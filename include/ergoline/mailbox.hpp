#ifndef __ERGOLINE_MAILBOX_HPP__
#define __ERGOLINE_MAILBOX_HPP__

#include <hypercomm/components/mailbox.hpp>

namespace ergoline {

template<typename... Ts>
struct mailbox : public hypercomm::mailbox<std::tuple<Ts...>> {
  using type = std::tuple<Ts...>;

  mailbox(const hypercomm::component::id_t& _1) : hypercomm::mailbox<std::tuple<Ts...>>(_1) {
    this->activate();
  }
};

}

#endif
