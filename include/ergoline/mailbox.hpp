#ifndef __ERGOLINE_MAILBOX_HPP__
#define __ERGOLINE_MAILBOX_HPP__

#include <hypercomm/components/mailbox.hpp>

namespace ergoline {

template<typename... Ts>
struct compactor_ {
  using type = std::tuple<std::tuple<Ts...>>;
};

template<typename T>
struct compactor_<T> {
  using type = std::tuple<T>;
};

template<typename... Ts>
using compact_t = typename compactor_<Ts...>::type;

template<typename... Ts>
class mailbox : public hypercomm::mailbox<compact_t<Ts...>> {
  using parent_t = hypercomm::mailbox<compact_t<Ts...>>;
 public:
  using type = typename parent_t::value_type;

  mailbox(const hypercomm::component_id_t& _1) : parent_t(_1) {
    this->activate();
  }
};

}

#endif
