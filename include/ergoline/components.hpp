#ifndef __ERGOLINE_COMPONENTS_HPP__
#define __ERGOLINE_COMPONENTS_HPP__

#include <hypercomm/core/locality.hpp>

namespace ergoline {

template<typename T, typename Arg>
using request_fn_t = void(*)(T&, Arg&&);

template<typename T, typename Arg>
class component : public hypercomm::component<T, std::tuple<>> {
  using parent_t = hypercomm::component<T, std::tuple<>>;
 public:
  using in_set_t = typename parent_t::in_set;
  using action_t = request_fn_t<T, Arg>;
 private:
  action_t action_;
  Arg arg_;

 public:
  component(const std::size_t& id_, const action_t& act_, Arg&& arg)
      : parent_t(id_), action_(act_), arg_(std::forward<Arg>(arg)) {}

  // action called when a value set is ready
  virtual std::tuple<> action(in_set_t& set) override {
    this->action_(set, std::move(this->arg_));

    return {};
  }
};

template <typename T, typename Arg, typename BaseIndex, typename Index>
inline hypercomm::comproxy<ergoline::component<T, Arg>> make_component(
    hypercomm::vil<BaseIndex, Index>& self, const request_fn_t<T, Arg>& fn, Arg&& arg) {
  return self.template emplace_component<ergoline::component<T, Arg>>(fn, std::forward<Arg>(arg));
}

}

#endif
