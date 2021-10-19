#ifndef __ERGOLINE_COMPONENTS_HPP__
#define __ERGOLINE_COMPONENTS_HPP__

#include <hypercomm/core/locality.hpp>

namespace ergoline {

template<typename T>
class component : public hypercomm::component<T, std::tuple<>> {
  using parent_t = hypercomm::component<T, std::tuple<>>;
 public:
  using in_set_t = typename parent_t::in_set;
  using action_t = std::function<void(in_set_t&)>;
 private:
  action_t action_;
 public:
  component(const std::size_t& id_, const std::size_t& n_inputs_, const action_t& act_)
      : parent_t(id_), action_(act_) {
        CkAssert(n_inputs_ == std::tuple_size<in_set_t>::value);
      }

  // action called when a value set is ready
  virtual std::tuple<> action(in_set_t& set) override {
    this->action_(set);

    return {};
  }
};

template <typename T, typename BaseIndex, typename Index, typename Fn>
inline hypercomm::comproxy<ergoline::component<T>> make_component(
    hypercomm::vil<BaseIndex, Index>& self, const std::size_t& n_inputs,
    const Fn& fn) {
  return self.template emplace_component<ergoline::component<T>>(n_inputs, fn);
}
}

#endif
