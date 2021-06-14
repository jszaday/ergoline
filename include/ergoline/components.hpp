#ifndef __ERGOLINE_COMPONENTS_HPP__
#define __ERGOLINE_COMPONENTS_HPP__

#include <hypercomm/core/locality.hpp>

namespace ergoline {

using action_type = std::function<void(hypercomm::component::value_set&&)>;

class component : public hypercomm::component {
  action_type action_;
  std::size_t n_inputs_;

 public:
  component(const std::size_t& _1, const std::size_t& _2, const action_type& _3)
      : hypercomm::component(_1), n_inputs_(_2), action_(_3) {}

  // the component's number of input ports
  virtual std::size_t n_inputs(void) const override { return this->n_inputs_; }

  // the component's number of output ports
  virtual std::size_t n_outputs(void) const override { return 0; }

  // action called when a value set is ready
  virtual value_set action(value_set&& values) override {
    this->action_(std::move(values));

    return {};
  }
};

template <typename Index, typename Fn>
inline const hypercomm::component_ptr& make_component(
    hypercomm::locality_base<Index>& self, const std::size_t& n_inputs,
    const Fn& fn) {
  return self.template emplace_component<ergoline::component>(n_inputs, fn);
}
}

#endif
