#ifndef __ERGOLINE_CALLBACK_HPP__
#define __ERGOLINE_CALLBACK_HPP__

#include <hypercomm/core/immediate.hpp>

namespace ergoline {

template<typename Ret, typename... Args>
class immediate;

template<typename Ret, typename... Args>
struct immediate<Ret(Args...)> : public hypercomm::immediate_action<Ret(Args...)> {
  using action_type = std::function<Ret(Args...)>;
  action_type action_;

  immediate(const action_type& _1): action_(_1) {}

  virtual Ret action(Args... args) override {
    return this->action_(args...);
  }

  virtual void __pup__(hypercomm::serdes& s) override {
    CkAbort("not yet implemented!");
  }
};

template<typename Ret, typename... Args>
std::shared_ptr<hypercomm::immediate_action<Ret(Args...)>> wrap_lambda(const typename immediate<Ret(Args...)>::action_type& action) {
  return std::make_shared<immediate<Ret(Args...)>>(action);
}

}

#endif
