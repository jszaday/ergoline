#ifndef __ERGOLINE_FUNCTION_HPP__
#define __ERGOLINE_FUNCTION_HPP__

#include <hypercomm/core/immediate.hpp>

namespace ergoline {

template <typename Ret, typename... Args>
class function: public hypercomm::immediate_action<Ret(Args...)> {
public:
  function() = default;
  function(PUP::reconstruct) {}
  virtual ~function() {}

  virtual Ret action(Args... args) override {
    return (*this)(args...);
  }

  virtual Ret operator()(Args... args) = 0;
};

}

#endif
