#ifndef __ERGOLINE_FUNCTION_HPP__
#define __ERGOLINE_FUNCTION_HPP__

#include <string>
#include <memory>
#include <functional>
#include <unordered_map>

#include <hypercomm/polymorph.hpp>

namespace ergoline {

class function_base_: public hypercomm::polymorph {
public:
  function_base_() { }
  function_base_(PUP::reconstruct) { }
  virtual ~function_base_() { }
};

template <typename Ret, typename... Args>
class function: public function_base_ {
public:
  function() { }
  function(PUP::reconstruct m) : function_base_(m) { }
  virtual ~function() { }
  virtual Ret operator()(Args... args) = 0;
};

}

#endif
