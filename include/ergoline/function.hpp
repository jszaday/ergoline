#ifndef __ERGOLINE_FUNCTION_HPP__
#define __ERGOLINE_FUNCTION_HPP__

#include <string>
#include <memory>
#include <functional>
#include <unordered_map>

#include "charm++.h"

namespace ergoline {

class function_base_: public PUP::able {
public:
  function_base_() { }
  function_base_(CkMigrateMessage *m) : PUP::able(m) { }
  virtual ~function_base_() { }
  PUPable_abstract(function_base_);
};

template <typename Ret, typename... Args>
class function: public function_base_ {
public:
  function() { }
  function(CkMigrateMessage *m) : function_base_(m) { }
  virtual ~function() { }
  virtual Ret operator()(Args... args) = 0;
};

}

#endif
