#ifndef __ERGOLINE_UTIL_HPP__
#define __ERGOLINE_UTIL_HPP__

#include <chrono>
#include <string>
#include <charm++.h>
#include <hypercomm/core/locality_base.hpp>

#include "hash.hpp"

std::ostream& operator<< (std::ostream& stream, const std::tuple<int, int>& idx) {
  return stream << "(" << std::get<0>(idx) << ", " << std::get<1>(idx) << ")";
}

namespace ergoline {

inline hypercomm::future make_future(const std::shared_ptr<hypercomm::proxy>& proxy) {
  auto* chare = static_cast<Chare*>(proxy->local());
  auto* manager = dynamic_cast<hypercomm::future_manager_*>(chare);
  CkAssert(manager && "unable to retrieve local chare");
  return manager->make_future();
}

inline std::string bool_toString(const bool& b) {
  return b ? "true" : "false";
}

inline std::int64_t timestamp() {
  auto curr = std::chrono::system_clock::now();
  return std::chrono::duration_cast<std::chrono::seconds>(curr.time_since_epoch()).count();
}
}

#endif
