#ifndef __ERGOLINE_UTIL_HPP__
#define __ERGOLINE_UTIL_HPP__

#include <chrono>
#include <string>

namespace ergoline {
inline std::string bool_toString(const bool& b) {
  return b ? "true" : "false";
}

inline std::int64_t timestamp() {
  auto curr = std::chrono::system_clock::now();
  return std::chrono::duration_cast<std::chrono::seconds>(curr.time_since_epoch()).count();
}
}

#endif