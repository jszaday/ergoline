#ifndef __ERGOLINE_UTIL_HPP__
#define __ERGOLINE_UTIL_HPP__

#include <charm++.h>

#include <chrono>
#include <string>
#include <iomanip>

namespace ergoline {
inline std::string bool_toString(const bool& b) {
  return b ? "true" : "false";
}

inline std::int64_t timestamp() {
  auto curr = std::chrono::system_clock::now();
  return std::chrono::duration_cast<std::chrono::seconds>(curr.time_since_epoch()).count();
}

std::string buf2str(const char* data, const std::size_t& size) {
  std::stringstream ss;
  ss << "[ ";
  for (auto i = 0; i < size; i++) {
    ss << std::hex << std::uppercase << std::setfill('0') << std::setw(2) << (unsigned short)(0xFF & data[i]) << " ";
  }
  ss << "]";
  return ss.str();
}

std::string env2str(const envelope* env) {
  auto* bytes = reinterpret_cast<const char*>(env);
  std::stringstream ss;
  ss << buf2str(bytes, sizeof(envelope)) << "|";
  ss << buf2str(bytes + sizeof(envelope), env->getTotalsize() - sizeof(envelope));
  return ss.str();
}
}

#endif
