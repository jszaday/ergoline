#ifndef __ERGOLINE_UTIL_HPP__
#define __ERGOLINE_UTIL_HPP__

#include <chrono>
#include <string>
#include <charm++.h>
#include <hypercomm/core/locality_base.hpp>

#include "hash.hpp"

#define CkAssertNot(b) CkAssert(!b)

namespace detail {
template <int... Is>
struct seq {};

template <int N, int... Is>
struct gen_seq : gen_seq<N - 1, N - 1, Is...> {};

template <int... Is>
struct gen_seq<0, Is...> : seq<Is...> {};

template <typename T, typename F, int... Is>
void for_each(T&& t, F f, seq<Is...>) {
  auto l = {(f(std::get<Is>(t)), 0)...};
}
}

template<typename... Ts>
std::ostream& operator<< (std::ostream& stream, const std::tuple<Ts...>& idx) {
  stream << "(";
  detail::for_each(idx, [&](const int& i) {
    stream << i << ",";
  }, detail::gen_seq<sizeof...(Ts)>());
  return stream << ")";
}

namespace ergoline {

template<typename T>
using extricate_t = typename std::conditional<hypercomm::is_specialization_of<std::shared_ptr, T>::value, typename T::element_type, T>::type;

inline hypercomm::future make_future(const std::shared_ptr<hypercomm::proxy>& proxy) {
  auto* chare = static_cast<Chare*>(proxy->local());
  auto* manager = dynamic_cast<hypercomm::future_manager_*>(chare);
  CkAssert(manager && "unable to retrieve local chare");
  return manager->make_future();
}

inline std::int64_t timestamp() {
  auto curr = std::chrono::system_clock::now();
  return std::chrono::duration_cast<std::chrono::seconds>(curr.time_since_epoch()).count();
}

namespace {
class generic_singleton {};

template <typename T>
struct typed_singleton : public generic_singleton {
  std::shared_ptr<T> instance;

  typed_singleton(void) : instance(new T()) {}
};

using singleton_map_type =
    std::map<std::type_index, std::unique_ptr<generic_singleton>>;

CkpvDeclare(singleton_map_type, singleton_map_);
}

template <typename T>
const std::shared_ptr<T>& access_singleton(void) {
  if (!CkpvInitialized(singleton_map_)) {
    CkpvInitialize(singleton_map_type, singleton_map_);
  }
  std::type_index typeIdx(typeid(T));
  auto& ptr = CkpvAccess(singleton_map_)[typeIdx];
  typed_singleton<T>* record = nullptr;
  if (!ptr) {
    record = new typed_singleton<T>();

    ptr.reset(record);
  } else {
    record = static_cast<decltype(record)>(ptr.get());
  }
  return record->instance;
}

}

#endif
