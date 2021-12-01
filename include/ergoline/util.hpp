#ifndef __ERGOLINE_UTIL_HPP__
#define __ERGOLINE_UTIL_HPP__

#include <chrono>
#include <string>
#include <charm++.h>

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

template<typename T, typename Enable = void>
struct extractor_ {
  using type = T;
};

template<typename T>
struct extractor_<std::shared_ptr<T>> {
  using type = T;
};

template<typename T>
struct extractor_<hypercomm::typed_value_ptr<T>> {
  using type = T;
};

template<typename T>
using extricate_t = typename extractor_<T>::type;

inline hypercomm::future make_future(const std::shared_ptr<hypercomm::proxy>& proxy) {
  auto* chare = static_cast<ArrayElement*>(proxy->local());
  auto* manager = static_cast<hypercomm::generic_locality_*>(chare);
  CkAssert(manager && "unable to retrieve local chare");
  return manager->make_future();
}

inline std::int64_t timestamp() {
  auto curr = std::chrono::system_clock::now();
  return std::chrono::duration_cast<std::chrono::seconds>(curr.time_since_epoch()).count();
}

struct link {
  link* prev;

  link(link* prev_) : prev(prev_) {}

 protected:
  ~link() = default;
};

struct sentinel_link : public link {
  std::int64_t count;
  bool active;

  sentinel_link(link* prev) : link(prev), count(0), active(false) {}

  inline void produce(void) {
    this->active = true;
    this->count++;
  }

  inline void consume(void) { this->count--; }

  inline bool deactivate(void) {
    this->active = false;
    return (this->count == 0);
  }

  inline bool complete(void) { return !(this->active) && (this->count == 0); }
};

struct speculator_link : public link {
  std::vector<hypercomm::component_id_t> ids;

  speculator_link(link* prev, std::size_t count) : link(prev), ids(count) {}
};
}

#endif
