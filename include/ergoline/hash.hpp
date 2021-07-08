#ifndef __ERGOLINE_HASH_HPP__
#define __ERGOLINE_HASH_HPP__

#include <hypercomm/utilities/hash.hpp>
#include <unordered_map>

#include "object.hpp"

namespace ergoline {

template <class T, typename Enable = void>
struct equal_to {
  inline bool operator()(const T &lhs, const T &rhs) const {
    return lhs == rhs;
  }
};

template <class T>
struct equal_to<std::shared_ptr<T>,
                typename std::enable_if<std::is_base_of<hypercomm::comparable, T>::value>::type> {
  inline bool operator()(const std::shared_ptr<T> &lhs, const std::shared_ptr<T> &rhs) const {
    return (!lhs || !rhs) ? (lhs == rhs) : (lhs->equals(rhs));
  }
};

template <typename K, typename V>
using hash_map = std::unordered_map<K, V, hypercomm::utilities::hash<K>, equal_to<K>>;

template <typename K, typename V>
inline bool map_contains(const hash_map<K, V>& map, const K& k) {
  return map.find(k) != map.end();
}

template <typename K, typename V>
inline bool map_contains(const std::shared_ptr<hash_map<K, V>>& map, const K& k) {
  return map_contains(*map, k);
}

template <typename K, typename V>
inline bool map_remove(hash_map<K, V>& map, const K& k) {
  auto search = map.find(k);
  if (search != map.end()) {
    map.erase(search);
    return true;
  } else {
    return false;
  }
}

template <typename K, typename V>
inline bool map_remove(const std::shared_ptr<hash_map<K, V>>& map, const K& k) {
  return map_remove(*map, k);
}

struct hasher {
  static const std::size_t default_seed = 0;

  hasher(PUP::reconstruct) {}
  hasher(size_t seed = default_seed) : hash_(seed) {}
  hasher(const hasher& other) : hash_(other.hash_) {}

  template <typename T>
  inline void operator|(const T& t) {
    hash_ = hypercomm::hash_combine(hash_, hypercomm::utilities::hash<T>()(t));
  }

  inline std::size_t hash() const { return hash_; }
  inline void reset(size_t seed = default_seed) { hash_ = seed; }
  void pup(PUP::er& p) { p | hash_; }

 private:
  size_t hash_;
};

template <typename... T>
std::size_t hash(const T&... args) {
  using expander = int[];
  hasher h;
  (void)expander{0, (void(h | args), 0)...};
  return h.hash();
}
}

namespace hypercomm {
template <typename K, typename V>
struct puper<ergoline::hash_map<K, V>> {
  inline static void impl(serdes& s, ergoline::hash_map<K, V>& t) {
    if (s.unpacking()) {
      std::size_t size;
      s.copy(&size);
      ::new (&t) ergoline::hash_map<K, V>(size);
      for (auto i = 0; i < size; i++) {
        std::tuple<K, V> pair;
        pup(s, pair);
        t[std::get<0>(pair)] = std::get<1>(pair);
      }
    } else {
      auto size = t.size();
      s.copy(&size);
      for (auto& pair : t) {
        pup(s, pair.first);
        pup(s, pair.second);
      }
    }
  }
};
}

#endif
