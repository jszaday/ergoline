#ifndef __ERGOLINE_HASH_HPP__
#define __ERGOLINE_HASH_HPP__

#include <deque>
#include <memory>
#include <vector>
#include <functional>
#include <type_traits>
#include <unordered_map>
#include "charm++.h"

namespace ergoline {

struct hashable {
  virtual std::size_t hash() = 0;
};

namespace hash_utils {

template <bool B>
using Requires = PUP::Requires<B>;

template <class, typename Enable = void>
struct hash;

template <class T>
inline void hash_combine(std::size_t& seed, const T& v) {
  hash<T> hasher;
  seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

template <class T>
inline std::size_t hash_iterable(const T& t) {
  std::size_t seed = 0;
  for (auto i : t) {
    hash_combine(seed, i);
  }
  return seed;
}

template <class T>
struct hash<T,
            typename std::enable_if<std::is_base_of<hashable, T>::value>::type> {
  std::size_t operator()(const T& t) const { return const_cast<T&>(t).hash(); }
};

template <class T>
struct hash<T*,
            typename std::enable_if<std::is_base_of<hashable, T>::value>::type> {
  std::size_t operator()(const T* t) const { return const_cast<T*>(t)->hash(); }
};

template <class T>
struct hash<
    T, typename std::enable_if<std::is_integral<T>::value ||
                               std::is_floating_point<T>::value ||
                               std::is_same<std::string, T>::value>::type> {
  std::size_t operator()(const T& t) const { return std::hash<T>()(t); }
};

template <class T>
struct hash<std::shared_ptr<T>> {
  std::size_t operator()(const std::shared_ptr<T>& t) const {
    return hash<T*>()(t.get());
  }
};

template <class T>
struct hash<std::vector<T>> {
  std::size_t operator()(const std::vector<T>& t) const {
    return hash_iterable(t);
  }
};

template <class T>
struct hash<std::deque<T>> {
  std::size_t operator()(const std::deque<T>& t) const {
    return hash_iterable(t);
  }
};

template <class T>
struct hash<ck::future<T>> {
  std::size_t operator()(const ck::future<T>& t) const {
    // ADD WHEN PR#3203 IS MERGED
    // auto handle = t.handle();
    // std::size_t seed = std::hash<CkFutureID>()(handle.id);
    // hash_combine(seed, handle.pe);
    // return seed;
    return 0;
  }
};

template <>
struct hash<CkChareID> {
  std::size_t operator()(const CkChareID& t) const {
    std::size_t seed = std::hash<void*>()(t.objPtr);
    hash_combine(seed, t.onPE);
    return seed;
  }
};

template <>
struct hash<CkGroupID> {
  std::size_t operator()(const CkGroupID& t) const {
    return std::hash<int>()(t.idx);
  }
};

template <>
struct hash<CkArrayIndex> {
  std::size_t operator()(const CkArrayIndex& t) const {
    std::size_t seed = 0;
    const int *idx = t.data();
    for (auto i = 0; i < CK_ARRAYINDEX_MAXLEN; i++) {
      hash_combine(seed, idx[i]);
    }
    return seed;
  }
};

template <typename T>
struct hash<T, typename std::enable_if<std::is_base_of<
                   CProxyElement_ArrayElement, T>::value>::type> {
  std::size_t operator()(const T& t) const {
    std::size_t seed = 0;
    hash_combine(seed, (CkGroupID)t.ckGetArrayID());
    hash_combine(seed, t.ckGetIndex());
    return seed;
  }
};

template <typename T>
struct hash<T, typename std::enable_if<
                   std::is_base_of<CProxy_ArrayElement, T>::value>::type> {
  std::size_t operator()(const T& t) const {
    return hash<CkGroupID>()((CkGroupID)t.ckGetArrayID());
  }
};

template <typename T>
struct hash<
    T, typename std::enable_if<std::is_base_of<CProxy_Chare, T>::value>::type> {
  std::size_t operator()(const T& t) const {
    return hash<CkChareID>()(t.ckGetChareID());
  }
};

template <typename T>
struct hash<T, typename std::enable_if<
                   std::is_base_of<CProxyElement_Group, T>::value ||
                   std::is_base_of<CProxyElement_NodeGroup, T>::value>::type> {
  std::size_t operator()(const T& t) const {
    std::size_t seed = 0;
    hash_combine(seed, t.ckGetGroupID());
    hash_combine(seed, t.ckGetGroupPe());
    return seed;
  }
};

template <typename T>
struct hash<T,
            typename std::enable_if<
                (std::is_base_of<CProxy_Group, T>::value ||
                 std::is_base_of<CProxy_NodeGroup, T>::value) &&
                !(std::is_base_of<CProxyElement_Group, T>::value ||
                  std::is_base_of<CProxyElement_NodeGroup, T>::value)>::type> {
  std::size_t operator()(const T& t) const {
    return hash<CkGroupID>()(t.ckGetGroupID());
  }
};

template <size_t N = 0, typename... Args,
          Requires<0 == sizeof...(Args)> = nullptr>
void hash_tuple_impl(std::size_t& /* p */, const std::tuple<Args...>& /* t */) {}

template <size_t N = 0, typename... Args,
          Requires<(0 < sizeof...(Args) && 0 == N)> = nullptr>
void hash_tuple_impl(std::size_t& p, const std::tuple<Args...>& t) {
  hash_combine(p, std::get<N>(t));
}

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N > 0)> = nullptr>
void hash_tuple_impl(std::size_t& p, const std::tuple<Args...>& t) {
  hash_combine(p, std::get<N>(t));
  hash_tuple_impl<N - 1>(p, t);
}

template <typename... Args>
struct hash<std::tuple<Args...>> {
  std::size_t operator()(const std::tuple<Args...>& t) const {
    size_t seed = 0;
    hash_tuple_impl<sizeof...(Args)-1>(seed, t);
    return seed;
  }
};
}

template <typename K, typename V>
using hash_map = std::unordered_map<K, V, hash_utils::hash<K>>;

template <typename K, typename V>
bool map_contains(const hash_map<K, V>& map, const K& k) {
  return map.find(k) != map.end();
}

template <typename K, typename V>
bool map_remove(hash_map<K, V>& map, const K& k) {
  auto search = map.find(k);
  if (search != map.end()) {
    map.erase(search);
    return true;
  } else {
    return false;
  }
}

struct hasher {
  static const std::size_t default_seed = 0;

  hasher(PUP::reconstruct) {}
  hasher(size_t seed = default_seed) : hash_(seed) {}
  hasher(const hasher& other) : hash_(other.hash_) {}

  template <typename T>
  inline void operator|(const T& t) {
    hash_utils::hash_combine(hash_, t);
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

#endif