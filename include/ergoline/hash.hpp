#ifndef __ERGOLINE_HASH_HPP__
#define __ERGOLINE_HASH_HPP__

#include <deque>
#include <memory>
#include <vector>
#include <functional>
#include <type_traits>
#include <unordered_map>
#include "object.hpp"
#include "ckfutures.h"

namespace ergoline {
template <class, typename Enable = void>
struct hash;

template <typename K, typename V>
using hash_map = std::unordered_map<K, V, hash<K>>;

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
struct hash<T*,
            typename std::enable_if<std::is_base_of<object, T>::value>::type> {
  std::size_t operator()(const T* t) const { return const_cast<T*>(t)->hash(); }
};

template <class T>
struct hash<T,
            typename std::enable_if<std::is_integral<T>::value ||
                                    std::is_floating_point<T>::value ||
                                    std::is_same<std::string, T>::value>::type> {
  std::size_t operator()(const T& t) const { return std::hash<T>()(t); }
};

template <class T>
struct hash<std::shared_ptr<T> > {
  std::size_t operator()(const std::shared_ptr<T>& t) const {
    return hash<T*>()(t.get());
  }
};

template <class T>
struct hash<std::vector<T> > {
  std::size_t operator()(const std::vector<T>& t) const { return hash_iterable(t); }
};

template <class T>
struct hash<std::deque<T> > {
  std::size_t operator()(const std::deque<T>& t) const { return hash_iterable(t); }
};

template <class T>
struct hash<ck::future<T> > {
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
    for (auto i = 0; i < CK_ARRAYINDEX_MAXLEN; i++) {
      hash_combine(seed, (t.data())[i]);
    }
    return 0;
  }
};
}

#endif
