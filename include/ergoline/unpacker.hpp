#ifndef __ERGOLINE_UNPACKER_HPP__
#define __ERGOLINE_UNPACKER_HPP__

#include "array.hpp"

namespace ergoline {

template <typename T, typename Enable = void>
struct unpacker {
  inline static void impl(const std::shared_ptr<void>& msg, char*& curr, T& t) {
    PUP::fromMem p(curr);
    p | t;
    curr += p.size();
  }
};

template <typename T>
inline void unpack(const std::shared_ptr<void>& msg, char*& curr, T& t) {
  unpacker<T>::impl(msg, curr, t);
}

template <typename... Ts>
inline void unpack(const std::shared_ptr<void>& msg, char*& curr,
                   const std::tuple<Ts...>& t) {
  unpacker<std::tuple<Ts...>>::impl(msg, curr,
                                    const_cast<std::tuple<Ts...>&>(t));
}

template <typename T>
struct unpacker<std::shared_ptr<T>,
                typename std::enable_if<is_pupable_t<T>::value>::type> {
  inline static void impl(const std::shared_ptr<void>& msg, char*& curr,
                          std::shared_ptr<T>& t) {
    PUP::able* p = nullptr;
    unpack<PUP::able*>(msg, curr, p);
    t = std::dynamic_pointer_cast<T>(std::shared_ptr<PUP::able>(p));
  }
};

template <typename T>
struct unpacker<std::shared_ptr<T>,
                typename std::enable_if<!is_pupable_t<T>::value>::type> {
  inline static void impl(const std::shared_ptr<void>& msg, char*& curr,
                          std::shared_ptr<T>& t) {
    const auto& is_nullptr = *reinterpret_cast<bool*>(curr);
    curr += sizeof(bool);
    if (is_nullptr) {
      t.reset();
    } else if (is_bytes<T>()) {
      t = std::shared_ptr<T>(msg, reinterpret_cast<T*>(curr));
      curr += sizeof(T);
    } else {
      auto p = static_cast<T*>(malloc(sizeof(T)));
      reconstruct(p);
      unpack(msg, curr, *p);
      t = std::shared_ptr<T>(p, [](T* p) { free(p); });
    }
  }
};

namespace {
template <bool B>
using Requires = PUP::Requires<B>;

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) == 0)> = nullptr>
inline void unpack_tuple_impl(const std::shared_ptr<void>&, char*&,
                              std::tuple<Args...>&) {}

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N == 0)> = nullptr>
inline void unpack_tuple_impl(const std::shared_ptr<void>& msg, char*& curr,
                              std::tuple<Args...>& t) {
  unpack(msg, curr, std::get<N>(t));
}

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N > 0)> = nullptr>
inline void unpack_tuple_impl(const std::shared_ptr<void>& msg, char*& curr,
                              std::tuple<Args...>& t) {
  unpack(msg, curr, std::get<N>(t));
  unpack_tuple_impl<N - 1>(msg, curr, t);
}
}

template <typename... Ts>
struct unpacker<std::tuple<Ts...>> {
  inline static void impl(const std::shared_ptr<void>& msg, char*& curr,
                          std::tuple<Ts...>& t) {
    unpack_tuple_impl<sizeof...(Ts)-1>(msg, curr, t);
  }
};

template <typename T, std::size_t N>
struct unpacker<array<T, N>> {
  inline static void impl(const std::shared_ptr<void>& msg, char*& curr,
                          array<T, N>& t) {
    unpack(msg, curr, t.shape);
    if (is_bytes<T>()) {
      t.source = msg;
      t.buffer = reinterpret_cast<T*>(curr);
    } else {
      t.alloc(true, true);
      for (auto& i : t) {
        unpack(msg, curr, i);
      }
    }
  }
};
}

#endif
