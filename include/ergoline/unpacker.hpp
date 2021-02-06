#ifndef __ERGOLINE_UNPACKER_HPP__
#define __ERGOLINE_UNPACKER_HPP__

#include "array.hpp"

#include <hypercomm/serdes.hpp>
#include <hypercomm/traits.hpp>

namespace ergoline {

using namespace hypercomm;

template <typename T>
inline void unpack(serdes& s, T& t);

template <typename... Ts>
inline void unpack(serdes& s, const std::tuple<Ts...>& t);

template <typename T, typename Enable = void>
struct unpacker;

template <typename T>
struct unpacker<T, typename std::enable_if<PUP::as_bytes<T>::value>::type> {
  inline static void impl(serdes& s, T& t) {
    t = *reinterpret_cast<T*>(s.current);
    s.advance<T>();
  }
};

template <typename T>
struct unpacker<T,
                typename std::enable_if<hypercomm::built_in<T>::value>::type> {
  inline static void impl(serdes& s, T& t) {
    PUP::fromMem p(s.current);
    reconstruct(&t);
    p | t;
    s.advance(p.size());
  }
};

template <typename T>
struct unpacker<
    std::shared_ptr<T>,
    typename std::enable_if<hypercomm::is_pupable<T>::value>::type> {
  inline static void impl(serdes& s, std::shared_ptr<T>& t) {
    PUP::able* p = nullptr;
    unpack<PUP::able*>(s, p);
    ::new (&t) std::shared_ptr<T>(
        std::dynamic_pointer_cast<T>(std::shared_ptr<PUP::able>(p)));
  }
};

template <typename T>
struct unpacker<
    std::shared_ptr<T>,
    typename std::enable_if<!hypercomm::is_pupable<T>::value>::type> {
  inline static void impl(serdes& s, std::shared_ptr<T>& t) {
    const auto& is_nullptr = *reinterpret_cast<bool*>(s.current);
    s.advance<bool>();
    if (is_nullptr) {
      ::new (&t) std::shared_ptr<T>();
    } else if (is_bytes<T>()) {
      ::new (&t) std::shared_ptr<T>(s.source, reinterpret_cast<T*>(s.current));
      s.advance<T>();
    } else {
      auto p = static_cast<T*>(malloc(sizeof(T)));
      reconstruct(p);
      unpack(s, *p);
      ::new (&t) std::shared_ptr<T>(p, [](T* p) { free(p); });
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
inline void unpack_tuple_impl(serdes& s, std::tuple<Args...>& t) {
  unpack(s, std::get<N>(t));
}

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N > 0)> = nullptr>
inline void unpack_tuple_impl(serdes& s, std::tuple<Args...>& t) {
  unpack(s, std::get<N>(t));
  unpack_tuple_impl<N - 1>(s, t);
}
}

template <typename... Ts>
struct unpacker<std::tuple<Ts...>> {
  inline static void impl(serdes& s, std::tuple<Ts...>& t) {
    unpack_tuple_impl<sizeof...(Ts)-1>(s, t);
  }
};

template <typename T, std::size_t N>
struct unpacker<ergoline::array<T, N>> {
  inline static void impl(serdes& s, ergoline::array<T, N>& t) {
    unpack(s, t.shape);
    if (is_bytes<T>()) {
      t.source = s.source;
      t.buffer = reinterpret_cast<T*>(s.current);
    } else {
      t.alloc(true, true);
      for (auto& i : t) {
        unpack(s, i);
      }
    }
  }
};

template <typename T, std::size_t N>
struct unpacker<std::array<T, N>,
                typename std::enable_if<PUP::as_bytes<T>::value>::type> {
  inline static void impl(serdes& s, std::array<T, N>& t) {
    auto start = reinterpret_cast<T*>(s.current);
    std::copy(start, start + N, std::begin(t));
    s.advance<T>(N);
  }
};

template <typename T>
inline void unpack(serdes& s, T& t) {
  unpacker<T>::impl(s, t);
}

template <typename... Ts>
inline void unpack(serdes& s, const std::tuple<Ts...>& t) {
  unpacker<std::tuple<Ts...>>::impl(s, const_cast<std::tuple<Ts...>&>(t));
}
}

#endif
