#ifndef __HYPERCOMM_PUP_HPP__
#define __HYPERCOMM_PUP_HPP__

#include <ergoline/array.hpp>
#include <hypercomm/serdes.hpp>
#include <hypercomm/traits.hpp>

namespace hypercomm {

using namespace ergoline;

template <typename T>
inline void pup(serdes& s, T& t);

template <typename... Ts>
inline void pup(serdes& s, const std::tuple<Ts...>& t);

template <typename T>
inline size_t size(const T& t) {
  auto s = serdes::make_sizer();
  pup(s, const_cast<T&>(t));
  return s.size();
}

template <typename T>
void interpup(PUP::er& p, T& t) {
  if (typeid(p) == typeid(PUP::fromMem)) {
    auto mem = static_cast<PUP::fromMem&>(p);
    auto s = serdes::make_unpacker(nullptr, mem.get_current_pointer());
    pup(s, t);
    mem.advance(s.size());
  } else if (typeid(p) == typeid(PUP::toMem)) {
    auto mem = static_cast<PUP::toMem&>(p);
    auto s = serdes::make_packer(mem.get_current_pointer());
    pup(s, t);
    mem.advance(s.size());
  } else if (typeid(p) == typeid(PUP::sizer)) {
    p(static_cast<char*>(nullptr), size(t));
  } else {
    CkAbort("unsure how to convert an %s into a serdes", typeid(p).name());
  }
}

template <typename T, typename Enable = void>
struct puper;

template <typename T>
struct puper<T, typename std::enable_if<PUP::as_bytes<T>::value>::type> {
  inline static void impl(serdes& s, T& t) { s.copy(&t); }
};

template <typename T, std::size_t N>
struct puper<std::array<T, N>,
             typename std::enable_if<PUP::as_bytes<T>::value>::type> {
  inline static void impl(serdes& s, std::array<T, N>& t) {
    s.copy(t.data(), N);
  }
};

template <typename T>
struct puper<T, typename std::enable_if<hypercomm::built_in<T>::value>::type> {
  inline static void impl(serdes& s, T& t) {
    switch (s.state) {
      case serdes::state_t::UNPACKING: {
        PUP::fromMem p(s.current);
        ergoline::reconstruct(&t);
        p | t;
        s.advance(p.size());
        break;
      }
      case serdes::state_t::PACKING: {
        PUP::toMem p(s.current);
        p | t;
        s.advance(p.size());
        break;
      }
      case serdes::state_t::SIZING:
        s.advance(PUP::size(t));
        break;
    }
  }
};

template <typename T>
struct puper<std::shared_ptr<T>,
             typename std::enable_if<hypercomm::is_pupable<T>::value>::type> {
  inline static void impl(serdes& s, std::shared_ptr<T>& t) {
    if (s.unpacking()) {
      PUP::able* p = nullptr;
      pup<PUP::able*>(s, p);
      ::new (&t) std::shared_ptr<T>(
          std::dynamic_pointer_cast<T>(std::shared_ptr<PUP::able>(p)));
    } else {
      auto p = dynamic_cast<PUP::able*>(t.get());
      pup<PUP::able*>(s, p);
    }
  }
};

template <typename T>
struct puper<std::shared_ptr<T>,
             typename std::enable_if<!hypercomm::is_pupable<T>::value>::type> {
  inline static void unpack(serdes& s, std::shared_ptr<T>& t) {
    const auto& is_nullptr = *reinterpret_cast<bool*>(s.current);
    s.advance<bool>();
    if (is_nullptr) {
      ::new (&t) std::shared_ptr<T>();
    } else if (is_bytes<T>()) {
      ::new (&t) std::shared_ptr<T>(s.source, reinterpret_cast<T*>(s.current));
      s.advance<T>();
    } else {
      auto p = static_cast<T*>(malloc(sizeof(T)));
      ergoline::reconstruct(p);
      pup(s, *p);
      ::new (&t) std::shared_ptr<T>(p, [](T* p) { free(p); });
    }
  }

  inline static void impl(serdes& s, std::shared_ptr<T>& t) {
    if (s.unpacking()) {
      unpack(s, t);
    } else {
      auto is_nullptr = nullptr == t;
      pup(s, is_nullptr);
      if (!is_nullptr) {
        pup(s, *t);
      }
    }
  }
};

namespace {
template <bool B>
using Requires = PUP::Requires<B>;

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) == 0)> = nullptr>
inline void pup_tuple_impl(const std::shared_ptr<void>&, char*&,
                           std::tuple<Args...>&) {}

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N == 0)> = nullptr>
inline void pup_tuple_impl(serdes& s, std::tuple<Args...>& t) {
  pup(s, std::get<N>(t));
}

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N > 0)> = nullptr>
inline void pup_tuple_impl(serdes& s, std::tuple<Args...>& t) {
  pup(s, std::get<N>(t));
  pup_tuple_impl<N - 1>(s, t);
}
}

template <typename... Ts>
struct puper<std::tuple<Ts...>> {
  inline static void impl(serdes& s, std::tuple<Ts...>& t) {
    pup_tuple_impl<sizeof...(Ts)-1>(s, t);
  }
};

template <typename T, std::size_t N>
struct puper<ergoline::array<T, N>> {
  inline static void impl(serdes& s, ergoline::array<T, N>& t) {
    pup(s, t.shape);

    if (s.unpacking()) {
      if (PUP::as_bytes<T>::value) {
        t.source = s.source;
        t.buffer = reinterpret_cast<T*>(s.current);
      } else {
        t.alloc(true, true);
        for (auto& i : t) {
          pup(s, i);
        }
      }
    } else if (PUP::as_bytes<T>::value) {
      s.copy(t.buffer, t.size());
    } else {
      for (auto& i : t) {
        pup(s, i);
      }
    }
  }
};

template <typename T>
inline void pup(serdes& s, T& t) {
  puper<T>::impl(s, t);
}

template <typename... Ts>
inline void pup(serdes& s, const std::tuple<Ts...>& t) {
  puper<std::tuple<Ts...>>::impl(s, const_cast<std::tuple<Ts...>&>(t));
}
}

#endif
