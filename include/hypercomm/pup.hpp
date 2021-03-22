#ifndef __HYPERCOMM_PUP_HPP__
#define __HYPERCOMM_PUP_HPP__

#include <hypercomm/serdes.hpp>
#include <hypercomm/traits.hpp>

#include <ergoline/hash.hpp>
#include <ergoline/array.hpp>

namespace hypercomm {

using namespace ergoline;

template <typename T>
inline void pup(serdes& s, const T& t);

template <typename T>
inline void pup(serdes& s, T& t);

template <typename... Ts>
inline void pup(serdes& s, const std::tuple<Ts...>& t);

template <typename T>
inline serdes& operator|(serdes& s, T& t) {
  pup(s, t);
  return s;
}

template <typename T>
inline size_t size(const T& t) {
  auto s = serdes::make_sizer();
  pup(s, const_cast<T&>(t));
  return s.size();
}

template <typename T>
void interpup(PUP::er& p, T& t) {
  if (typeid(p) == typeid(PUP::fromMem)) {
    auto& mem = *static_cast<PUP::fromMem*>(&p);
    auto s = serdes::make_unpacker(nullptr, mem.get_current_pointer());
    pup(s, t);
    mem.advance(s.size());
  } else if (typeid(p) == typeid(PUP::toMem)) {
    auto& mem = *static_cast<PUP::toMem*>(&p);
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

template <>
struct puper<chare_t> {
  using impl_type = std::underlying_type<chare_t>::type;

  inline static void impl(serdes& s, chare_t& t) {
    s | reinterpret_cast<impl_type&>(t);
  }
};

template <typename T>
struct puper<T, typename std::enable_if<is_list_or_deque<T>::value>::type> {
  using value_type = typename T::value_type;

  inline static void impl(serdes& s, T& t) {
    if (s.unpacking()) {
      std::size_t size;
      s.copy(&size);
      ::new (&t) T();
      for (auto i = 0; i < size; i++) {
        PUP::detail::TemporaryObjectHolder<value_type> h;
        pup(s, h.t);
        t.push_back(h.t);
      }
    } else {
      auto size = t.size();
      s.copy(&size);
      for (auto& i : t) {
        pup(s, i);
      }
    }
  }
};

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
struct puper<
    std::shared_ptr<T>,
    typename std::enable_if<std::is_base_of<PUP::able, T>::value>::type> {
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

namespace {
template <typename T, typename IdentifyFn>
inline static void pack_ptr(serdes& s, std::shared_ptr<T>& p,
                            const IdentifyFn& f) {
  if (!p) {
    ptr_record rec(nullptr);
    pup(s, rec);
  } else {
    auto search = s.records.find(p);
    if (search != s.records.end()) {
      ptr_record rec(search->second);
      pup(s, rec);
    } else {
      auto id = s.records.size();
      ptr_record rec(id, f());
      pup(s, rec);
      s.records[p] = id;
      pup(s, *p);
    }
  }
}
}

template <>
struct puper<polymorph> {
  inline static void impl(serdes& s, polymorph& t) { t.__pup__(s); }
};

template <>
struct puper<ptr_record> {
  inline static void impl(serdes& s, ptr_record& t) {
    auto ty = reinterpret_cast<std::uint8_t*>(&t.t);
    s.copy(ty);
    switch (*ty) {
      case ptr_record::REFERENCE:
        pup(s, t.d.reference.id);
        break;
      case ptr_record::INSTANCE:
        pup(s, t.d.instance.id);
        pup(s, t.d.instance.ty);
        break;
      case ptr_record::IGNORE:
        break;
      default:
        CkAbort("unknown record type %d", static_cast<int>(*ty));
        break;
    }
  }
};

template <typename T>
struct puper<std::shared_ptr<T>,
             typename std::enable_if<
                 std::is_base_of<ergoline::object, T>::value ||
                 std::is_base_of<hypercomm::polymorph, T>::value>::type> {
  inline static void impl(serdes& s, std::shared_ptr<T>& t) {
    if (s.unpacking()) {
      ptr_record rec;
      pup(s, rec);
      if (rec.is_null()) {
        ::new (&t) std::shared_ptr<T>();
      } else {
        std::shared_ptr<polymorph> p;
        if (rec.is_instance()) {
          p = hypercomm::instantiate(rec.d.instance.ty);
          s.instances[rec.d.instance.id] = p;
          p->__pup__(s);
        } else if (rec.is_reference()) {
          p = std::static_pointer_cast<polymorph>(
              s.instances[rec.d.reference.id].lock());
        } else {
          CkAbort("unknown record type %d", static_cast<int>(rec.t));
        }
        ::new (&t) std::shared_ptr<T>(std::dynamic_pointer_cast<T>(p));
      }
    } else {
      auto p = std::dynamic_pointer_cast<polymorph>(t);
#if CMK_ERROR_CHECKING
      if (t && (p == nullptr))
        CkAbort("could not cast %s to pup'able", typeid(t.get()).name());
#endif
      pack_ptr(s, p, [p]() { return hypercomm::identify(*p); });
    }
  }
};

template <typename T>
class puper<std::shared_ptr<T>, typename std::enable_if<std::is_base_of<
                                    hypercomm::proxy, T>::value>::type> {
  template <typename A>
  inline static void helper(serdes& s, std::shared_ptr<T>& t) {
    if (s.unpacking()) {
      ::new (&t) std::shared_ptr<proxy>(new A());
    }

    s | std::dynamic_pointer_cast<A>(t)->proxy;
  }

 public:
  static void impl(serdes& s, std::shared_ptr<T>& t) {
    chare_t ty = s.unpacking() ? (chare_t::TypeInvalid) : t->type();
    s | ty;
    if (ty == chare_t::TypeChare || ty == chare_t::TypeMainChare) {
      helper<chare_proxy>(s, t);
    } else if (ty != chare_t::TypeInvalid) {
      bool collective = s.unpacking() ? false : t->collective();
      s | collective;
      if (collective) {
        CkAbort("collectives currently unsupported");
      }
      switch (ty) {
        case chare_t::TypeArray: {
          helper<array_element_proxy>(s, t);
          break;
        }
        case chare_t::TypeGroup: {
          helper<group_element_proxy>(s, t);
          break;
        }
        case chare_t::TypeNodeGroup: {
          helper<nodegroup_element_proxy>(s, t);
          break;
        }
        default: { CkAbort("unknown chare type"); }
      }
    } else {
      CkAbort("invalid chare type");
    }
  }
};

template <typename T>
struct puper<std::shared_ptr<T>,
             typename std::enable_if<!hypercomm::is_pupable<T>::value>::type> {
  inline static void unpack(serdes& s, std::shared_ptr<T>& t) {
    ptr_record rec;
    pup(s, rec);
    if (rec.is_null()) {
      ::new (&t) std::shared_ptr<T>();
    } else if (rec.is_instance()) {
      if (is_bytes<T>()) {
        ::new (&t) std::shared_ptr<T>(std::move(s.source.lock()),
                                      reinterpret_cast<T*>(s.current));
        s.advance<T>();
      } else {
        auto p = static_cast<T*>(malloc(sizeof(T)));
        ::new (&t) std::shared_ptr<T>(p, [](T* p) {
          p->~T();
          free(p);
        });
      }
      s.instances[rec.d.instance.id] = t;
      if (!is_bytes<T>()) {
        pup(s, *t);
      }
    } else if (rec.is_reference()) {
      ::new (&t) std::shared_ptr<T>(
          std::static_pointer_cast<T>(s.instances[rec.d.reference.id].lock()));
    } else {
      CkAbort("unknown record type %d", static_cast<int>(rec.t));
    }
  }

  inline static void impl(serdes& s, std::shared_ptr<T>& t) {
    if (s.unpacking()) {
      unpack(s, t);
    } else {
      pack_ptr(s, t, []() { return 0; });
    }
  }
};

namespace {
template <bool B>
using Requires = PUP::Requires<B>;

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N == 0)> = nullptr>
inline void pup_tuple_impl(serdes& s, std::tuple<Args...>& t) {
  pup(s, std::get<N>(t));
}

template <size_t N, typename... Args,
          Requires<(sizeof...(Args) > 0 && N > 0)> = nullptr>
inline void pup_tuple_impl(serdes& s, std::tuple<Args...>& t) {
  pup_tuple_impl<N - 1>(s, t);
  pup(s, std::get<N>(t));
}
}

template <typename... Ts>
struct puper<std::tuple<Ts...>,
             typename std::enable_if<(sizeof...(Ts) > 0)>::type> {
  inline static void impl(serdes& s, std::tuple<Ts...>& t) {
    pup_tuple_impl<sizeof...(Ts)-1>(s, t);
  }
};

template <typename... Ts>
struct puper<std::tuple<Ts...>,
             typename std::enable_if<(sizeof...(Ts) == 0)>::type> {
  inline static void impl(serdes& s, std::tuple<Ts...>& t) {}
};

namespace {
template <typename T>
bool is_uninitialized(std::weak_ptr<T> const& weak) {
  using wt = std::weak_ptr<T>;
  return !weak.owner_before(wt{}) && !wt{}.owner_before(weak);
}
}

template <typename T, std::size_t N>
struct puper<ergoline::array<T, N>> {
  inline static void impl(serdes& s, ergoline::array<T, N>& t) {
    if (s.unpacking()) {
      reconstruct(&t);
    }

    pup(s, t.shape);

    if (s.unpacking()) {
      if (PUP::as_bytes<T>::value) {
        if (!is_uninitialized(s.source)) {
          t.source = std::move(s.source.lock());
          t.buffer = reinterpret_cast<T*>(s.current);
          s.advance(sizeof(T) * t.size());
        } else {
          t.alloc(false, false);
          s.copy(t.buffer, t.size());
        }
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
inline void pup(serdes& s, const T& t) {
  puper<T>::impl(s, const_cast<T&>(t));
}

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
