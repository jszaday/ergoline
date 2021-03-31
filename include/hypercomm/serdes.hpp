#ifndef __HYPERCOMM_SERDES_HPP__
#define __HYPERCOMM_SERDES_HPP__

#include <algorithm>
#include <memory>
#include <map>

namespace hypercomm {

using ptr_id_t = std::size_t;
using polymorph_id_t = std::size_t;
struct ptr_record;

struct serdes {
  enum state_t { SIZING, PACKING, UNPACKING };

  const std::weak_ptr<void> source;
  const char* start;
  char* current;
  const state_t state;
  std::map<std::weak_ptr<void>, ptr_id_t, std::owner_less<std::weak_ptr<void>>> records;
  std::map<ptr_id_t, std::weak_ptr<void>> instances;

  inline bool packing() const { return state == state_t::PACKING; }
  inline bool unpacking() const { return state == state_t::UNPACKING; }
  inline bool sizing() const { return state == state_t::SIZING; }

  inline std::size_t size() { return current - start; }

  template <typename T>
  inline void copy(T* data, std::size_t n = 1) {
    const auto nBytes = n * sizeof(T);
    const auto nAlign = 0; // CK_ALIGN(nBytes, sizeof(T));
    switch (state) {
      case PACKING:
        std::copy(reinterpret_cast<char*>(data),
                  reinterpret_cast<char*>(data) + nBytes,
                  current + nAlign);
        break;
      case UNPACKING:
        std::copy(current + nAlign, current + nAlign + nBytes,
                  reinterpret_cast<char*>(data));
        break;
    }
    advanceBytes(nAlign + nBytes);
  }

  inline void advanceBytes(std::size_t size) { current += size; }

  template <typename T>
  inline void advance(std::size_t n = 1) {
    current += sizeof(T) * n;
  }

  inline static serdes make_unpacker(const std::shared_ptr<void>& source,
                                     const char* start) {
    return serdes{
        .source = source,
        .start = start,
        .current = const_cast<char*>(start),
        .state = UNPACKING,
    };
  }

  inline static serdes make_packer(const char* start) {
    return serdes{
        .source = {},
        .start = start,
        .current = const_cast<char*>(start),
        .state = PACKING,
    };
  }

  inline static serdes make_sizer() {
    return serdes{
        .source = {},
        .start = nullptr,
        .current = nullptr,
        .state = SIZING,
    };
  }
};

struct ptr_record {
  union data_t {
    struct s_reference {
      ptr_id_t id;
    } reference;
    struct s_instance {
      ptr_id_t id;
      polymorph_id_t ty;
    } instance;
  };

  enum type_t: std::uint8_t {
    UNKNOWN,
    IGNORE,
    REFERENCE,
    INSTANCE
  };

  data_t d;
  type_t t;

  ptr_record(): t(UNKNOWN) {}
  ptr_record(std::nullptr_t): t(IGNORE) {}
  ptr_record(const ptr_id_t &id): t(REFERENCE) { d.reference.id = id; }
  ptr_record(const ptr_id_t &id, const polymorph_id_t& ty): t(INSTANCE) {
    d.instance.id = id;
    d.instance.ty = ty;
  }

  inline bool is_null() const { return t == IGNORE; }
  inline bool is_instance() const { return t == INSTANCE; }
  inline bool is_reference() const { return t == REFERENCE; }
};

}

#endif
