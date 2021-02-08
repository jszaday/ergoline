#ifndef __HYPERCOMM_SERDES_HPP__
#define __HYPERCOMM_SERDES_HPP__

#include <algorithm>
#include <memory>

namespace hypercomm {

struct serdes {
  enum state_t { SIZING, PACKING, UNPACKING };

  const std::shared_ptr<void> source;
  const char* start;
  char* current;
  const state_t state;

  inline bool unpacking() { return state == state_t::UNPACKING; }

  inline std::size_t size() { return current - start; }

  template <typename T>
  inline void copy(T* data, std::size_t n = 1) {
    const auto nBytes = n * sizeof(T);
    switch (state) {
      case PACKING:
        std::copy(reinterpret_cast<char*>(data),
                  reinterpret_cast<char*>(data) + nBytes, current);
        break;
      case UNPACKING:
        std::copy(current, current + nBytes,
                  reinterpret_cast<char*>(data));
        break;
    }
    advance(nBytes);
  }

  inline void advance(std::size_t size) { current += size; }

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
        .source = nullptr,
        .start = start,
        .current = const_cast<char*>(start),
        .state = PACKING,
    };
  }

  inline static serdes make_sizer() {
    return serdes{
        .source = nullptr,
        .start = nullptr,
        .current = nullptr,
        .state = SIZING,
    };
  }
};
}

#endif
