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
    switch (state) {
      case PACKING:
        std::copy(reinterpret_cast<char*>(data),
                  reinterpret_cast<char*>(data) + (n * sizeof(T)), current);
        break;
      case UNPACKING:
        std::copy(current, current + (n * sizeof(T)), reinterpret_cast<char*>(data));
        break;
    }
    advance<T>();
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
};
}

#endif
