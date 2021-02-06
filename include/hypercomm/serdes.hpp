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

  inline std::size_t size() { return current - start; }

  inline void copy_in(std::size_t size, char* data) {
    switch (state) {
      case PACKING:
        std::copy(data, data + size, current);
        current += size;
        break;
      case SIZING:
        current += size;
        break;
    }
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
