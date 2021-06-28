#ifndef __ERGOLINE_SECTION_DEF_HPP__
#define __ERGOLINE_SECTION_DEF_HPP__

#include "section.decl.hpp"

namespace ergoline {
  template<typename T, std::size_t N>
  hypercomm::vector_section<T> conv2section(const std::shared_ptr<array<T, N>>& arr) {
    std::vector<T> vect(arr->size());
    std::copy(std::begin(*arr), std::end(*arr), std::begin(vect));
    return hypercomm::vector_section<T>(std::move(vect));
  }

  template<typename T>
  hypercomm::vector_section<T> conv2section(const range<T>& rng) {
    std::vector<T> vect{};
    auto iter = const_cast<range<T>&>(rng).iter();
    while (iter->hasNext()) {
      vect.push_back(iter->next());
    }
    return hypercomm::vector_section<T>(std::move(vect));
  }

  hypercomm::combiner_ptr make_null_combiner(void) {
    return std::make_shared<hypercomm::null_combiner>();
  }
}

#endif
