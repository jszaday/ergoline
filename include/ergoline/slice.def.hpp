#ifndef ERGOLINE_SLICE_DEF_HPP
#define ERGOLINE_SLICE_DEF_HPP

#include "slice.decl.hpp"
#include "iterator.def.hpp"

namespace ergoline {
struct slice;

std::shared_ptr<iterator<int>> make_slice_iterator(const slice& self) {
  if (self.end) {
    auto start = self.start ? *(self.start) : 0;
    auto& end = *(self.end);
    auto step = self.step ? *(self.step) : ((start > end) ? -1 : 1);
    return std::make_shared<range<int>::range_iterator>(
        range<int>(start, step, end));
  } else {
    return nullptr;
  }
}
}

#endif
