#ifndef ERGOLINE_SLICE_DECL_HPP
#define ERGOLINE_SLICE_DECL_HPP

#include "iterator.decl.hpp"

namespace ergoline {
struct slice;

std::shared_ptr<iterator<int>> make_slice_iterator(const slice&);
}

#endif
