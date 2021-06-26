#ifndef __ERGOLINE_SECTION_DECL_HPP__
#define __ERGOLINE_SECTION_DECL_HPP__

#include <hypercomm/core/null_combiner.hpp>
#include <hypercomm/core/inter_callback.hpp>

#include <hypercomm/sections.hpp>

#include "array.hpp"

namespace ergoline {
  template<typename Index, typename T>
  hypercomm::vector_section<Index> conv2section(const T&);

  template<typename T, std::size_t N>
  hypercomm::vector_section<T> conv2section(const std::shared_ptr<array<T, N>>&);

  template<typename T>
  hypercomm::vector_section<T> conv2section(const range<T>&);

  hypercomm::combiner_ptr make_null_combiner(void);
}

#endif
