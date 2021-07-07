#ifndef __ERGOLINE_SECTION_DEF_HPP__
#define __ERGOLINE_SECTION_DEF_HPP__

#include "section.decl.hpp"

namespace ergoline {

template <typename T>
inline iterator_t<T> access_value_iter(const T& t) {
  return const_cast<T&>(t).iter();
}

template <typename T>
inline iterator_t<T> access_ref_iter(const std::shared_ptr<T>& t) {
  return t->iter();
}

template <typename T>
inline hypercomm::vector_section<iterator_value_t<T>> conv2section(const T& t) {
  using iter_type = iterator_for<extricate_t<T>>;
  using value_type = typename iter_type::value_type;
  std::vector<value_type> vect{};
  auto iter = iter_type::accessor(t);
  while (iter->hasNext()) {
    vect.push_back(iter->next());
  }
  return hypercomm::vector_section<value_type>(std::move(vect));
}

hypercomm::combiner_ptr make_null_combiner(void) {
  return std::make_shared<hypercomm::null_combiner>();
}
}

#endif
