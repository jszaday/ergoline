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
inline std::shared_ptr<imprintable_t<T>> conv2section(const T& t) {
  using iter_type = iterator_for<extricate_t<T>>;
  using value_type = typename iter_type::value_type;
  using section_type = hypercomm::vector_section<value_type>;
  std::vector<value_type> vect{};
  auto iter = iter_type::accessor(t);
  while (iter->hasNext()) {
    vect.push_back(iter->next());
  }
  return std::static_pointer_cast<imprintable_t<T>>(
      std::make_shared<section_type>(std::move(vect)));
}

hypercomm::combiner_ptr make_null_combiner(void) {
  return std::make_shared<hypercomm::null_combiner>();
}

std::shared_ptr<iterator<int>> make_slice_iterator(const slice& self) {
  if (self.stop) {
    auto start = self.start ? *(self.start) : 0;
    auto& stop = *(self.stop);
    auto step = self.step ? *(self.step) : ((start > stop) ? -1 : 1);
    return std::make_shared<range<int>::range_iterator>(
        range<int>(start, step, stop));
  } else {
    return nullptr;
  }
}
}  // namespace ergoline

#endif
