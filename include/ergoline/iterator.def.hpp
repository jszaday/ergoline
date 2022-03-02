#ifndef ERGOLINE_ITERATOR_DEF_HPP
#define ERGOLINE_ITERATOR_DEF_HPP

#include "iterator.decl.hpp"

namespace ergoline {

template <typename T> inline iterator_t<T> access_value_iter(const T &t) {
  return const_cast<T &>(t).iter();
}

template <typename T>
inline iterator_t<T> access_ref_iter(const std::shared_ptr<T> &t) {
  return t->iter();
}

template <typename T, typename Enable = void> struct iterator_accessor {
  using iter_type = iterator_for<extricate_t<T>>;

  iterator_t<T> operator()(const T &t) const { return iter_type::accessor(t); }
};

template <typename T>
using enable_if_iterator_t = typename std::enable_if<
    hypercomm::is_base_of_template<T, iterator>::value>::type;

template <typename T>
struct iterator_accessor<std::shared_ptr<T>, enable_if_iterator_t<T>> {
  std::shared_ptr<T> operator()(const std::shared_ptr<T> &t) const { return t; }
};

template <typename T>
auto access_iterator(const T &t)
    -> decltype(std::declval<iterator_accessor<T>>()(std::declval<T>())) {
  return iterator_accessor<T>()(t);
}

template <typename T>
inline std::shared_ptr<imprintable_t<T>> conv2section(const T &t) {
  using iter_type = iterator_for<extricate_t<T>>;
  using value_type = typename iter_type::value_type;
  using section_type = hypercomm::vector_section<value_type>;
  std::vector<value_type> vect{};
  auto iter = access_iterator(t);
  while (iter->hasNext()) {
    vect.push_back(iter->next());
  }
  return std::static_pointer_cast<imprintable_t<T>>(
      std::make_shared<section_type>(std::move(vect)));
}

hypercomm::combiner_ptr make_null_combiner(void) {
  return std::make_shared<hypercomm::null_combiner>();
}
} // namespace ergoline

#endif
