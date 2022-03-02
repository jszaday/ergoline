#ifndef ERGOLINE_ITERATOR_DECL_HPP
#define ERGOLINE_ITERATOR_DECL_HPP

#include <hypercomm/core/inter_callback.hpp>
#include <hypercomm/core/null_combiner.hpp>
#include <hypercomm/sections.hpp>
#include <hypercomm/sections/imprintable.hpp>

#include "array.hpp"

namespace ergoline {
template <typename T, typename Enable = void>
struct iterator_for;

template <typename T>
using iterator_value_t = typename iterator_for<extricate_t<T>>::value_type;

template <typename T>
using iterator_t = std::shared_ptr<iterator<iterator_value_t<T>>>;

template <typename T>
using imprintable_t = hypercomm::imprintable<iterator_value_t<T>>;

template <typename T>
iterator_t<T> access_value_iter(const T&);

template <typename T>
iterator_t<T> access_ref_iter(const std::shared_ptr<T>&);

template <typename T>
std::shared_ptr<imprintable_t<T>> conv2section(const T&);

hypercomm::combiner_ptr make_null_combiner(void);
}  // namespace ergoline

#endif
