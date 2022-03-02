#ifndef ERGOLINE_TRAIT_HPP
#define ERGOLINE_TRAIT_HPP

#include <tuple>

namespace ergoline {
using unit_type = void;

template <typename T, int N, typename Enable = void>
struct tuple_multiply;

template <typename T, int N>
struct tuple_multiply<T, N, typename std::enable_if<(N <= 0)>::type> {
  using type = std::tuple<>;
};

template <typename T, int N>
struct tuple_multiply<T, N, typename std::enable_if<(N > 0)>::type> {
  using type = decltype(std::tuple_cat(
      std::declval<std::tuple<T>>(),
      std::declval<typename tuple_multiply<T, (N - 1)>::type>()));
};

template <typename T, typename Enable = void>
struct convert_tuple;

// empty tuples become "unit"
template <>
struct convert_tuple<std::tuple<>> {
  using type = unit_type;
};

// tuples with one element decay to that type
// (turns out this is _bad_ for TMP, consider refactor)
template <typename T>
struct convert_tuple<std::tuple<T>> {
  using type = T;
};

// tuples with multiple elements are unphased
template <typename... Ts>
struct convert_tuple<std::tuple<Ts...>,
                     typename std::enable_if<(sizeof...(Ts) >= 2)>::type> {
  using type = std::tuple<Ts...>;
};

template <typename T>
using convert_tuple_t = typename convert_tuple<T>::type;

template <typename T, int N>
using tuple_multiply_t = convert_tuple_t<typename tuple_multiply<T, N>::type>;
}  // namespace ergoline

#endif
