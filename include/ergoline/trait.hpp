#ifndef ERGOLINE_TRAIT_HPP
#define ERGOLINE_TRAIT_HPP

#include <tuple>

namespace ergoline {
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

template <typename T, int N>
using tuple_multiply_t = typename tuple_multiply<T, N>::type;
}  // namespace ergoline

#endif
