#ifndef ERGOLINE_TO_STRING_HPP
#define ERGOLINE_TO_STRING_HPP

#include <pup.h>

#include <memory>
#include <string>
#include <type_traits>

namespace ergoline {

template <typename T>
std::string to_string(const T& t);

template <typename T, typename U = void>
struct has_toString : std::false_type {};

template <typename T>
struct has_toString<
    T, PUP::details::void_t<typename std::enable_if<std::is_same<
           decltype(std::declval<T&>().toString()), std::string>::value>::type>>
    : std::true_type {};

template <typename T, typename U = void>
struct has_to_string : std::false_type {};

template <typename T>
struct has_to_string<
    T,
    PUP::details::void_t<typename std::enable_if<std::is_same<
        decltype(std::declval<T&>().to_string()), std::string>::value>::type>>
    : std::true_type {};

template <typename T, typename Enable = void>
struct to_string_helper_ {
  std::string operator()(const T& t) const { return std::to_string(t); }
};

template <typename T>
struct to_string_helper_<
    T, typename std::enable_if<has_toString<T>::value>::type> {
  std::string operator()(const T& t) const {
    return const_cast<T&>(t).toString();
  }
};

template <typename T>
struct to_string_helper_<
    T, typename std::enable_if<has_to_string<T>::value>::type> {
  std::string operator()(const T& t) const {
    return const_cast<T&>(t).to_string();
  }
};

template <>
struct to_string_helper_<std::string> {
  std::string operator()(const std::string& t) const { return t; }
};

template <typename T>
struct to_string_helper_<std::shared_ptr<T>> {
  std::string operator()(const std::shared_ptr<T>& t) const {
    return to_string(*t);
  }
};

template <typename T>
struct to_string_helper_<std::unique_ptr<T>> {
  std::string operator()(const std::unique_ptr<T>& t) const {
    return to_string(*t);
  }
};

template <typename T>
std::string to_string(const T& t) {
  return to_string_helper_<T>()(t);
}
}  // namespace ergoline

#endif
