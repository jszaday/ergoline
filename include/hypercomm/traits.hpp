#ifndef __HYPERCOMM_TRAITS_HPP__
#define __HYPERCOMM_TRAITS_HPP__

#include <ergoline/object.hpp>

namespace hypercomm {
template <class T, typename Enable = void>
struct built_in {
  enum { value = 0 };
};

template <class T>
struct built_in<ck::future<T>> {
  enum { value = 1 };
};

template <>
struct built_in<PUP::able*> {
  enum { value = 1 };
};

template <>
struct built_in<std::string> {
  enum { value = 1 };
};

template <typename T>
struct built_in<
    T, typename std::enable_if<std::is_base_of<CProxy, T>::value>::type> {
  enum { value = 1 };
};

template <class T, typename Enable = void>
struct is_pupable {
  enum { value = 0 };
};

template <class T>
struct is_pupable<T,
  typename std::enable_if<std::is_base_of<PUP::able, T>::value ||
                          std::is_base_of<ergoline::object, T>::value>::type> {
  enum { value = 1 };
};

}

#endif
