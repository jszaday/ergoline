#ifndef __HYPERCOMM_TRAITS_HPP__
#define __HYPERCOMM_TRAITS_HPP__

#include <ergoline/object.hpp>
#include <ergoline/future.hpp>

#include <hypercomm/proxy.hpp>
#include <hypercomm/polymorph.hpp>

#include <deque>
#include <list>

namespace hypercomm {

template <typename T>
struct is_list_or_deque {
  enum { value = false };
};

template <typename T>
struct is_list_or_deque<std::list<T>> {
  enum { value = true };
};

template <typename T>
struct is_list_or_deque<std::deque<T>> {
  enum { value = true };
};

template <class T, typename Enable = void>
struct built_in {
  enum { value = 0 };
};

template <class T>
struct built_in<ergoline::future<T>> {
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
                  typename std::enable_if<
                      std::is_base_of<PUP::able, T>::value ||
                      std::is_base_of<ergoline::object, T>::value ||
                      std::is_base_of<hypercomm::proxy, T>::value ||
                      std::is_base_of<hypercomm::polymorph, T>::value>::type> {
  enum { value = 1 };
};
}

#endif
