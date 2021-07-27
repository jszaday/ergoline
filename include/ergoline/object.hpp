#ifndef __ERGOLINE_OBJECT_HPP__
#define __ERGOLINE_OBJECT_HPP__

#include <hypercomm/core/locality_base.hpp>
#include <hypercomm/core/comparable.hpp>
#include <hypercomm/utilities.hpp>

namespace ergoline {

template <typename T>
using element_t = typename T::element_t;

template <typename Proxy, typename Index>
element_t<Proxy> create_element(Proxy& proxy, const Index& idx) {
  auto element = proxy[hypercomm::conv2idx<CkArrayIndex>(idx)];
  element.insert();
  return element;
}

template <typename Proxy, typename Index>
element_t<Proxy> create_element(Proxy& proxy, const Index& idx,
                                const int& onPe) {
  auto element = proxy[hypercomm::conv2idx<CkArrayIndex>(idx)];
  element.insert(onPe);
  return element;
}

template <typename Proxy, typename Index>
element_t<Proxy> create_element(Proxy& proxy, const Index& idx,
                                CkMessage* msg) {
  auto element = proxy[hypercomm::conv2idx<CkArrayIndex>(idx)];
  element.insert(msg);
  return element;
}

template <typename Proxy, typename Index>
element_t<Proxy> create_element(Proxy& proxy, const Index& idx, CkMessage* msg,
                                const int& onPe) {
  auto element = proxy[hypercomm::conv2idx<CkArrayIndex>(idx)];
  element.insert(msg, onPe);
  return element;
}

struct object_base_
    : public hypercomm::polymorph,
      public hypercomm::virtual_enable_shared_from_this<object_base_> {
  virtual std::shared_ptr<object_base_> __this_object__(void) {
    return this->shared_from_this();
  }
};

template <typename T>
struct object : virtual public object_base_,
                virtual public hypercomm::comparable {
  virtual bool equals(const std::shared_ptr<comparable>& other) const override {
    return this == dynamic_cast<object*>(other.get());
  }
};

template <class T>
struct trait : virtual public hypercomm::polymorph::trait,
               virtual public object_base_ {
  friend T;

 private:
  inline std::shared_ptr<T> __self__(void) {
    return std::dynamic_pointer_cast<T>(this->__this_object__());
  }
};
}

#include "hash.hpp"
#include "util.hpp"
#include "message.hpp"
#include "singleton.hpp"

#endif
