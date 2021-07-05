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

struct object : virtual public hypercomm::polymorph,
                virtual public hypercomm::comparable {
  virtual bool equals(const std::shared_ptr<comparable>& other) const override {
    return this == dynamic_cast<object*>(other.get());
  }
};
}

#include "hash.hpp"
#include "util.hpp"
#include "message.hpp"
#include "singleton.hpp"

#endif
