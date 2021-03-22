#ifndef __HYPERCOMM_PROXY_HPP__
#define __HYPERCOMM_PROXY_HPP__

#include <ck.h>
#include <memory>

namespace hypercomm {
using chare_t = ChareType;

struct proxy {
  virtual chare_t type(void) const = 0;
  virtual int home(void) const = 0;
  virtual int last_known(void) const = 0;
  virtual bool collective(void) const = 0;
  virtual void* local(void) const = 0;
};

struct element_proxy : virtual public proxy {
  virtual bool collective(void) const override { return false; }
};

struct non_migratable_proxy : virtual public proxy {
  virtual int last_known(void) const override { return this->home(); }
};

struct chare_proxy : public non_migratable_proxy {
  using proxy_type = CProxy_Chare;

  proxy_type proxy;

  chare_proxy(void) = default;
  chare_proxy(const proxy_type& _1) : proxy(_1) {}

  inline const CkChareID& id(void) const { return proxy.ckGetChareID(); }

  virtual chare_t type(void) const override { return chare_t::TypeChare; }

  virtual int home(void) const override { return this->id().onPE; }

  virtual bool collective(void) const override { return false; }

  virtual void* local(void) const override {
    return (this->home() == CkMyPe()) ? (this->id().objPtr) : nullptr;
  }
};

struct array_element_proxy : public element_proxy {
  using proxy_type = CProxyElement_ArrayElement;

  proxy_type proxy;

  array_element_proxy(void) = default;
  array_element_proxy(const proxy_type& _1) : proxy(_1) {}

  inline CkArrayID id(void) const { return proxy.ckGetArrayID(); }
  inline const CkArrayIndex& index(void) const { return proxy.ckGetIndex(); }

  virtual chare_t type(void) const override { return chare_t::TypeArray; }

  virtual int home(void) const override {
    return this->id().ckLocalBranch()->homePe(this->index());
  }

  virtual int last_known(void) const override {
    return this->id().ckLocalBranch()->lastKnown(this->index());
  }

  virtual void* local(void) const override {
    return this->id().ckLocalBranch()->lookup(this->index());
  }
};

template<typename T>
struct grouplike_element_proxy : public element_proxy, public non_migratable_proxy {
  using proxy_type = T;

  static constexpr auto is_node = std::is_same<CProxyElement_NodeGroup, proxy_type>::value;

  proxy_type proxy;

  grouplike_element_proxy(void) = default;
  grouplike_element_proxy(const proxy_type& _1) : proxy(_1) {}

  inline CkGroupID id(void) const { return proxy.ckGetGroupID(); }
  inline int index(void) const { return proxy.ckGetGroupPe(); }

  virtual chare_t type(void) const override {
    return (is_node) ? (chare_t::TypeNodeGroup)
                     : (chare_t::TypeGroup);
  }

  virtual int home(void) const override {
    return this->index();
  }

  virtual void* local(void) const override {
    if (is_node) {
      return (this->home() == CkMyNode()) ? CkLocalNodeBranch(this->id()) : nullptr;
    } else {
      return (this->home() == CkMyPe()) ? CkLocalBranch(this->id()) : nullptr;
    }
  }
};

using group_element_proxy = grouplike_element_proxy<CProxyElement_Group>;

using nodegroup_element_proxy = grouplike_element_proxy<CProxyElement_NodeGroup>;

inline std::shared_ptr<chare_proxy> make_proxy(const chare_proxy::proxy_type& base) {
  return std::make_shared<chare_proxy>(base);
}

inline std::shared_ptr<array_element_proxy> make_proxy(const array_element_proxy::proxy_type& base) {
  return std::make_shared<array_element_proxy>(base);
}

inline std::shared_ptr<group_element_proxy> make_proxy(const group_element_proxy::proxy_type& base) {
  return std::make_shared<group_element_proxy>(base);
}

inline std::shared_ptr<nodegroup_element_proxy> make_proxy(const nodegroup_element_proxy::proxy_type& base) {
  return std::make_shared<nodegroup_element_proxy>(base);
}
}

#endif
