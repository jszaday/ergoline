#ifndef __HYPERCOMM_PROXY_HPP__
#define __HYPERCOMM_PROXY_HPP__

#include <ck.h>
#include <memory>
#include <utility>

namespace hypercomm {
using chare_t = ChareType;

struct proxy {
public:
  virtual chare_t type(void) const = 0;
  virtual int home(void) const = 0;
  virtual int last_known(void) const = 0;
  virtual bool collective(void) const = 0;
  inline bool node_level(void) const;

  inline std::pair<int, bool> path(void) const {
    auto home = this->home();
    auto last = this->last_known();
    auto node = this->node_level();
    auto mine = node ? CkMyNode() : CkMyPe();
    auto dst = (home == mine) ? last : home;
    return std::make_pair(dst, node);
  }

  virtual bool equals(const hypercomm::proxy& other) const = 0;

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

  virtual bool equals(const hypercomm::proxy& _1) const override {
    const auto* _2 = dynamic_cast<const chare_proxy*>(&_1);

    if (_2) {
      const auto& ours = this->id();
      const auto& theirs = _2->id();
      return (ours.onPE == theirs.onPE) && (ours.objPtr == theirs.objPtr);
    } else {
      return false;
    }
  }

  inline const CkChareID& id(void) const { return proxy.ckGetChareID(); }

  virtual chare_t type(void) const override { return chare_t::TypeChare; }

  virtual int home(void) const override { return this->id().onPE; }

  virtual bool collective(void) const override { return false; }

  virtual void* local(void) const override {
    return (this->home() == CkMyPe()) ? (this->id().objPtr) : nullptr;
  }
};

struct array_element_proxy : public element_proxy {
  using proxy_type = CProxyElement_ArrayBase;

  proxy_type proxy;

  array_element_proxy(void) = default;
  array_element_proxy(const proxy_type& _1) : proxy(_1) {}

  virtual bool equals(const hypercomm::proxy& _1) const override {
    const auto* other = dynamic_cast<const array_element_proxy*>(&_1);
    return (other != nullptr) &&
      (const_cast<proxy_type&>(proxy) == other->proxy);
  }

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

  virtual bool equals(const hypercomm::proxy& _1) const override {
    const auto* other = dynamic_cast<const grouplike_element_proxy<T>*>(&_1);
    return (other != nullptr) && (const_cast<proxy_type&>(proxy) == other->proxy);
  }

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

inline bool proxy::node_level(void) const {
  return dynamic_cast<const nodegroup_element_proxy*>(this) != nullptr;
}

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
