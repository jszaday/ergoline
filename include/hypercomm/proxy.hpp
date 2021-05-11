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

  virtual std::string to_string(void) const = 0;
};

template<typename Index>
struct element_proxy : virtual public proxy {
  virtual Index index() const = 0;
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
    auto& id = this->id();
    if (id.onPE == CkMyPe()) {
      auto* objs = &(CkpvAccess(chare_objs));
      if (reinterpret_cast<std::size_t>(id.objPtr) >= objs->size()) {
        return id.objPtr;
      } else {
        return CkLocalChare(&id);
      }
    } else {
      return nullptr;
    }
  }

  virtual std::string to_string(void) const override {
    std::stringstream ss;
    const auto& ourId = this->id();
    ss << "chare(pe=" << ourId.onPE << ",obj=" << ourId.objPtr << ")";
    return ss.str();
  }
};

struct array_element_proxy : public element_proxy<CkArrayIndex> {
  using proxy_type = CProxyElement_ArrayElement;

  proxy_type proxy;

  array_element_proxy(void) = default;
  array_element_proxy(const proxy_type& _1) : proxy(_1) {}

  virtual bool equals(const hypercomm::proxy& _1) const override {
    const auto* other = dynamic_cast<const array_element_proxy*>(&_1);
    return (other != nullptr) &&
      (const_cast<proxy_type&>(proxy) == other->proxy);
  }

  inline CkArrayID id(void) const { return proxy.ckGetArrayID(); }
  virtual CkArrayIndex index() const override { return proxy.ckGetIndex(); }

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

  virtual std::string to_string(void) const override {
    std::stringstream ss;
    const auto& idx = this->index();
    ss << "array(idx=[";
    if (idx.dimension > 4) {
      const auto& data = idx.indexShorts;
      for (auto i = 0; i < idx.dimension; i++) {
        ss << data[i] << ",";
      }
    } else {
      const auto& data = idx.index;
      for (auto i = 0; i < idx.dimension; i++) {
        ss << data[i] << ",";
      }
    }
    ss << "],id=" << ((CkGroupID)this->id()).idx << ")";
    return ss.str();
  }
};

template<typename T>
struct grouplike_element_proxy : public element_proxy<int>, public non_migratable_proxy {
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

  virtual int index(void) const override {
    return proxy.ckGetGroupPe();
  }

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

  virtual std::string to_string(void) const override {
    std::stringstream ss;
    ss << (is_node ? "nodegroup" : "group");
    ss << "(pe=" << this->index() << ",id=" << this->id().idx << ")";
    return ss.str();
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

template<typename T,
         PUP::Requires<std::is_base_of<array_element_proxy::proxy_type, T>::value> = nullptr>
inline std::shared_ptr<array_element_proxy> make_proxy(const T& base) {
  return std::make_shared<array_element_proxy>(static_cast<const array_element_proxy::proxy_type&>(base));
}

inline std::shared_ptr<group_element_proxy> make_proxy(const group_element_proxy::proxy_type& base) {
  return std::make_shared<group_element_proxy>(base);
}

inline std::shared_ptr<nodegroup_element_proxy> make_proxy(const nodegroup_element_proxy::proxy_type& base) {
  return std::make_shared<nodegroup_element_proxy>(base);
}
}

#endif
