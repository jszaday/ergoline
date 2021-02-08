#ifndef __HYPERCOMM_POLYMORPH_HPP__
#define __HYPERCOMM_POLYMORPH_HPP__

#include <map>
#include <memory>
#include <typeindex>
#include <functional>

#include <converse.h>

#include <hypercomm/serdes.hpp>

namespace hypercomm {

struct polymorph {
  virtual ~polymorph() = default;
  virtual void __pup__(serdes& s) = 0;
};

using polymorph_id_t = std::size_t;
using polymorph_ptr_t = std::shared_ptr<polymorph>;
using allocator_t = std::function<polymorph_ptr_t(void)>;

namespace {
using type_registry_t = std::map<std::type_index, polymorph_id_t>;
using alloc_registry_t = std::map<polymorph_id_t, allocator_t>;

CsvDeclare(type_registry_t, type_registry_);
CsvDeclare(alloc_registry_t, alloc_registry_);
}

namespace detail {
void initialize() {
  CsvInitialize(type_registry_t, type_registry_);
  CsvInitialize(alloc_registry_t, alloc_registry_);
}
}

polymorph_id_t enroll(const std::type_index& index, const allocator_t& alloc) {
  auto& t = CsvAccess(type_registry_);
  auto& i = CsvAccess(alloc_registry_);
  const auto id = static_cast<polymorph_id_t>(t.size());
  t[index] = id;
  i[id] = alloc;
  return id;
}

template<typename T>
inline polymorph_id_t enroll() {
  return enroll(std::type_index(typeid(T)), []() {
    return std::make_shared<T>(PUP::reconstruct{});
  });
}

inline polymorph_id_t identify(const std::type_index& index) {
  return (CsvAccess(type_registry_))[index];
}

inline polymorph_id_t identify(const polymorph& morph) {
  return identify(std::type_index(typeid(morph)));
}

inline polymorph_ptr_t instantiate(const polymorph_id_t& id) {
  return ((CsvAccess(alloc_registry_))[id])();
}
}

#endif
