#ifndef __ERGOLINE_SINGLETON_HPP__
#define __ERGOLINE_SINGLETON_HPP__

#include <hypercomm/serialization/pup.hpp>

namespace ergoline {

struct generic_singleton : public hypercomm::polymorph {
  virtual std::type_index type(void) const = 0;
};

template <typename T, typename Enable = void>
struct typed_singleton;

template <typename T>
struct typed_singleton<T, typename std::enable_if<std::is_base_of<
                              hypercomm::polymorph, T>::value>::type>
    : public generic_singleton {
  std::shared_ptr<T> instance;

  typed_singleton(void) : instance(new T()) {}

  typed_singleton(std::shared_ptr<T>&& _1) : instance(_1) {}

  typed_singleton(PUP::reconstruct) {}

  virtual std::type_index type(void) const override {
    return std::type_index(typeid(T));
  }

  virtual void __pup__(hypercomm::serdes& s) override { s | this->instance; }
};

namespace {
using singleton_value_type_ = std::unique_ptr<generic_singleton>;
using singleton_map_type_ = std::map<std::type_index, singleton_value_type_>;

CkpvDeclare(singleton_map_type_, singleton_map_);
CkpvDeclare(int, singleton_idx_);
CkpvDeclare(int, reduction_idx_);

inline singleton_value_type_& access_record_(const std::type_index& idx) {
  return CkpvAccess(singleton_map_)[idx];
}

template <typename T>
inline CkMessage* pack_record_(const CkCallback& cb) {
  auto& ptr = access_record_(std::type_index(typeid(T)));
  CkAssert(ptr && "cannot pup an invalid record");
  return hypercomm::pack(cb, ptr);
}

void* null_reducer_(int* size, void* local, void** remote, int count) {
  // for (auto i = 0; i < count; i++) {
  //   CmiFree(remote[i]);
  // }

  return local;
}

void singleton_handler_(envelope* env) {
  std::unique_ptr<generic_singleton> value;
  CkCallback cb;
  // Unpack the Instance and Callback from the Broadcast
  CkUnpackMessage(&env);
  auto* msg = (CkMessage*)EnvToUsr(env);
  hypercomm::unpack(msg, cb, value);
  if (CkMyPe() != env->getSrcPe()) {
    auto& ptr = access_record_(value->type());
    ptr.reset(value.release());
  }
  // Contribute to the Reduction Across PEs
  auto size = PUP::size(cb);
  auto rednEnv = _allocEnv(RODataMsg, size);
  PUP::toMemBuf(cb, EnvToUsr(rednEnv), size);
  CmiSetHandler(rednEnv, CkpvAccess(reduction_idx_));
  CmiReduce(rednEnv, rednEnv->getTotalsize(), null_reducer_);
}

void reduction_handler_(envelope* env) {
  PUP::fromMem p(EnvToUsr(env));
  CkCallback cb;
  p | cb;
  cb.send(CkAllocateMarshallMsg(0, NULL));
  CmiFree(env);
}
}

inline void setup_singleton_module(void) {
  CkpvInitialize(singleton_map_type, singleton_map_);
  CkpvInitialize(int, reduction_idx_);
  CkpvInitialize(int, singleton_idx_);
  CkpvAccess(reduction_idx_) =
      CmiRegisterHandler(reinterpret_cast<CmiHandler>(reduction_handler_));
  CkpvAccess(singleton_idx_) =
      CmiRegisterHandler(reinterpret_cast<CmiHandler>(singleton_handler_));
}

template <typename T>
inline void broadcast_singleton(const CkCallback& cb) {
  auto* msg = pack_record_<T>(cb);
  auto* env = UsrToEnv(msg);
  CkPackMessage(&env);
  env->setSrcPe(CkMyPe());
  CmiSetHandler(env, CkpvAccess(singleton_idx_));
  CmiSyncBroadcastAllAndFree(env->getTotalsize(), (char*)env);
}

template <typename T>
inline const std::shared_ptr<T>& access_singleton(void) {
  auto& ptr = access_record_(std::type_index(typeid(T)));
  typed_singleton<T>* record = nullptr;
  if (!ptr) {
    record = new typed_singleton<T>();
    ptr.reset(record);
  } else {
    record = static_cast<decltype(record)>(ptr.get());
  }
  return record->instance;
}
}

#endif
