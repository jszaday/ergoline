#ifndef __ERGOLINE__FUTURE_HPP__
#define __ERGOLINE__FUTURE_HPP__

#include <ckfutures.h>
#include <ergoline/requests.hpp>

namespace ergoline {
using future_id_t = std::uint64_t;

struct future {
  std::shared_ptr<hypercomm::proxy> proxy;
  future_id_t id;

  inline std::string to_string(void) const {
    std::stringstream ss;
    ss << "future(id=" << id << ",src=";
    if (proxy)
      ss << proxy->to_string() << ")";
    else
      ss << "(nil))";
    return ss.str();
  }
};
}

namespace hypercomm {

template <>
struct puper<ergoline::future> {
  inline static void impl(serdes& s, ergoline::future& f) {
    s | f.id;
    s | f.proxy;
  }
};
}

namespace ergoline {
struct future_manager;

inline void register_future_handlers(void);

inline future make_future(future_manager* manager);
inline future make_future(const std::shared_ptr<hypercomm::proxy>& proxy);

inline void send_future(const future& f,
                        const std::shared_ptr<hypercomm::proxy>& dst,
                        std::shared_ptr<CkMessage>&& msg);

inline void send_future(const future& f,
                        const std::shared_ptr<hypercomm::proxy>& dst,
                        CkMessage* msg) {
  std::shared_ptr<CkMessage> ptr(msg, [](CkMessage* msg) { CkFreeMsg(msg); });

  send_future(f, dst, std::move(ptr));
}

inline void send_future(const future& f, CkMessage* msg) {
  send_future(f, f.proxy, msg);
}

namespace {
using msg_size_t = UInt;
constexpr std::uint8_t value_magic_nbr_ = 0x42;
constexpr std::uint32_t identity_magic_nbr_ = 0x12345678;
CkpvDeclare(int, recv_val_idx_);
CkpvDeclare(int, recv_req_idx_);

char* get_value_buffer_(envelope* env) {
  auto* hdr = reinterpret_cast<char*>(env) + CmiReservedHeaderSize;
  auto& magic = *(reinterpret_cast<std::uint8_t*>(hdr));
  CkAssert((magic == value_magic_nbr_) && "magic number not found");
  return hdr + 2 * sizeof(std::uint8_t) + sizeof(msg_size_t);
}

future_id_t extract_id_(const CkMessage* msg) {
  auto* buffer = get_value_buffer_(UsrToEnv(msg));
  auto& id = *(reinterpret_cast<future_id_t*>(buffer));
  return id;
}

std::shared_ptr<hypercomm::proxy> extract_proxy_(const CkMessage* msg) {
  auto* buffer = get_value_buffer_(UsrToEnv(msg)) + sizeof(future_id_t);
  auto s = hypercomm::serdes::make_unpacker(nullptr, buffer);
  std::shared_ptr<hypercomm::proxy> proxy;
  s | proxy;
  return proxy;
}

inline std::size_t make_value_header_(
    const future& f, const std::shared_ptr<hypercomm::proxy>& dst,
    CkMessage* msg) {
  auto* env = UsrToEnv(msg);
  auto* hdr = reinterpret_cast<char*>(env) + CmiReservedHeaderSize;
  auto& magic = *(reinterpret_cast<std::uint8_t*>(hdr));
  auto& index = *(reinterpret_cast<std::uint8_t*>(hdr + sizeof(std::uint8_t)));
  auto& size = *(reinterpret_cast<msg_size_t*>(hdr + 2 * sizeof(std::uint8_t)));

  CmiSetHandler(env, CkpvAccess(recv_val_idx_));

  if (magic == value_magic_nbr_) {
    return size;
  } else {
    // Ensure the message's package routine is called before we wreck it
    if (_msgTable[env->getMsgIdx()]->pack) {
      auto newMsg =
          static_cast<CkMessage*>(_msgTable[env->getMsgIdx()]->pack(msg));
      CkAssert(msg == newMsg && "message changed due to packing!");
    }
    // Retrieve its "essential" properties
    std::uint8_t idx = env->getMsgIdx();
    msg_size_t sz = env->getTotalsize();
    // Then start wrecking it
    magic = value_magic_nbr_;
    index = idx;
    size = sz;
    auto buffer = hdr + 2 * sizeof(std::uint8_t) + sizeof(msg_size_t);
    auto s = hypercomm::serdes::make_packer(buffer);
    s | f;
    if (dst->equals(*f.proxy)) {
      s | identity_magic_nbr_;
    } else {
      s | dst;
    }
    CkAssert((s.current <= reinterpret_cast<char*>(msg)) &&
             "not enough free space in header");
    return size;
  }
}

inline void undo_value_header_(const CkMessage* msg) {
  auto* env = UsrToEnv(msg);

  auto* hdr = reinterpret_cast<char*>(env) + CmiReservedHeaderSize;

  std::uint8_t index =
      *(reinterpret_cast<std::uint32_t*>(hdr + sizeof(std::uint8_t)));
  msg_size_t size =
      *(reinterpret_cast<msg_size_t*>(hdr + 2 * sizeof(std::uint8_t)));

  std::fill(hdr, reinterpret_cast<char*>(env) + sizeof(envelope), '\0');
  env->setMsgIdx(index);
  env->setTotalsize(size);

  if (_msgTable[env->getMsgIdx()]->pack) {
    auto newMsg = _msgTable[index]->unpack(const_cast<CkMessage*>(msg));
    CkAssert(msg == newMsg && "message changed due to unpacking!");
  }
}

void remote_req_(const future& f, const std::shared_ptr<hypercomm::proxy>& dst);
}

class future_manager {
  mailbox<CkMessage*> mailbox_;
  future_id_t last_ = 0;

 public:
  using action_t = typename request<CkMessage*>::action_t;
  using request_t = std::shared_ptr<request<CkMessage*>>;

  future_id_t __next_future__(void) { return last_++; }

  request_t __make_future_req__(const future& f, const action_t& act) {
    auto ours = this->__proxy__();

    if (!ours->equals(*f.proxy)) {
      remote_req_(f, ours);
    }

    return mailbox_.make_request(
        [act](std::shared_ptr<CkMessage>& msg) {
          undo_value_header_(msg.get());
          return act(msg);
        },
        [f](const std::shared_ptr<CkMessage>& _1) {
          const auto* msg = _1.get();
          return f.id == extract_id_(msg) &&
                 f.proxy->equals(*(extract_proxy_(msg)));
        });
  }

  void __put_future_req__(const request_t& req) { mailbox_.put(req); }

  void __put_future_val__(std::shared_ptr<CkMessage>&& msg) {
    mailbox_.put(std::move(msg));
  }

  virtual std::shared_ptr<hypercomm::proxy> __proxy__(void) const = 0;
};

namespace {
future_manager* as_manager_(const hypercomm::proxy& proxy) {
  auto* local = static_cast<Chare*>(proxy.local());
  auto* manager = dynamic_cast<future_manager*>(local);
  CkAssert((!local || manager) && "future associated with a non-manager");
  return manager;
}

void recv_val_(void* _1) {
  auto* env = static_cast<envelope*>(_1);
  auto* msg = static_cast<CkMessage*>(EnvToUsr(env));
  auto buffer = get_value_buffer_(env);
  auto s = hypercomm::serdes::make_unpacker(nullptr, buffer);

  future f;
  std::shared_ptr<hypercomm::proxy> dst;
  s | f;

  auto& ident = *(reinterpret_cast<std::uint32_t*>(s.current));
  if (ident == identity_magic_nbr_) {
    dst = f.proxy;
  } else {
    s | dst;
  }

  send_future(f, dst, msg);
}

inline void send_remote_req_(envelope* env, const future& f,
                             const std::shared_ptr<hypercomm::proxy>& dst) {
  auto* proxy = f.proxy.get();
  auto manager = as_manager_(*proxy);
  if (manager == nullptr) {
    auto path = proxy->path();

    CmiSetHandler(env, CkpvAccess(recv_req_idx_));

    if (path.second) {
      CmiSyncNodeSendAndFree(path.first, env->getTotalsize(),
                             reinterpret_cast<char*>(env));
    } else {
      CmiSyncSendAndFree(path.first, env->getTotalsize(),
                         reinterpret_cast<char*>(env));
    }
  } else {
    auto req = manager->__make_future_req__(
        f, [=](std::shared_ptr<CkMessage>& val) -> bool {
          send_future(f, dst, std::move(val));
          return true;
        });

    manager->__put_future_req__(std::move(req));

    CmiFree(env);
  }
}

void recv_req_(void* _1) {
  auto* env = reinterpret_cast<envelope*>(_1);
  auto* buffer = reinterpret_cast<char*>(EnvToUsr(env));
  auto s = hypercomm::serdes::make_unpacker(nullptr, buffer);

  std::shared_ptr<hypercomm::proxy> dst;
  future f;

  s | f;
  s | dst;

  send_remote_req_(env, f, dst);
}

void remote_req_(const future& f,
                 const std::shared_ptr<hypercomm::proxy>& dst) {
  auto pup = [&](hypercomm::serdes& s) {
    s | f;
    s | dst;
  };

  auto szr = hypercomm::serdes::make_sizer();
  pup(szr);

  auto* env = _allocEnv(CkEnvelopeType::ForBocMsg, szr.size());
  auto pkr =
      hypercomm::serdes::make_packer(reinterpret_cast<char*>(EnvToUsr(env)));
  pup(pkr);

  send_remote_req_(env, f, dst);
}
}

inline future make_future(const std::shared_ptr<hypercomm::proxy>& proxy) {
  auto manager =
      dynamic_cast<future_manager*>(static_cast<Chare*>(proxy->local()));
  CkAssert((manager != nullptr) && "manager not found");
  return future{.proxy = proxy, .id = manager->__next_future__()};
}

inline future make_future(future_manager* manager) {
  return future{.proxy = manager->__proxy__(),
                .id = manager->__next_future__()};
}

inline void send_future(const future& f,
                        const std::shared_ptr<hypercomm::proxy>& dst,
                        std::shared_ptr<CkMessage>&& msg) {
  auto* proxy = dst.get();
  auto* manager = as_manager_(*proxy);
  auto size = make_value_header_(f, dst, msg.get());

  if (manager == nullptr) {
    auto path = proxy->path();
    auto* env = reinterpret_cast<char*>(UsrToEnv(msg.get()));

#ifdef CMK_DEBUG
    CkPrintf("[%d] Routing a value %s for %s via (%s) %d.\n",
              CkMyPe(), f.to_string().c_str(),
              dst->to_string().c_str(), (path.second) ? "node" : "pe",
              path.first);
#endif

    if (path.second) {
      CmiSyncNodeSendAndFree(path.first, size, env);
    } else {
      CmiSyncSendAndFree(path.first, size, env);
    }

    ::new (&msg) std::shared_ptr<CkMessage>{};
  } else {
    manager->__put_future_val__(std::move(msg));
  }
}

inline void register_future_handlers(void) {
  CkpvInitialize(int, recv_val_idx_);
  CkpvAccess(recv_val_idx_) =
      CmiRegisterHandler(reinterpret_cast<CmiHandler>(recv_val_));

  CkpvInitialize(int, recv_req_idx_);
  CkpvAccess(recv_req_idx_) =
      CmiRegisterHandler(reinterpret_cast<CmiHandler>(recv_req_));
}
}

#endif
