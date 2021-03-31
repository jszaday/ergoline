#ifndef __ERGOLINE__FUTURE_HPP__
#define __ERGOLINE__FUTURE_HPP__

#include <ckfutures.h>
#include <ergoline/requests.hpp>

namespace ergoline {
using future_id_t = std::uint32_t;

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
namespace {
constexpr CmiUInt2 value_magic_nbr_ = 0x6969;
constexpr future_id_t identity_magic_nbr_ = std::numeric_limits<future_id_t>::max();
}

struct future_manager;

inline void register_future_handlers(void);

inline future make_future(future_manager* manager);
inline future make_future(const std::shared_ptr<hypercomm::proxy>& proxy);

inline void send_future(const future& f,
                        const std::shared_ptr<hypercomm::proxy>& dst,
                        std::shared_ptr<CkMessage>&& msg,
                        bool internal);

inline void send_future(const future& f,
                        const std::shared_ptr<hypercomm::proxy>& dst,
                        CkMessage* msg,
                        bool internal) {
  std::shared_ptr<CkMessage> ptr(msg, [](CkMessage* msg) {
    auto* env = UsrToEnv(msg);
    auto* hdr = reinterpret_cast<char*>(env) + CmiReservedHeaderSize;
    auto& magic = *(reinterpret_cast<CmiUInt2*>(hdr));
    if (magic == value_magic_nbr_) {
      CkAbort("unsanitized buffer!");
    }
    CkFreeMsg(msg);
  });

  send_future(f, dst, std::move(ptr), internal);
}

inline void send_future(const future& f, CkMessage* msg) {
  send_future(f, f.proxy, msg, false);
}

namespace {
using msg_size_t = UInt;
CkpvDeclare(int, recv_val_idx_);
CkpvDeclare(int, recv_req_idx_);

char* get_value_buffer_(envelope* env) {
  auto* hdr = reinterpret_cast<char*>(env) + CmiReservedHeaderSize;
  auto& magic = *(reinterpret_cast<CmiUInt2*>(hdr));
  CkAssert((magic == value_magic_nbr_) && "magic number not found");
  return hdr + sizeof(CmiUInt2) + sizeof(std::uint8_t) + sizeof(msg_size_t);
}

msg_size_t extract_size_(envelope* env) {
  return *(reinterpret_cast<msg_size_t*>(get_value_buffer_(env) - sizeof(msg_size_t)));
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

msg_size_t make_value_header_(
    const future& f, const std::shared_ptr<hypercomm::proxy>& dst,
    CkMessage* msg) {
  auto* env = UsrToEnv(msg);
  auto* raw = reinterpret_cast<char*>(env);
  auto* hdr = raw + CmiReservedHeaderSize;
  auto* end = raw + sizeof(envelope);
  auto& magic = *(reinterpret_cast<CmiUInt2*>(hdr));
  hdr += sizeof(CmiUInt2);
  std::uint8_t idx;
  msg_size_t  size;
  if (magic != value_magic_nbr_) {
    idx  = env->getMsgIdx();
    size = env->getTotalsize();
    magic = value_magic_nbr_;
  } else {
    idx  = *(reinterpret_cast<std::uint8_t*>(hdr));
    size = *(reinterpret_cast<msg_size_t*>(hdr + sizeof(std::uint8_t)));
  }
  CmiSetInfo(env, CkMyPe());
  CmiSetHandler(env, CpvAccess(recv_val_idx_));
  if (_msgTable[idx]->pack) {
    auto newMsg = _msgTable[idx]->pack(msg);
    CkAssert(msg == newMsg && "message changed due to packing!");
  }
  std::fill(hdr, end, '\0');
  auto s = hypercomm::serdes::make_packer(hdr);
  s | idx;
  s | size;
  s | f;
  if (f.proxy->equals(*dst)) {
    s | identity_magic_nbr_;
  } else {
    s | dst;
  }
  if (s.current > end) {
    CkAbort("ran out of space (%lu over)", s.current - end);
  }
  return size;
}

inline void undo_value_header_(const CkMessage* msg) {
  auto* env = UsrToEnv(msg);
  auto* hdr = reinterpret_cast<char*>(env) + CmiReservedHeaderSize;
  auto& magic = *(reinterpret_cast<CmiUInt2*>(hdr));

  CkAssert(magic == value_magic_nbr_ && "attempting to undo non-magical header");

  std::uint8_t index =
      *(reinterpret_cast<std::uint8_t*>(hdr + sizeof(CmiUInt2)));
  msg_size_t size =
      *(reinterpret_cast<msg_size_t*>(hdr + sizeof(std::uint8_t) + sizeof(CmiUInt2)));

  std::fill(hdr, reinterpret_cast<char*>(env) + sizeof(envelope), '\0');
  env->setMsgIdx(index);
  env->setTotalsize(size);

  CkAssert(magic != value_magic_nbr_ && "did not clear magical header");

  if (_msgTable[index]->pack) {
    env->setPacked(1);
    auto non_const_ = const_cast<CkMessage*>(msg);
    auto newMsg = _msgTable[index]->unpack(non_const_);
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

          if (act(msg)) {
            return true;
          } else {
            CkAbort("rejecting a future request is currently unsupported");
          }
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
  auto* env = reinterpret_cast<envelope*>(_1);
  auto buffer = get_value_buffer_(env);
  auto s = hypercomm::serdes::make_unpacker(nullptr, buffer);

  future f;
  std::shared_ptr<hypercomm::proxy> dst;
  s | f;

  auto& ident = *(reinterpret_cast<future_id_t*>(s.current));
  if (ident == identity_magic_nbr_) {
    dst = f.proxy;
  } else {
    s | dst;
  }

  auto* msg = static_cast<CkMessage*>(EnvToUsr(env));
  send_future(f, dst, msg, true);
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
          send_future(f, dst, std::move(val), false);
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
                        std::shared_ptr<CkMessage>&& msg,
                        bool internal) {
  auto* proxy = dst.get();
  auto* manager = as_manager_(*proxy);
  auto* env = UsrToEnv(msg.get());

  msg_size_t size;
  if (internal) {
    size = extract_size_(env);
  } else {
    size = make_value_header_(f, dst, msg.get());
  }

  if (manager == nullptr) {
    auto path = proxy->path();
    auto* raw = reinterpret_cast<char*>(env);

#if CMK_DEBUG
    CkPrintf("[%d] Routing a value for %s from %hu for %s via (%s) %d, header: %s\n",
              CkMyPe(), f.to_string().c_str(), CmiGetInfo(env),
              dst->to_string().c_str(), (path.second) ? "node" : "pe",
              path.first, buf2str(raw + CmiReservedHeaderSize, sizeof(envelope) - CmiReservedHeaderSize).c_str());
#endif

    if (path.second) {
      CmiSyncNodeSendAndFree(path.first, size, raw);
    } else {
      CmiSyncSendAndFree(path.first, size, raw);
    }

    CkAssert(msg.use_count() == 1);
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
