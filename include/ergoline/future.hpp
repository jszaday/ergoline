#ifndef __ERGOLINE__FUTURE_HPP__
#define __ERGOLINE__FUTURE_HPP__

#include <ckfutures.h>
#include <ergoline/requests.hpp>

namespace ergoline {
using future_id_t = std::uint64_t;

struct future {
  std::shared_ptr<hypercomm::proxy> proxy;
  future_id_t id;
};

namespace {
  future_id_t extract_id(const CkMessage* msg) {
    return 0;
  }

  std::shared_ptr<hypercomm::proxy> extract_proxy(const CkMessage* msg) {
    return {};
  }

  inline std::size_t prepare_header(const future& f, CkMessage* msg) {
    // TODO adjust size
    // TODO set handler
    return 0;
  }
}

class future_mananger {
  mailbox<CkMessage*> mailbox_;
  future_id_t last_ = 0;
public:
  using action_t = typename request<CkMessage*>::action_t;
  using request_t = std::shared_ptr<request<CkMessage*>>;

  future_id_t __next_future__(void) {
    return last_++;
  }

  request_t __make_future_req__(const future& f, const action_t& act) {
    if (__proxy__()->equals(*f.proxy)) {
      return mailbox_.make_request(act, [f](const std::shared_ptr<CkMessage>& _1) {
        const auto* msg = _1.get();
        const auto theirs = extract_proxy(msg);
        return f.id == extract_id(msg) && f.proxy->equals(*theirs); 
      });
    } else {
      // TODO non-local req
    }
  }

  void __put_future_req__(request_t&& req) {
    mailbox_.put(req);
  }

  void __put_future_val__(std::shared_ptr<CkMessage>&& msg) {
    mailbox_.put(std::move(msg));
  }

  virtual std::shared_ptr<hypercomm::proxy> __proxy__(void) = 0;
};

inline future make_future(future_mananger* manager) {
  return future {
    .proxy = manager->__proxy__(),
    .id = manager->__next_future__()
  };
}

inline void send_future(const future& f, CkMessage* msg) {
  const auto* proxy = f.proxy.get();
  auto* local = static_cast<Chare*>(proxy->local());
  auto* manager = dynamic_cast<future_mananger*>(local);
  CkAssert((!local || manager) && "future associated with a non-mananger");

  if (manager == nullptr) {
    auto path = proxy->path();
    auto size = prepare_header(f, msg);

    if (path.first) {
      CmiSyncNodeSendAndFree(path.second, size, reinterpret_cast<char*>(UsrToEnv(msg)));
    } else {
      CmiSyncSendAndFree(path.second, size, reinterpret_cast<char*>(UsrToEnv(msg)));
    }
  } else {
    auto ptr = std::shared_ptr<CkMessage>(msg, [](CkMessage* msg) {
      CkFreeMsg(msg);
    });

    manager->__put_future_val__(std::move(ptr));
  }
}
}

#endif
