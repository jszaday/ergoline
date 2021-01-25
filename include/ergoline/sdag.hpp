#ifndef __ERGOLINE_SDAG_HPP__
#define __ERGOLINE_SDAG_HPP__

#include <map>
#include <deque>
#include <tuple>
#include <memory>
#include <functional>

#include <pup_stl.h>
#include <converse.h>

namespace ergoline {

template <typename... Ts>
struct mailbox {
  using tuple_t = std::tuple<Ts...>;
  using value_t = std::shared_ptr<tuple_t>;
  struct request {
    virtual bool stale() = 0;
    virtual bool ready() = 0;
    virtual bool accepts(const value_t& value) = 0;
    virtual void fulfill(const value_t& value) = 0;
  };
  using request_t = std::shared_ptr<request>;

 private:
  std::deque<value_t> values_;
  std::deque<request_t> requests_;

 public:
  void put(const value_t& value) {
    for (auto it = requests_.begin(); it != requests_.end();) {
      if ((*it)->stale()) {
        it = requests_.erase(it);
      } else if ((*it)->accepts(value)) {
        (*it)->fulfill(value);
        it = requests_.erase(it);
        return;
      } else {
        it++;
      }
    }
    // if nobody accepted the value, save it
    values_.push_back(value);
  }

  bool req(const request_t& req) {
    CmiAssert(!req->stale());
    for (auto it = values_.begin(); it != values_.end();) {
      if (req->accepts(*it)) {
        req->fulfill(*it);
        it = values_.erase(it);
        return true;
      }
    }
    requests_.push_back(req);
    return false;
  }

  void pup(PUP::er& p) {
    CmiAssert(requests_.empty());
    p | values_;
  }
};

namespace requests {
template <typename... Ts>
struct to_thread : public mailbox<Ts...>::request {
  using value_t = typename mailbox<Ts...>::value_t;
  using cond_t = std::function<bool(const value_t&)>;

 private:
  cond_t cond_;
  value_t value_;
  CthThread th_;

 public:
  to_thread() {}

  to_thread(const CthThread& th) : th_(th), value_(nullptr) {
    new (&cond_) cond_t();
  }

  to_thread(const CthThread& th, const cond_t& cond)
      : th_(th), cond_(cond), value_(nullptr) {}

  value_t value() { return value_; }
  bool stale() override { return th_ == nullptr; }
  bool ready() override { return (bool)value_; }
  bool accepts(const value_t& value) override { return !cond_ || cond_(value); }
  void fulfill(const value_t& value) override {
    CmiAssert(!stale());
    value_ = value;
    if (th_ != CthSelf()) CthAwaken(th_);
    th_ = nullptr;
  }
};
}
}

#endif
