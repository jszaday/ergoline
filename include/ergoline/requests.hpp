#ifndef __ERGOLINE_REQUESTS_HPP__
#define __ERGOLINE_REQUESTS_HPP__

#include <map>
#include <deque>
#include <tuple>
#include <memory>
#include <functional>

#include <charm++.h>

/* Design overview:
 * - mailbox unmarshalls incoming message
 * - requests are notified about the new value
 * - requests compare predicate against value
 * - when true -- they proceed
 */

namespace ergoline {

struct request_base_ {
  virtual void cancel() = 0;
};

template <typename... Ts>
struct request : public request_base_,
                 public std::enable_shared_from_this<request<Ts...>> {
  using value_t = std::shared_ptr<std::tuple<Ts...>>;
  using request_t = std::shared_ptr<request<Ts...>>;
  using action_t = std::function<bool(value_t)>;
  using predicate_t = std::function<bool(const value_t&)>;
  using reject_t = std::function<void(void)>;

  request(const action_t& act, const predicate_t& pred)
      : act_(act), pred_(pred), stale_(false) {}

  bool accepts(const value_t& val) { return !stale_ && (!pred_ || pred_(val)); }

  bool notify(value_t val) {
    if (accepts(val) && this->act_(val)) {
      this->cancel();
      return true;
    } else {
      return false;
    }
  }

  inline bool stale(void) const { return stale_; }
  virtual std::pair<reject_t, value_t> query(void) = 0;

  action_t act_;
  predicate_t pred_;
  bool stale_;
};

template <typename... Ts>
struct mailbox {
  using tuple_t = std::tuple<Ts...>;
  using value_t = typename request<Ts...>::value_t;
  using request_t = std::shared_ptr<request<Ts...>>;

  std::list<value_t> values_;
  std::list<request_t> requests_;

  void put(value_t val) {
    for (auto& req : requests_) {
      if (req->notify(val)) {
        return;
      }
    }

    values_.push_back(val);
  }

  request_t make_request(const typename request<Ts...>::action_t& act,
                         const typename request<Ts...>::predicate_t& pred) {
    return std::make_shared<mboxreq>(this, act, pred);
  }

  void put(request_t req) {
    if (req->stale()) return;

    requests_.push_back(req);

    for (auto it = values_.begin(); it != values_.end(); it++) {
      auto& val = *it;

      if (req->notify(val)) {
        values_.erase(it);
        break;
      }
    }
  }

  typename decltype(values_)::iterator query(request_t req) {
    auto it = values_.begin();
    for (; it != values_.end(); it++) {
      auto& val = *it;
      if (req->accepts(val)) {
        break;
      }
    }
    return it;
  }

  void invalidate(request_t req) {
    auto it = std::find(requests_.begin(), requests_.end(), req);
    if (it != requests_.end()) {
      requests_.erase(it);
    }
  }

  void pup(PUP::er& p) {
    CkAssert(requests_.empty());
    hypercomm::interpup(p, values_);
  }

 private:
  struct mboxreq : public request<Ts...> {
    mailbox<Ts...>* src_;

    mboxreq(mailbox<Ts...>* src, const typename request<Ts...>::action_t& act,
            const typename request<Ts...>::predicate_t& pred)
        : src_(src), request<Ts...>(act, pred) {}

    virtual void cancel() override {
      this->stale_ = true;
      src_->invalidate(this->shared_from_this());
    }

    virtual std::pair<typename request<Ts...>::reject_t, value_t> query()
        override {
      auto it = src_->query(this->shared_from_this());
      auto val = (it == src_->values_.end()) ? nullptr : *it;
      if (it != src_->values_.end()) src_->values_.erase(it);
      return std::make_pair([&]() { src_->values_.push_back(val); }, val);
    }
  };
};

template <typename T, typename U>
struct compound_request;

template <typename... Ts, typename... Us>
struct compound_request<request<Ts...>, request<Us...>>
    : public request<Ts..., Us...> {
  using value_t = typename request<Ts..., Us...>::value_t;
  using lreq_t = std::shared_ptr<request<Ts...>>;
  using rreq_t = std::shared_ptr<request<Us...>>;
  using pair_t = std::pair<lreq_t, rreq_t>;

  pair_t requests_;

  template <typename Concatenator, typename... As, typename... Bs>
  void set_action(std::shared_ptr<request<As...>>& a,
                  std::shared_ptr<request<Bs...>>& b,
                  const Concatenator& concat) {
    a->act_ = [&](typename request<As...>::value_t val) {
      auto res = b->query();
      if (res.second) {
        if (this->notify(concat(*val, *res.second))) {
          return true;
        }

        res.first();
      }
      return false;
    };
  }

  inline value_t make_value(std::tuple<Ts...>& ts, std::tuple<Us...>& us) {
    return std::make_shared<std::tuple<Ts..., Us...>>(std::tuple_cat(ts, us));
  }

  compound_request(const pair_t& reqs,
                   const typename request<Ts..., Us...>::action_t& act,
                   const typename request<Ts..., Us...>::predicate_t& pred)
      : requests_(reqs), request<Ts..., Us...>(act, pred) {
    set_action(requests_.first, requests_.second,
               [&](std::tuple<Ts...>& ts, std::tuple<Us...>& us) {
                 return make_value(ts, us);
               });
    set_action(requests_.second, requests_.first,
               [&](std::tuple<Us...>& us, std::tuple<Ts...>& ts) {
                 return make_value(ts, us);
               });
  }

  inline lreq_t left() const { return requests_.first; }
  inline rreq_t right() const { return requests_.second; }

  virtual void cancel() override {
    left()->cancel();
    right()->cancel();

    this->stale_ = true;
  }

  virtual std::pair<typename request<Ts...>::reject_t, value_t> query()
      override {
    auto l = left()->query(), r = right()->query();
    return std::make_pair([&]() {
      l.first();
      r.first();
    }, (!l.second || !r.second) ? nullptr : make_value(*l.second, *r.second));
  }
};

template <typename Action, typename Predicate, typename... As, typename... Bs>
std::shared_ptr<compound_request<request<As...>, request<Bs...>>> join(
    const std::shared_ptr<request<As...>>& a,
    const std::shared_ptr<request<Bs...>>& b, const Action& action,
    const Predicate& predicate) {
  return std::make_shared<compound_request<request<As...>, request<Bs...>>>(
      std::make_pair(a, b), action, predicate);
}

struct reqman {
  using done_fn = std::function<void(void)>;

  reqman(bool all): all_(all), sleeper_(nullptr) {}

  template <typename... Ts>
  void put(const std::shared_ptr<request<Ts...>>& req,
           const std::function<void(typename request<Ts...>::value_t)>& fn) {
    req->act_ = [=](typename request<Ts...>::value_t value) {
      if (!requests_.empty()) {
        if (all_) {
          const auto search = std::find(requests_.begin(), requests_.end(), req);
          CkAssert(search != requests_.end());
          requests_.erase(search);
        } else {
          for (auto it = requests_.begin(); it != requests_.end();) {            
            (*it)->cancel();

            it = requests_.erase(it);
          }
        }

        fn(value);

        if (sleeper_ && requests_.empty()) {
          CthAwaken(sleeper_);
        }

        return true;
      } else {
        return false;
      }
    };

    requests_.push_back(req);
  }

  void block(void) {
    CkAssert(sleeper_ == nullptr);

    sleeper_ = CthSelf();
    while (!requests_.empty()) {
      CthSuspend();
    }
    sleeper_ = nullptr;
  }

 private:
  bool all_;
  CthThread sleeper_;

  std::vector<std::function<void(void)>> actions_;
  std::vector<std::shared_ptr<request_base_>> requests_;
};
}

#endif
