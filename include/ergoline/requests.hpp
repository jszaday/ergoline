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
  virtual void cancel(void) = 0;
};

template <typename... Ts>
struct request : public request_base_ {
  using value_t = std::shared_ptr<std::tuple<Ts...>>;
  using request_t = std::shared_ptr<request<Ts...>>;
  using action_t = std::function<bool(value_t&)>;
  using predicate_t = std::function<bool(const value_t&)>;

  request(const action_t& act, const predicate_t& pred)
      : act_(act), pred_(pred), stale_(false) {}

  virtual bool accepts(const value_t& val) const {
    return !stale_ && (!pred_ || pred_(val));
  }

  bool notify(value_t& val) {
    if (accepts(val) && this->act_(val)) {
      this->cancel();
      return true;
    } else {
      return false;
    }
  }

  virtual value_t query(void) = 0;
  virtual void reject(value_t&& val) = 0;
  inline bool stale(void) const { return stale_; }

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

  void put(value_t&& val) {
    for (auto& req : requests_) {
      if (req->notify(val)) {
        return;
      }
    }

    values_.emplace_back(std::forward<value_t>(val));
  }

  request_t make_request(const typename request<Ts...>::action_t& act,
                         const typename request<Ts...>::predicate_t& pred) {
    return std::make_shared<mboxreq>(this, act, pred);
  }

  void put(const request_t& req) {
    if (req->stale()) return;

    for (auto it = values_.begin(); it != values_.end(); it++) {
      if (req->notify(*it)) {
        values_.erase(it);
        return;
      }
    }

    std::static_pointer_cast<mboxreq>(req)->inserted_ = true;
    requests_.push_back(req);
  }

  value_t query(const request<Ts...>* req) {
    for (auto it = values_.begin(); it != values_.end(); it++) {
      if (req->accepts(*it)) {
        auto val = *it;
        values_.erase(it);
        return val;
      }
    }
    return nullptr;
  }

  void invalidate(const request<Ts...>* theirs) {
    auto it = std::find_if(
        requests_.begin(), requests_.end(),
        [&](const request_t& ours) { return ours.get() == theirs; });
    CkAssert(it != requests_.end());
    requests_.erase(it);
  }

  void pup(PUP::er& p) {
    CkAssert(requests_.empty());
    hypercomm::interpup(p, values_);
  }

 private:
  struct mboxreq : public request<Ts...> {
    mailbox<Ts...>* src_;
    bool inserted_;

    mboxreq(mailbox<Ts...>* src, const typename request<Ts...>::action_t& act,
            const typename request<Ts...>::predicate_t& pred)
        : src_(src), request<Ts...>(act, pred), inserted_(false) {}

    virtual void cancel(void) override {
      // if this request was put into a recv queue
      // and wasn't already cancelled
      if (this->inserted_ && !this->stale_) {
        // then remove it from the recv queue
        src_->invalidate(this);
      }

      this->stale_ = true;
    }

    virtual value_t query(void) override { return src_->query(this); }

    virtual void reject(value_t&& val) override {
      src_->values_.emplace_back(std::forward<value_t>(val));
    }
  };
};

template <typename T, typename U>
struct compound_request;

template <typename... Ts, typename... Us>
struct compound_request<request<Ts...>, request<Us...>>
    : public request<Ts..., Us...> {
  using parent_t = request<Ts..., Us...>;
  using value_t = typename parent_t::value_t;
  using lreq_t = std::shared_ptr<request<Ts...>>;
  using rreq_t = std::shared_ptr<request<Us...>>;
  using pair_t = std::pair<lreq_t, rreq_t>;

  pair_t requests_;

  template <typename Concatenator, typename... As, typename... Bs>
  void set_action(std::shared_ptr<request<As...>>& a,
                  std::shared_ptr<request<Bs...>>& b,
                  const Concatenator& concat) {
    a->act_ = [=](typename request<As...>::value_t& val) {
      auto res = b->query();
      if (res) {
        auto copy = concat(*val, *res);
        if (this->notify(copy)) {
          return true;
        } else {
          b->reject(res);
        }
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

  inline lreq_t& left(void) { return requests_.first; }
  inline rreq_t& right(void) { return requests_.second; }

  virtual bool accepts(const value_t& val) const override {
    // auto& ts = *(reinterpret_cast<std::tuple<Ts...>*>(val.get()));
    // auto& us =
    // *(reinterpret_cast<std::tuple<Us...>*>(&std::get<sizeof...(Ts)>(*val)));
    // TODO implement this
    return true;
  }

  virtual void cancel(void) override {
    left()->cancel();
    right()->cancel();

    this->stale_ = true;
  }

  virtual value_t query(void) override {
    auto l = left()->query();
    auto r = right()->query();

    if (l != nullptr && r == nullptr) {
      left()->reject(std::move(l));
      return nullptr;
    } else if (l == nullptr && r != nullptr) {
      right()->reject(std::move(r));
      return nullptr;
    } else {
      return make_value(*l, *r);
    }
  }

  virtual void reject(value_t&& val) override {
    auto ts = reinterpret_cast<std::tuple<Ts...>*>(val.get());
    auto us =
        reinterpret_cast<std::tuple<Us...>*>(&std::get<sizeof...(Ts)>(*val));
    auto l = std::shared_ptr<std::tuple<Ts...>>(val, ts);
    left()->reject(std::move(l));
    auto r = std::shared_ptr<std::tuple<Us...>>(val, us);
    right()->reject(std::move(r));
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

  reqman(bool all) : all_(all), sleeper_(nullptr) {}

  template <typename... Ts>
  void put(const std::shared_ptr<request<Ts...>>& req,
           const std::function<void(typename request<Ts...>::value_t&&)>& fn) {
    req->act_ = [this, req, fn](typename request<Ts...>::value_t& value) {
      if (!requests_.empty()) {
        if (all_) {
          const auto search =
              std::find(requests_.begin(), requests_.end(), req);
          CkAssert(search != requests_.end());
          requests_.erase(search);
        } else {
          for (auto it = requests_.begin(); it != requests_.end();) {
            (*it)->cancel();

            it = requests_.erase(it);
          }
        }

        fn(std::move(value));

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
