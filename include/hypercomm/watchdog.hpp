#ifndef __HYPERCOMM_WATCHDOG_HPP__
#define __HYPERCOMM_WATCHDOG_HPP__

#include <ck.h>
#include <atomic>

namespace hypercomm {

namespace {
using watchdog_id_t = std::size_t;
using watchdog_map_t = std::map<watchdog_id_t, void*>;

CkpvDeclare(watchdog_id_t, last_watchdog_id_);
CkpvDeclare(watchdog_map_t, watchdog_map_);
}

class watchdog {
  std::atomic<double> last;
  watchdog_id_t idx;
  double timeout;

  static void _trigger(void* arg) {
    auto idx = reinterpret_cast<watchdog_id_t>(arg);
    auto map = &(CkpvAccess(watchdog_map_));
    auto search = map->find(idx);

    if (search != map->end()) {
      auto dog = static_cast<watchdog*>(search->second);
      auto duration = CkWallTimer() - (dog->last).load();

      if (duration >= dog->timeout) {
        dog->bark(duration);
      } else {
        dog->_next_period();
      }
    }
  }

  void _next_period(void) const {
    CcdCallFnAfter((CcdVoidFn)_trigger, reinterpret_cast<void*>(this->idx),
                   this->timeout);
  }

 public:
  watchdog(const double& _1) : timeout(_1) {
    if (!(CkpvInitialized(last_watchdog_id_) &&
          CkpvInitialized(watchdog_map_))) {
      CkpvInitialize(watchdog_id_t, last_watchdog_id_);
      CkpvInitialize(watchdog_map_t, watchdog_map_);
    }

    this->idx = (CkpvAccess(last_watchdog_id_))++;
    (CkpvAccess(watchdog_map_))[this->idx] = this;

    this->_next_period();
  }

  ~watchdog() { (CkpvAccess(watchdog_map_)).erase(this->idx); }

  void pet(void) { last = CkWallTimer(); }

  void bark(const double& overtime) {
    CkAbort("woof woof! watchdog hasn't been pet in %lf s", overtime);
  }
};
}

#endif
