#ifndef ERGOLINE_WORKGROUP_HPP
#define ERGOLINE_WORKGROUP_HPP

#include <hypercomm/tasking/workgroup.hpp>

// NOTE ( this is all rather sloppy... refine! )

namespace ergoline {
int workgroup_size(void) { return CkNumPes(); }

hypercomm::tasking::workgroup_proxy workgroup(void) {
  static hypercomm::tasking::workgroup_proxy proxy(
      hypercomm::tasking::workgroup_proxy::ckNew(workgroup_size()));
  return proxy;
}
}  // namespace ergoline

#endif
