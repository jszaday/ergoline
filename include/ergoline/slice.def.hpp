#ifndef ERGOLINE_SLICE_DEF_HPP
#define ERGOLINE_SLICE_DEF_HPP

#include "iterator.def.hpp"
#include "slice.decl.hpp"

namespace ergoline {
struct slice;

std::shared_ptr<iterator<int>> make_slice_iterator(const slice &self) {
  if (self.end) {
    auto start = self.start ? *(self.start) : 0;
    auto &end = *(self.end);
    auto step = self.step ? *(self.step) : ((start > end) ? -1 : 1);
    return std::make_shared<range<int>::range_iterator>(
        range<int>(start, step, end));
  } else {
    return nullptr;
  }
}

template <typename T> struct array_view_iterator : public iterator<T> {
  T *current;
  int step;
  std::uintptr_t end;

  array_view_iterator(T *begin_, int step_, T *end_)
      : current(begin_), step(step_), end((std::uintptr_t)end_) {}

  virtual T next(void) override {
    auto &value = *(this->current);
    current += step;
    return value;
  }

  virtual bool hasNext(void) override { return (std::uintptr_t)current < end; }

  virtual std::shared_ptr<object_base_> __this_object__(void) override {
    return std::shared_ptr<ergoline::object_base_>(
        std::shared_ptr<ergoline::object_base_>{}, this);
  }
};

template <typename T>
std::shared_ptr<iterator<T>> array_view<T, 2>::iter(void) const {
  return std::make_shared<array_view_iterator<T>>(this->start, this->step,
                                                  this->stop);
}

// TODO ( eliminate the need for this! )
template <typename T>
struct iterator_accessor<std::shared_ptr<array_view<T, 2>>> {
  std::shared_ptr<iterator<T>>
  operator()(const std::shared_ptr<array_view<T, 2>> &t) const {
    return t->iter();
  }
};

template <typename T, std::size_t N, typename... Args>
struct array_view_initializer;

template <typename T>
struct array_view_initializer<T, 2, std::tuple<ergoline::slice, int>> {
  void operator()(array_view<T, 2> *self,
                  const std::shared_ptr<nd_span<T, 2>> &array,
                  std::tuple<ergoline::slice, int> ij) {
    auto &i = std::get<0>(ij);
    auto &j = std::get<1>(ij);
    if (i.start || i.end || i.step) {
      CkAbort("not yet implemented");
    } else {
      self->start = array->begin() + j;
      self->stop = array->begin() + (array->shape[0] * array->shape[1]);
      self->step = array->shape[1];
    }
  }
};

template <typename T>
struct array_view_initializer<T, 2, std::tuple<int, ergoline::slice>> {
  void operator()(array_view<T, 2> *self,
                  const std::shared_ptr<nd_span<T, 2>> &array,
                  std::tuple<int, ergoline::slice> ij) {
    auto &i = std::get<0>(ij);
    auto &j = std::get<1>(ij);
    if (j.start || j.end || j.step) {
      CkAbort("not yet implemented");
    } else {
      self->start = array->begin() + i * array->shape[1];
      self->stop = array->begin() + (i + 1) * array->shape[1];
      self->step = 1;
    }
  }
};

template <typename T>
template <typename... Args>
array_view<T, 2>::array_view(const std::shared_ptr<nd_span<T, 2>> &array,
                             Args... args) {
  array_view_initializer<T, 2, Args...>()(this, array,
                                          std::forward<Args>(args)...);
}

template <typename T> void array_view<T, 2>::set(const T &t) {
  auto *curr = this->start;
  auto stop = (std::uintptr_t)this->stop;

  while ((std::uintptr_t)curr < stop) {
    *curr = t;
    curr += this->step;
  }
}

template <typename T>
template <typename It>
void array_view<T, 2>::set(const std::shared_ptr<It> &it) {
  auto *curr = this->start;
  auto stop = (std::uintptr_t)this->stop;
  auto theirs = access_iterator(it);
#if CMK_DEBUG
  auto count = 0;
#endif

  while (((std::uintptr_t)curr < stop) && theirs->hasNext()) {
    *curr = theirs->next();
    curr += this->step;
#if CMK_DEBUG
    count += 1;
#endif
  }

#if CMK_DEBUG
  CkPrintf("\nset the value of %d elements\n", count);
#endif
}
} // namespace ergoline

#endif
