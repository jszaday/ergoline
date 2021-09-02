#ifndef __ERGOLINE_EITHER_HPP__
#define __ERGOLINE_EITHER_HPP__

#include <pup.h>

namespace ergoline {

template <typename L, typename R>
class either {
  bool left_;

  union s_data_ {
    L left_;
    R right_;

    ~s_data_() {}
    s_data_(void) = delete;
    s_data_(PUP::reconstruct) {}
    s_data_(const L& l) : left_(l) {}
    s_data_(const R& r) : right_(r) {}
  } data_;

  friend class hypercomm::puper<either<L, R>>;

 public:
  either(const L& l) : left_(true), data_(l) {}
  either(const R& r) : left_(false), data_(r) {}

  either(const either<L, R>& other)
      : left_(other.left_), data_(PUP::reconstruct{}) {
    if (this->left_) {
      new (&data_) s_data_(other.left());
    } else {
      new (&data_) s_data_(other.right());
    }
  }

  either(PUP::reconstruct tag) : data_(tag) {}

  ~either() {
    if (this->left_) {
      this->data_.left_.~L();
    } else {
      this->data_.right_.~R();
    }
  }

  inline L& left(void) {
    CkAssert(this->isLeft());
    return this->data_.left_;
  }

  inline R& right(void) {
    CkAssert(this->isRight());
    return this->data_.right_;
  }

  inline bool isLeft(void) { return this->left_; }
  inline bool isRight(void) { return !this->isLeft(); }
};

}  // namespace ergoline

namespace hypercomm {
template <typename L, typename R>
struct puper<ergoline::either<L, R>> {
  inline static void impl(serdes& s, ergoline::either<L, R>& t) {
    if (s.unpacking()) {
      reconstruct(&t);
    }

    s | t.left_;

    if (t.isLeft()) {
      s | t.left();
    } else {
      s | t.right();
    }
  }
};
}  // namespace hypercomm

#endif
