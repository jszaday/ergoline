#ifndef __ERGOLINE_EITHER_HPP__
#define __ERGOLINE_EITHER_HPP__

#include <pup.h>

namespace ergoline {

template <typename L, typename R>
class either;

template <typename L>
class either<L, void> {
  std::unique_ptr<L> data_;

 public:
  template <typename T1, typename T2>
  friend class either;
  friend class hypercomm::puper<either<L, void>>;

  either(void) : data_(nullptr) {}
  either(const L& l) : data_(new L(l)) {}
  either(either<void, L>&& other) : data_(std::move(other.data_)) {}

  template <typename R>
  either(const either<L, R>& other)
      : data_(other.isLeft() ? new L(other.left()) : nullptr) {}

  inline L& left(void) {
    CkAssert(this->isLeft());
    return *(this->data_);
  }

  inline bool isLeft(void) { return (bool)this->data_; }
  inline bool isRight(void) { return !this->isLeft(); }

 private:
  inline void reallocate(void) { this->data_.reset(::new (sizeof(L)) L); }
};

template <typename R>
class either<void, R> {
  std::unique_ptr<R> data_;

 public:
  template <typename T1, typename T2>
  friend class either;
  friend class hypercomm::puper<either<void, R>>;

  either(void) : data_(nullptr) {}
  either(const R& r) : data_(new R(r)) {}
  either(either<void, R>&& other) : data_(std::move(other.data_)) {}

  template <typename L>
  either(const either<L, R>& other)
      : data_(other.isLeft() ? nullptr : new R(other.right())) {}

  inline R& right(void) {
    CkAssert(this->isRight());
    return *(this->data_);
  }

  inline bool isLeft(void) { return !this->isRight(); }
  inline bool isRight(void) { return (bool)this->data_; }

 private:
  inline void reallocate(void) { this->data_.reset(::new (sizeof(R)) R); }
};

template <typename L, typename R>
class either : public either<L, void>, public either<void, R> {
  bool left_;

  friend class hypercomm::puper<either<L, R>>;

 public:
  either(const L& l) : left_(true), either<L, void>(l) {}
  either(const R& r) : left_(false), either<void, R>(r) {}

  either(const either<L, void>& other);
  either(const either<void, R>& other);

  either(const either<L, R>& other) : left_(other.isLeft()) {
    if (this->left_) {
      either<L, void>::data_.reset(new L(other.left()));
    } else {
      either<void, R>::data_.reset(new R(other.right()));
    }
  }

  either(PUP::reconstruct) {}

  inline bool isLeft(void) { return this->left_; }
  inline bool isRight(void) { return !this->isLeft(); }

 private:
  inline void reallocate(void) {
    if (this->isLeft()) {
      either<L, void>::reallocate();
    } else {
      either<void, R>::reallocate();
    }
  }
};

template <typename L, typename R>
either<L, R>::either(const either<L, void>& other)
    : left_(other.isLeft()) {
  if (this->left_) {
    either<L, void>::data_.reset(new L(other.left()));
  } else {
    throw std::invalid_argument("cannot cast void to " +
                                std::string(typeid(R).name()) + "!");
  }
}

template <typename L, typename R>
either<L, R>::either(const either<void, R>& other)
    : left_(other.isLeft()) {
  if (this->left_) {
    either<void, R>::data_.reset(new R(other.right()));
  } else {
    throw std::invalid_argument("cannot cast void to " +
                                std::string(typeid(L).name()) + "!");
  }
}

}  // namespace ergoline

namespace hypercomm {
template <typename L, typename R>
struct puper<ergoline::either<L, R>> {
  inline static void impl(serdes& s, ergoline::either<L, R>& t) {
    if (s.unpacking()) {
      reconstruct(&t);
    }

    s | t.left_;

    if (s.unpacking()) {
      t.reallocate();
    }

    if (t.isLeft()) {
      s | t.left();
    } else {
      s | t.right();
    }
  }
};
}  // namespace hypercomm

#endif
