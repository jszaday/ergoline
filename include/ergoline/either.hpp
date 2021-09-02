#ifndef __ERGOLINE_EITHER_HPP__
#define __ERGOLINE_EITHER_HPP__

namespace ergoline {

template <typename L, typename R>
class either {
  bool left_;

 public:
  either(const L& l) : left_(true) {}
  either(const R& r) : left_(false) {}

  L& left(void) {}
  R& right(void) {}

  inline bool isLeft(void) { return this->left_; }
  inline bool isRight(void) { return !this->isLeft(); }
};

}  // namespace ergoline

#endif
