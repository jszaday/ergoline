#ifndef __ERGOLINE_BLAS_HPP__
#define __ERGOLINE_BLAS_HPP__

#if defined __has_include
#if __has_include(<gsl / gsl_blas.h>)
#include <gsl/gsl_blas.h>
#define __ERGOLINE_USE_GSL__
#endif
#endif

#include <ergoline/array.hpp>

namespace ergoline {
namespace blas {
inline int dgemm(const double& alpha, const ergoline::array<double, 2>& a,
          const ergoline::array<double, 2>& b, const double& beta,
          ergoline::array<double, 2>& c);

#if defined __ERGOLINE_USE_GSL__
namespace {
template <typename T>
inline gsl_matrix_view erg2gsl(const ergoline::array<T, 2>& a) {
  return gsl_matrix_view_array(const_cast<double*>(a.buffer),
                               std::get<1>(a.shape), std::get<0>(a.shape));
}
}

inline int dgemm(const double& alpha, const ergoline::array<double, 2>& a,
          const ergoline::array<double, 2>& b, const double& beta,
          ergoline::array<double, 2>& c) {
  auto A = erg2gsl(a), B = erg2gsl(b), C = erg2gsl(c);
  return gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, alpha, &B.matrix, &A.matrix,
                        beta, &C.matrix);
}
#else

#warning \
    "Ergoline could not find a suitable BLAS target (e.g. GSL), performance may suffer."

inline int dgemm(const double& alpha, const ergoline::array<double, 2>& a,
                 const ergoline::array<double, 2>& b, const double& beta,
                 ergoline::array<double, 2>& c) {
  const auto& m = std::get<0>(a.shape);
  const auto& k = std::get<1>(a.shape);
  CkAssert(k == std::get<0>(b.shape) && "matrix dims must match");
  const auto& n = std::get<1>(b.shape);
  CkAssert(m == std::get<0>(c.shape) && n == std::get<1>(c.shape) &&
           "matrix dims must match");

  const double* __restrict a_ = a.buffer;
  const double* __restrict b_ = b.buffer;
  double* __restrict c_ = c.buffer;

  for (int j = 0; (j < n); j += 1) {
    for (int i = 0; (i < m); i += 1) {
      auto sum = 0;
      for (int l = 0; (l < k); l += 1) {
        sum += a_[i + l * m] * b_[j * k + l];
      }
      c_[i + j * m] = alpha * sum + beta * c_[i + j * m];
    }
  }

  return 0;
}

#endif
}
}

#endif
