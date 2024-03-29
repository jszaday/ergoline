#ifndef __ERGOLINE_BLAS_HPP__
#define __ERGOLINE_BLAS_HPP__

#if defined __has_include
#if __has_include(<gsl/gsl_blas.h>)
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
  return gsl_matrix_view_array(a.data(), std::get<0>(a.shape), std::get<1>(a.shape));
}
}  // namespace

inline int dgemm(const double& alpha, const ergoline::array<double, 2>& a,
                 const ergoline::array<double, 2>& b, const double& beta,
                 ergoline::array<double, 2>& c) {
  auto A = erg2gsl(a), B = erg2gsl(b), C = erg2gsl(c);
  return gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, alpha, &A.matrix, &B.matrix,
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

  const double* __restrict A = a.data();
  const double* __restrict B = b.data();
  double* __restrict C = c.data();

  for (int i = 0; (i < m); i += 1) {
    for (int j = 0; (j < n); j += 1) {
      auto sum = 0;
      for (int l = 0; (l < k); l += 1) {
        sum += A[i * k + l] * B[l * n + j];
      }
      C[i * n + j] = alpha * sum + beta * C[i * n + j];
    }
  }

  return 0;
}

#endif
}  // namespace blas
}  // namespace ergoline

#endif
