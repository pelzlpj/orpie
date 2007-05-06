
#include "wrappers.h"

#define BASE_TYPE complex

#define CONV_FLAT

#define TYPE(t) CONCAT2(t,BASE_TYPE)
#define _DECLARE_BASE_TYPE(v) gsl_complex conv_##v
#define _CONVERT_BASE_TYPE(v) GSL_SET_COMPLEX(&conv_##v,Double_field(v, 0), Double_field(v,1))
#define FUNCTION(a,b) CONCAT3(a,BASE_TYPE,b)

#include "mlgsl_matrix.h"

#define _DECLARE_COMPLEX_MATRIX(a) gsl_matrix_complex m_##a
#define _DECLARE_COMPLEX_MATRIX2(a,b) _DECLARE_COMPLEX_MATRIX(a); _DECLARE_COMPLEX_MATRIX(b)

#define _CONVERT_COMPLEX_MATRIX(a) mlgsl_mat_of_value_complex(&m_##a, a)
#define _CONVERT_COMPLEX_MATRIX2(a,b) _CONVERT_COMPLEX_MATRIX(a); _CONVERT_COMPLEX_MATRIX(b)
