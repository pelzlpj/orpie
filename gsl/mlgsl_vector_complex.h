
#include "wrappers.h"

#define BASE_TYPE complex

#define CONV_FLAT

#define TYPE(t) CONCAT2(t,BASE_TYPE)
#define FUNCTION(a,b) CONCAT3(a,BASE_TYPE,b)

#include "mlgsl_vector.h"

#define _DECLARE_COMPLEX_VECTOR(a) gsl_vector_complex v_##a
#define _DECLARE_COMPLEX_VECTOR2(a,b) _DECLARE_COMPLEX_VECTOR(a); _DECLARE_COMPLEX_VECTOR(b)

#define _CONVERT_COMPLEX_VECTOR(a) mlgsl_vec_of_value_complex(&v_##a, a)
#define _CONVERT_COMPLEX_VECTOR2(a,b) _CONVERT_COMPLEX_VECTOR(a); _CONVERT_COMPLEX_VECTOR(b)

#if __GNUC__ >= 3
#define DECLARE_COMPLEX_VECTOR(a) _DECLARE_COMPLEX_VECTOR(a); _CONVERT_COMPLEX_VECTOR(a)
#define DECLARE_COMPLEX_VECTOR2(a,b) DECLARE_COMPLEX_VECTOR(a); DECLARE_COMPLEX_VECTOR(b)
#endif /* __GNUC__ */
