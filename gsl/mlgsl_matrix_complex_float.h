
#include "wrappers.h"

#define BASE_TYPE complex_float

#undef CONV_FLAT

#define TYPE(t) CONCAT2(t,BASE_TYPE)
#define _DECLARE_BASE_TYPE(v) gsl_complex_float conv_##v
#define _CONVERT_BASE_TYPE(v) GSL_SET_COMPLEX(&conv_##v,Double_field(v, 0), Double_field(v,1))
#define FUNCTION(a,b) CONCAT3(a,BASE_TYPE,b)

#include "mlgsl_matrix.h"
