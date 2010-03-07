
#include "wrappers.h"

#define BASE_TYPE float

#undef CONV_FLAT

#define TYPE(t) CONCAT2(t,BASE_TYPE)
#define _DECLARE_BASE_TYPE(v) double conv_##v
#define _CONVERT_BASE_TYPE(v) conv_##v = Double_val(v)
#define FUNCTION(a,b) CONCAT3(a,BASE_TYPE,b)

#include "mlgsl_matrix.h"
