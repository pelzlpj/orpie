
#define BASE_TYPE double

#define CONV_FLAT

#define TYPE(t) t
#define _DECLARE_BASE_TYPE(v) double conv_##v
#define _CONVERT_BASE_TYPE(v) conv_##v = Double_val(v)
#if __GNUC__ >= 3
#define DECLARE_BASE_TYPE(v) _DECLARE_BASE_TYPE(v); _CONVERT_BASE_TYPE(v)
#endif /* __GNUC__ */
#define FUNCTION(a,b) a ## _ ## b

#include "mlgsl_matrix.h"
