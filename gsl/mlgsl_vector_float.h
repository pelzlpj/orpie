#include "wrappers.h"

#define BASE_TYPE float

#undef CONV_FLAT

#define TYPE(t) CONCAT2(t,BASE_TYPE)
#define FUNCTION(a,b) CONCAT3(a,BASE_TYPE,b)

#include "mlgsl_vector.h"
