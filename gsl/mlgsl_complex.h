
#include <gsl/gsl_complex.h>

#include "wrappers.h"

static inline value 
#ifndef FLOAT_COMPLEX
copy_complex(gsl_complex * c)
#else
copy_complex(gsl_complex_float * c)
#endif /* FLOAT_COMPLEX */
{
  return copy_two_double_arr(GSL_COMPLEX_P_REAL(c), GSL_COMPLEX_P_IMAG(c));
}

#ifndef FLOAT_COMPLEX
#define _DECLARE_COMPLEX(v) gsl_complex z_##v
#else
#define _DECLARE_COMPLEX(v) gsl_complex_float z_##v
#endif /* FLOAT_COMPLEX */
#define _DECLARE_COMPLEX2(v1,v2) _DECLARE_COMPLEX(v1); _DECLARE_COMPLEX(v2)
#define _DECLARE_COMPLEX3(v1,v2,v3) _DECLARE_COMPLEX2(v1,v2); _DECLARE_COMPLEX(v3)

#define _CONVERT_COMPLEX(v) GSL_SET_COMPLEX(&z_##v,Double_field(v, 0), Double_field(v,1))
#define _CONVERT_COMPLEX2(v1,v2) _CONVERT_COMPLEX(v1); _CONVERT_COMPLEX(v2)

#if __GNUC__ >= 3
#define DECLARE_COMPLEX(v) _DECLARE_COMPLEX(v); _CONVERT_COMPLEX(v)
#define DECLARE_COMPLEX2(v1,v2) DECLARE_COMPLEX(v1); DECLARE_COMPLEX(v2)
#endif /* __GNUC__ */
