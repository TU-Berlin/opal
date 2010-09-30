
/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/
/* hand-coded interface part of Real */

#include <math.h>

  /* representation */

typedef struct sREAL {
    struct sCELL header;
    double value;
} * REAL;

#define size_real sizeof_small(struct sREAL)
#define alloc_real(r) alloc_small_flat(size_real,r)
#define copy_real(o,n) copy_structured(o,n)
#define free_real(o,n) free_structured(o,n)
#define decr_real(o,n) decr_structured(o,n)
#define excl_real(o,n) excl_structured(o,n)


#define make_real(v,r) {alloc_real(r);((REAL)(r))->value = (v);}

extern OBJ inline_opal_real(OBJ);
extern OBJ inline_c_real(double);

  /* macro based implementations */

#define AReal_AuncheckedAdd(f1,f2,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = ((REAL)f1)->value + ((REAL)f2)->value;\
        free_real(f2,1); \
}
#define AReal_AuncheckedSub(f1,f2,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = ((REAL)f1)->value - ((REAL)f2)->value;\
        free_real(f2,1); \
}
#define AReal_AuncheckedMul(f1,f2,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = ((REAL)f1)->value * ((REAL)f2)->value;\
        free_real(f2,1); \
}
#define AReal_AuncheckedDiv(f1,f2,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = ((REAL)f1)->value / ((REAL)f2)->value;\
        free_real(f2,1); \
}
#define AReal_AuncheckedPow(f1,f2,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = pow(((REAL)f1)->value,((REAL)f2)->value);\
        free_real(f2,1); \
}
#define AReal_Aexp(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = exp(((REAL)f1)->value);\
}
#define AReal_AuncheckedSqrt(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = sqrt(((REAL)f1)->value);\
}
#define AReal_AuncheckedLn(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = log(((REAL)f1)->value);\
}
#define AReal_AuncheckedLog(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = log10(((REAL)f1)->value);\
}
#define AReal_Sm_O1(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = -(((REAL)f1)->value);\
}
#define AReal_Aabs(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = fabs(((REAL)f1)->value);\
}
#define AReal_Aceil(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = ceil(((REAL)f1)->value);\
}
#define AReal_Afloor(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = floor(((REAL)f1)->value);\
}
#define AReal_Apos_(f1,r){\
        r=pack_bool(((REAL)f1)->value > 0.0);\
        free_real(f1,1);\
}
#define AReal_Aneg_(f1,r){\
        r=pack_bool(((REAL)f1)->value < 0.0);\
        free_real(f1,1);\
}
#define AReal_Azero_(f1,r){\
        r=pack_bool(((REAL)f1)->value == 0.0);\
        free_real(f1,1);\
}
#define AReal_Sle(f1,f2,r) {\
	r=pack_bool(((REAL)f1)->value <= ((REAL)f2)->value);\
	free_real(f1,1); free_real(f2,1);\
}
#define AReal_Sl(f1,f2,r) {\
	r=pack_bool(((REAL)f1)->value <  ((REAL)f2)->value);\
	free_real(f1,1); free_real(f2,1);\
}
#define AReal_Sge(f1,f2,r) {\
	r=pack_bool(((REAL)f1)->value >= ((REAL)f2)->value);\
	free_real(f1,1); free_real(f2,1);\
}
#define AReal_Sg(f1,f2,r) {\
	r=pack_bool(((REAL)f1)->value >  ((REAL)f2)->value);\
	free_real(f1,1); free_real(f2,1);\
}
#define AReal_Se(f1,f2,r) {\
	r=pack_bool(((REAL)f1)->value == ((REAL)f2)->value);\
	free_real(f1,1); free_real(f2,1);\
}
#define AReal_SSe(f1,f2,r) {\
	r=pack_bool(((REAL)f1)->value != ((REAL)f2)->value);\
	free_real(f1,1); free_real(f2,1);\
}

#define AReal_Asin(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = sin(((REAL)f1)->value);\
}
#define AReal_Acos(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = cos(((REAL)f1)->value);\
}
#define AReal_Atan(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = tan(((REAL)f1)->value);\
}
#define AReal_AuncheckedArcsin(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = asin(((REAL)f1)->value);\
}
#define AReal_AuncheckedArccos(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = acos(((REAL)f1)->value);\
}
#define AReal_Aarctan(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = atan(((REAL)f1)->value);\
}
#define AReal_Aarctan2(f1,f2,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = atan2( ((REAL)f1)->value , ((REAL)f2)->value );\
        free_real(f2,1); \
}
#define AReal_Asinh(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = sinh(((REAL)f1)->value);\
}
#define AReal_Acosh(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = cosh(((REAL)f1)->value);\
}
#define AReal_Atanh(f1,r){\
        if (excl_real(f1,1)) {r=f1;} else {decr_real(f1,1); alloc_real(r);};\
        ((REAL)r)->value = tanh(((REAL)f1)->value);\
}


