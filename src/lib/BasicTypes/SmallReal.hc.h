
/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/
/* hand-coded interface part of SmallReal */

#include <math.h>

  /* representation */

typedef union sSREAL{
    float value;
    WORD  image;
} SREAL;


#define make_sreal(v,r) {SREAL __r; __r.value = v; r=(OBJ)(__r.image);}
#define get_sreal(o,r) {SREAL __r; __r.image = (WORD)(o); r=__r.value;}

#define ASmallReal_Apack(x, r) {r=(OBJ)((WORD)(x) | 0x1);}
#define ASmallReal_Aunpack(x, r) {r=(OBJ)((WORD)(x) - 1);}

extern OBJ inline_opal_sreal(OBJ);

#ifdef __GNUC__
  #define inline_c_sreal(f) \
    ({OBJ _inline_r; make_sreal(f,_inline_r); _inline_r;})
#else
  extern OBJ inline_c_sreal(float); 
#endif

  /* macro based implementations */

#define ASmallReal_S_Aadd(f1,f2,r){\
    float __f1,__f2; get_sreal(f1,__f1); get_sreal(f2,__f2);\
    make_sreal(__f1 + __f2,r);\
}

#define ASmallReal_S_Asub(f1,f2,r){\
    float __f1,__f2; get_sreal(f1,__f1); get_sreal(f2,__f2);\
    make_sreal(__f1 - __f2,r);\
}
#define ASmallReal_S_Amul(f1,f2,r){\
    float __f1,__f2; get_sreal(f1,__f1); get_sreal(f2,__f2);\
    make_sreal(__f1 * __f2,r);\
}
#define ASmallReal_S_Adiv(f1,f2,r){\
    float __f1,__f2; get_sreal(f1,__f1); get_sreal(f2,__f2);\
    make_sreal(__f1 / __f2,r);\
}
#define ASmallReal_S_Apow(f1,f2,r){\
    float __f1,__f2; get_sreal(f1,__f1); get_sreal(f2,__f2);\
    make_sreal(pow(__f1 , __f2),r);\
}
#define ASmallReal_S_Aexp(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(exp(__f1),r);\
}
#define ASmallReal_S_Asqrt(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(sqrt(__f1),r);\
}
#define ASmallReal_S_Aln(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(log(__f1),r);\
}
#define ASmallReal_S_Alog(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(log10(__f1),r);\
}
#define ASmallReal_S_Aneg(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(-(__f1),r);\
}
#define ASmallReal_S_Aabs(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(fabs(__f1),r);\
}
#define ASmallReal_S_Aceil(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(ceil(__f1),r);\
}
#define ASmallReal_S_Afloor(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(floor(__f1),r);\
}
#define ASmallReal_S_Apos_(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    r=pack_bool(__f1 > 0.0);\
}
#define ASmallReal_S_Aneg_(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    r=pack_bool(__f1 < 0.0);\
}
#define ASmallReal_S_Azero_(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    r=pack_bool(__f1 == 0.0);\
}
#define ASmallReal_S_Ale(f1,f2,r) {\
    float __f1, __f2; get_sreal(f1,__f1); get_sreal(f2, __f2);\
    r=pack_bool(__f1 <= __f2);\
}
#define ASmallReal_S_Alt(f1,f2,r) {\
    float __f1, __f2; get_sreal(f1,__f1); get_sreal(f2, __f2);\
    r=pack_bool(__f1 < __f2);\
}
#define ASmallReal_S_Aeq(f1,f2,r) {\
    float __f1, __f2; get_sreal(f1,__f1); get_sreal(f2, __f2);\
    r=pack_bool(__f1 == __f2);\
}

#define ASmallReal_S_Asin(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(sin(__f1),r);\
}
#define ASmallReal_S_Acos(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(cos(__f1),r);\
}
#define ASmallReal_S_Atan(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(tan(__f1),r);\
}
#define ASmallReal_S_Aarcsin(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(asin(__f1),r);\
}
#define ASmallReal_S_Aarccos(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(acos(__f1),r);\
}
#define ASmallReal_S_Aarctan(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(atan(__f1),r);\
}
#define ASmallReal_S_Aarctan2(f1,f2,r){\
    float __f1,__f2; get_sreal(f1,__f1); get_sreal(f2,__f2);\
    make_sreal(atan2(__f1,__f2),r);\
}
#define ASmallReal_S_Asinh(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(sinh(__f1),r);\
}
#define ASmallReal_S_Acosh(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(cosh(__f1),r);\
}
#define ASmallReal_S_Atanh(f1,r){\
    float __f1; get_sreal(f1,__f1);\
    make_sreal(tanh(__f1),r);\
}


