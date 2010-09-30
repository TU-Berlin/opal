/* hand-coded implementation part of SmallReal */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>

/* vgl. Real.hc.c (kd) */
/* extern double strtod(const char*, char **); */

#include "Real.h"

extern OBJ _ASmallReal_Apack(OBJ x1) /* pack */
{OBJ r;
 ASmallReal_Apack(x1,r);
 return r;
}

extern OBJ _ASmallReal_Aunpack(OBJ x1) /* unpack */
{OBJ r;
 ASmallReal_Aunpack(x1,r);
 return r;
}

extern OBJ _ASmallReal_S_Apos_(OBJ x1) /* _pos? */
{OBJ r;
 ASmallReal_S_Apos_(x1,r);
 return r;
}

extern OBJ _ASmallReal_S_Aneg_(OBJ x1) /* _neg? */
{OBJ r;
 ASmallReal_S_Aneg_(x1,r);
 return r;
}

extern OBJ _ASmallReal_S_Azero_(OBJ x1) /* _zero? */
{OBJ r;
 ASmallReal_S_Azero_(x1,r);
 return r;
}

extern OBJ _ASmallReal_S_AasReal(OBJ x1) /* asReal */{
 OBJ r; float f;
 get_sreal(x1,f);
 make_real(f,r);
 return r;
}

extern OBJ _ASmallReal_S_Aadd(OBJ x1,OBJ x2) /* + */
{OBJ r;
 ASmallReal_S_Aadd(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Asub(OBJ x1,OBJ x2) /* - */
{OBJ r;
 ASmallReal_S_Asub(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Amul(OBJ x1,OBJ x2) /* * */
{OBJ r;
 ASmallReal_S_Amul(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Adiv(OBJ x1,OBJ x2) /* / */
{OBJ r;
 ASmallReal_S_Adiv(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Apow(OBJ x1,OBJ x2) /* pow */
{OBJ r;
 ASmallReal_S_Apow(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Amax(OBJ x1,OBJ x2) /* _max */
{OBJ r;
 ASmallReal_S_Amax(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Amin(OBJ x1,OBJ x2) /* _min */
{OBJ r;
 ASmallReal_S_Amin(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Aexp(OBJ x1) /* exp */
{OBJ r;
 ASmallReal_S_Aexp(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Asqrt(OBJ x1) /* sqrt */
{OBJ r;
 ASmallReal_S_Asqrt(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Aln(OBJ x1) /* ln */
{OBJ r;
 ASmallReal_S_Aln(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Alog(OBJ x1) /* log */
{OBJ r;
 ASmallReal_S_Alog(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Aneg(OBJ x1) /* -,1 */
{OBJ r;
 ASmallReal_S_Aneg(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Aabs(OBJ x1) /* abs */
{OBJ r;
 ASmallReal_S_Aabs(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Aceil(OBJ x1) /* ceil */
{OBJ r;
 ASmallReal_S_Aceil(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Afloor(OBJ x1) /* floor */
{OBJ r;
 ASmallReal_S_Afloor(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Ale(OBJ x1,OBJ x2) /* <= */
{OBJ r;
 ASmallReal_S_Ale(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Aeq(OBJ x1,OBJ x2) /* = */
{OBJ r;
 ASmallReal_S_Aeq(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Alt(OBJ x1,OBJ x2) /* < */
{OBJ r;
 ASmallReal_S_Alt(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Asin(OBJ x1) /* sin */
{OBJ r;
 ASmallReal_S_Asin(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Acos(OBJ x1) /* cos */
{OBJ r;
 ASmallReal_S_Acos(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Atan(OBJ x1) /* tan */
{OBJ r;
 ASmallReal_S_Atan(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Aarcsin(OBJ x1) /* arcsin */
{OBJ r;
 ASmallReal_S_Aarcsin(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Aarccos(OBJ x1) /* arccos */
{OBJ r;
 ASmallReal_S_Aarccos(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Aarctan(OBJ x1) /* arctan */
{OBJ r;
 ASmallReal_S_Aarctan(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Aarctan2(OBJ x1,OBJ x2) /* arctan */
{OBJ r;
 ASmallReal_S_Aarctan2(x1,x2,r);
 return r;}

extern OBJ _ASmallReal_S_Asinh(OBJ x1) /* sinh */
{OBJ r;
 ASmallReal_S_Asinh(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Acosh(OBJ x1) /* cosh */
{OBJ r;
 ASmallReal_S_Acosh(x1,r);
 return r;}

extern OBJ _ASmallReal_S_Atanh(OBJ x1) /* tanh */
{OBJ r;
 ASmallReal_S_Atanh(x1,r);
 return r;}

extern OBJ inline_opal_sreal(OBJ d) {
  char *test,buf[64];
  OBJ result; double v;
  get_denotation(d,buf,sizeof(buf));
#ifdef HAVE_STRTOD
  v = strtod(buf, &test);
  if(test == buf)
    HLT("!'SmallReal: no valid sreal denoted");
  if(*test != '\0')   /* scanning endete nicht am Stringende */
    HLT("!'SmallReal: found non permitted character");
#else
  sscanf(buf, "%G", &v);
#endif
  make_sreal(v,result);
  return result;
}

#ifndef __GNUC__
extern OBJ inline_c_sreal(float d){
  OBJ r;
  make_sreal(d,r);
  return r;
}
#endif

static init_const_ASmallReal()
{
 make_sreal(0.0,__ASmallReal_S_A0);
 make_sreal(acos(-1.0),__ASmallReal_S_Api);
 make_sreal(exp(1),__ASmallReal_S_Ae);
 make_sreal(FLT_MAX, __ASmallReal_S_Amaxval);
 make_sreal(FLT_MIN, __ASmallReal_S_Aminval);
 make_sreal(FLT_EPSILON, __ASmallReal_S_Aeps);
}
