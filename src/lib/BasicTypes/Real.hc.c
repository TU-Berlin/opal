/* hand-coded implementation part of Real */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include <unixconfig.h>

/* sieht sehr überflüssig aus (kd) (evtl. bei bestimtmen Architekturen? */
/* extern double strtod(const char*, char **); */



extern OBJ _AReal_AuncheckedAdd(OBJ x1,OBJ x2) /* + */
{OBJ r;
 AReal_AuncheckedAdd(x1,x2,r);
 return r;}

extern OBJ _AReal_AuncheckedSub(OBJ x1,OBJ x2) /* - */
{OBJ r;
 AReal_AuncheckedSub(x1,x2,r);
 return r;}

extern OBJ _AReal_AuncheckedMul(OBJ x1,OBJ x2) /* * */
{OBJ r;
 AReal_AuncheckedMul(x1,x2,r);
 return r;}

extern OBJ _AReal_AuncheckedDiv(OBJ x1,OBJ x2) /* / */
{OBJ r;
 AReal_AuncheckedDiv(x1,x2,r);
 return r;}

extern OBJ _AReal_AuncheckedPow(OBJ x1,OBJ x2) /* pow */
{OBJ r;
 AReal_AuncheckedPow(x1,x2,r);
 return r;}

extern OBJ _AReal_Aexp(OBJ x1) /* exp */
{OBJ r;
 AReal_Aexp(x1,r);
 return r;}

extern OBJ _AReal_AuncheckedSqrt(OBJ x1) /* sqrt */
{OBJ r;
 AReal_AuncheckedSqrt(x1,r);
 return r;}

extern OBJ _AReal_AuncheckedLn(OBJ x1) /* ln */
{OBJ r;
 AReal_AuncheckedLn(x1,r);
 return r;}

extern OBJ _AReal_AuncheckedLog(OBJ x1) /* log */
{OBJ r;
 AReal_AuncheckedLog(x1,r);
 return r;}

extern OBJ _AReal_Sm_O1(OBJ x1) /* -,1 */
{OBJ r;
 AReal_Sm_O1(x1,r);
 return r;}

extern OBJ _AReal_Aabs(OBJ x1) /* abs */
{OBJ r;
 AReal_Aabs(x1,r);
 return r;}

extern OBJ _AReal_Aceil(OBJ x1) /* ceil */
{OBJ r;
 AReal_Aceil(x1,r);
 return r;}

extern OBJ _AReal_Afloor(OBJ x1) /* floor */
{OBJ r;
 AReal_Afloor(x1,r);
 return r;}

extern OBJ _AReal_Sle(OBJ x1,OBJ x2) /* <= */
{OBJ r;
 AReal_Sle(x1,x2,r);
 return r;}

extern OBJ _AReal_Se(OBJ x1,OBJ x2) /* = */
{OBJ r;
 AReal_Se(x1,x2,r);
 return r;}

extern OBJ _AReal_Sge(OBJ x1,OBJ x2) /* >= */
{OBJ r;
 AReal_Sge(x1,x2,r);
 return r;}

extern OBJ _AReal_Sl(OBJ x1,OBJ x2) /* < */
{OBJ r;
 AReal_Sl(x1,x2,r);
 return r;}

extern OBJ _AReal_Sg(OBJ x1,OBJ x2) /* > */
{OBJ r;
 AReal_Sg(x1,x2,r);
 return r;}

extern OBJ _AReal_SSe(OBJ x1,OBJ x2) /* |= */
{OBJ r;
 AReal_SSe(x1,x2,r);
 return r;}

extern OBJ _AReal_Asin(OBJ x1) /* sin */
{OBJ r;
 AReal_Asin(x1,r);
 return r;}

extern OBJ _AReal_Acos(OBJ x1) /* cos */
{OBJ r;
 AReal_Acos(x1,r);
 return r;}

extern OBJ _AReal_Atan(OBJ x1) /* tan */
{OBJ r;
 AReal_Atan(x1,r);
 return r;}

extern OBJ _AReal_AuncheckedArcsin(OBJ x1) /* arcsin */
{OBJ r;
 AReal_AuncheckedArcsin(x1,r);
 return r;}

extern OBJ _AReal_AuncheckedArccos(OBJ x1) /* arccos */
{OBJ r;
 AReal_AuncheckedArccos(x1,r);
 return r;}

extern OBJ _AReal_Aarctan(OBJ x1) /* arctan */
{OBJ r;
 AReal_Aarctan(x1,r);
 return r;}

extern OBJ _AReal_Aarctan2(OBJ x1, OBJ x2) /* arctan2 */
{OBJ r;
 AReal_Aarctan2(x1,x2,r);
 return r;}

extern OBJ _AReal_Asinh(OBJ x1) /* sinh */
{OBJ r;
 AReal_Asinh(x1,r);
 return r;}

extern OBJ _AReal_Acosh(OBJ x1) /* cosh */
{OBJ r;
 AReal_Acosh(x1,r);
 return r;}

extern OBJ _AReal_Atanh(OBJ x1) /* tanh */
{OBJ r;
 AReal_Atanh(x1,r);
 return r;}

extern OBJ inline_opal_real(OBJ d) {
  char *test,buf[64];
  OBJ result; double v;
  get_denotation(d,buf,sizeof(buf));
#ifdef HAVE_STRTOD
  v = strtod(buf, &test);
  if(test == buf)
    HLT("!'Real: no valid real denoted");
  if(*test != '\0')   /* scanning endete nicht am Stringende */
    HLT("!'Real: found non permitted character");
#else
  sscanf(buf,"%G", &v);
#endif
  make_real(v,result);
  return result;
}

extern OBJ inline_c_real(double d){
  OBJ r;
  make_real(d,r);
  return r;
}

static init_const_AReal()
{
 make_real(0.0,__AReal_A0);
 make_real(acos(-1.0),__AReal_Api);
 make_real(exp(1),__AReal_Ae);
 make_real(DBL_MAX, __AReal_Amax);
 make_real(DBL_MIN, __AReal_Amin);
 make_real(DBL_EPSILON, __AReal_Aeps);
}
