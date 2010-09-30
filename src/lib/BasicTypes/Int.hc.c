/* hand-coded implementation part of Int */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
extern OBJ _AInt_Sm_O1(OBJ x1) /* -,1 */
{OBJ r;
 AInt_Sm_O1(x1,r);
 return r;}

extern OBJ _AInt_Aabs(OBJ x1) /* abs */
{OBJ r;
 AInt_Aabs(x1,r);
 return r;}

extern OBJ _AInt_Asign(OBJ x1) /* sign */
{OBJ r;
 AInt_Asign(x1,r);
 return r;}

extern OBJ _AInt_Aeven_(OBJ x1) /* even? */
{OBJ r;
 AInt_Aeven_(x1,r);
 return r;}

extern OBJ _AInt_Aodd_(OBJ x1) /* odd? */
{OBJ r;
 AInt_Aodd_(x1,r);
 return r;}

extern OBJ _AInt_Apos_(OBJ x1) /* pos? */
{OBJ r;
 AInt_Apos_(x1,r);
 return r;}

extern OBJ _AInt_Aneg_(OBJ x1) /* neg? */
{OBJ r;
 AInt_Aneg_(x1,r);
 return r;}

extern OBJ _AInt_Azero_(OBJ x1) /* zero? */
{OBJ r;
 AInt_Azero_(x1,r);
 return r;}

extern OBJ _AInt_Sle(OBJ x1,OBJ x2) /* <= */
{OBJ r;
 AInt_Sle(x1,x2,r);
 return r;}

extern OBJ _AInt_Se(OBJ x1,OBJ x2) /* = */
{OBJ r;
 AInt_Se(x1,x2,r);
 return r;}

extern OBJ _AInt_Sge(OBJ x1,OBJ x2) /* >= */
{OBJ r;
 AInt_Sge(x1,x2,r);
 return r;}

extern OBJ _AInt_Sl(OBJ x1,OBJ x2) /* < */
{OBJ r;
 AInt_Sl(x1,x2,r);
 return r;}

extern OBJ _AInt_Sg(OBJ x1,OBJ x2) /* > */
{OBJ r;
 AInt_Sg(x1,x2,r);
 return r;}

extern OBJ _AInt_SSe(OBJ x1,OBJ x2) /* |= */
{OBJ r;
 AInt_SSe(x1,x2,r);
 return r;}

extern OBJ _AInt_AuncheckedSucc(OBJ x1) /* uncheckedSucc */
{OBJ r;
 AInt_AuncheckedSucc(x1,r);
 return r;}

extern OBJ _AInt_AuncheckedPred(OBJ x1) /* uncheckedPred */
{OBJ r;
 AInt_AuncheckedPred(x1,r);
 return r;}

extern OBJ _AInt_AuncheckedAdd(OBJ x1,OBJ x2) /* uncheckedAdd */
{OBJ r;
 AInt_AuncheckedAdd(x1,x2,r);
 return r;}

extern OBJ _AInt_AuncheckedSub(OBJ x1,OBJ x2) /* uncheckedSub */
{OBJ r;
 AInt_AuncheckedSub(x1,x2,r);
 return r;}

extern OBJ _AInt_AuncheckedMul(OBJ x1,OBJ x2) /* uncheckedMul */
{OBJ r;
 AInt_AuncheckedMul(x1,x2,r);
 return r;}

extern OBJ _AInt_AuncheckedDiv(OBJ x1,OBJ x2) /* uncheckedDiv */
{OBJ r;
 AInt_AuncheckedDiv(x1,x2,r);
 return r;}

extern OBJ _AInt_AuncheckedMod(OBJ x1,OBJ x2) /* uncheckedMod */
{OBJ r;
 AInt_AuncheckedMod(x1,x2,r);
 return r;}

extern OBJ _AInt_Apow(OBJ x1,OBJ x2) /* pow */
{INT x,y,r,sign, rsign = 1;

 x = unpack_int(x1);
 y = unpack_int(x2);
 if (y == 0) {
   r = 1;}
 else if (y < 0) {
   HLT("^'Int: cannot raise integer to a negative power");
 }
 else if (x == 0) { 
   r = 0;
 }
 else { 
   if (x > 0) {sign = 1;} else {sign = -1; x = -x;}
   for(r = 1, rsign = 1; y > 0; y--) {
     if (max_sword / x < r) {
       HLT("^'Int: result too large");
     };
     r *= x; rsign *= sign;
   }
 };
 r *= rsign;
 return pack_int(r);
}

extern OBJ _AInt_S1(OBJ d) /* ! */ {
    int n = 0, i = 0, l = leng_denotation(d);
    int value, vorzeichen = 1;

    value = data_denotation(d)[i];
    if(value == '-')
      { vorzeichen = -1;
        i++;
      };
    if (l == i)
      HLT("!'Int:denotation->int: no digits in denotation");
    while (i < l){
        value = data_denotation(d)[i++];
        if(value < '0' || value > '9')
           HLT("!'Int:denotation -> int: found non digit character");
        value -= '0';
        if(vorzeichen == 1)
          { if((max_sword - value)/10 < n)
              HLT("!'Int:denotation -> int: constant too large");
            n = n*10 + value;
          }
        else /* vorzeichen == -1 */
          { if((min_sword + value)/10 > n)
              HLT("!'Int:denotation -> int: constant too small");
            n = n*10 - value;
          };
    }
    free_denotation(d,1);
    return pack_int(n);
}

static init_const_AInt()
{
 /* __AInt_A0 */
 __AInt_A0 = pack_int(0);
 __AInt_Amax = pack_int(max_sword);
 __AInt_Amin = pack_int(min_sword);
}
