/* hand-coded implementation part of Nat */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1999-03-09 11:51:19 $ ($Revision: 1.2 $)
*/


extern OBJ _ANat_Asucc_(OBJ x1) /* succ? */
{OBJ r;
 ANat_Asucc_(x1,r);
 return r;}

extern OBJ _ANat_A0_(OBJ x1) /* 0? */
{OBJ r;
 ANat_A0_(x1,r);
 return r;}

extern OBJ _ANat_Aeven_(OBJ x1) /* even? */
{OBJ r;
 ANat_Aeven_(x1,r);
 return r;}

extern OBJ _ANat_Aodd_(OBJ x1) /* odd? */
{OBJ r;
 ANat_Aodd_(x1,r);
 return r;}

extern OBJ _ANat_Sle(OBJ x1,OBJ x2) /* <= */
{OBJ r;
 ANat_Sle(x1,x2,r);
 return r;}

extern OBJ _ANat_Se(OBJ x1,OBJ x2) /* = */
{OBJ r;
 ANat_Se(x1,x2,r);
 return r;}

extern OBJ _ANat_Sge(OBJ x1,OBJ x2) /* >= */
{OBJ r;
 ANat_Sge(x1,x2,r);
 return r;}

extern OBJ _ANat_Sl(OBJ x1,OBJ x2) /* < */
{OBJ r;
 ANat_Sl(x1,x2,r);
 return r;}

extern OBJ _ANat_Sg(OBJ x1,OBJ x2) /* > */
{OBJ r;
 ANat_Sg(x1,x2,r);
 return r;}

extern OBJ _ANat_SSe(OBJ x1,OBJ x2) /* |= */
{OBJ r;
 ANat_SSe(x1,x2,r);
 return r;}

extern OBJ _ANat_AuncheckedSucc(OBJ x1) /* uncheckedSucc */
{OBJ r;
 ANat_AuncheckedSucc(x1,r);
 return r;}

extern OBJ _ANat_AuncheckedPred(OBJ x1) /* uncheckedPred */
{OBJ r;
 ANat_AuncheckedPred(x1,r);
 return r;}

extern OBJ _ANat_AuncheckedAdd(OBJ x1,OBJ x2) /* uncheckedAdd */
{OBJ r;
 ANat_AuncheckedAdd(x1,x2,r);
 return r;}

extern OBJ _ANat_AuncheckedSub(OBJ x1,OBJ x2) /* uncheckedSub */
{OBJ r;
 ANat_AuncheckedSub(x1,x2,r);
 return r;}

extern OBJ _ANat_AuncheckedMul(OBJ x1,OBJ x2) /* uncheckedMul */
{OBJ r;
 ANat_AuncheckedMul(x1,x2,r);
 return r;}

extern OBJ _ANat_AuncheckedDiv(OBJ x1,OBJ x2) /* uncheckedDiv */
{OBJ r;
 ANat_AuncheckedDiv(x1,x2,r);
 return r;}

extern OBJ _ANat_AuncheckedMod(OBJ x1,OBJ x2) /* uncheckedMod */
{OBJ r;
 ANat_AuncheckedMod(x1,x2,r);
 return r;}

extern OBJ _ANat_Apow(OBJ x1,OBJ x2) /* pow */
{NAT x,y,r;

 x = unpack_nat(x1);
 y = unpack_nat(x2);
 if (y == 0) {
   r = 1;}
 else if (x == 0) {
   r = 0;}
 else { 
   for(r = 1; y > 0; y--) {
     if (max_word / x < r) {
       HLT("^'Nat:nat**nat->nat: result too large");
	 };
     r *= x;
   }
 };
 return pack_nat(r);
}


extern OBJ inline_opal_nat(OBJ d) 
{ NAT n = 0, i = 0; 
  int l = leng_denotation(d);
  int value;
  if (l == 0)
      HLT("!'Nat:denotation->nat: no digits in denotation");
  while (i < l){
      value = data_denotation(d)[i++];
      if(value < '0' || value > '9')
         HLT("!'Nat:denotation -> nat: found non digit character");
      value -= '0';
      if((max_word - value)/10 < n)
           HLT("!'Nat:denotation -> nat: constant too large");
      n = n*10 + value;
  }
  free_denotation(d,1);
  return pack_nat(n);
}

static init_const_ANat()
{
 __ANat_A0 = pack_nat(0);
 __ANat_Amax = pack_nat(max_word);
}
