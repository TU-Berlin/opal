/* hand-coded interface part of Nat */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 15:59:58 $ ($Revision: 1.1.1.1 $)
*/
  /* representation */

typedef WORD NAT;	

#define pack_nat(x)   pack_word(x)
#define unpack_nat(x) unpack_word(x)

extern OBJ inline_opal_nat(OBJ);
#define inline_c_nat(n) pack_nat(n)


  /* macro based implementations */

#define ANat_AuncheckedSucc(x1,x2) {x2=(OBJ)((NAT)(x1)+2);}
#define ANat_AuncheckedPred(x1,x2) {x2=(OBJ)((NAT)(x1)-2);}

#define ANat_A0_(x1,x2) {x2=pack_clean_bool(x1 == pack_nat(0));}
#define ANat_Asucc_(x1,x2) {x2=pack_clean_bool(x1 != pack_nat(0));}

#define ANat_AuncheckedMod(x1,x2,x3)\
	{x3=pack_nat(unpack_nat(x1)%unpack_nat(x2));}
#define ANat_AuncheckedDiv(x1,x2,x3)\
	{x3=pack_nat(unpack_nat(x1)/unpack_nat(x2));}
#define ANat_AuncheckedMul(x1,x2,x3)\
	{x3=(OBJ)(((NAT)(x1)-1)*((NAT)(x2)/2)+1);}
#define ANat_AuncheckedSub(x1,x2,x3)\
	{x3=(OBJ)((NAT)(x1)-(NAT)(x2)+1);}
#define ANat_AuncheckedAdd(x1,x2,x3)\
	{x3=(OBJ)((NAT)(x1)+(NAT)(x2)-1);}

#define ANat_Se(x1,x2,x3) {x3=pack_clean_bool((NAT)(x1) == (NAT)(x2));}
#define ANat_SSe(x1,x2,x3) {x3=pack_clean_bool((NAT)(x1) != (NAT)(x2));}
#define ANat_Sg(x1,x2,x3) {x3=pack_clean_bool((NAT)(x1) > (NAT)(x2));}
#define ANat_Sl(x1,x2,x3) {x3=pack_clean_bool((NAT)(x1) < (NAT)(x2));}
#define ANat_Sge(x1,x2,x3) {x3=pack_clean_bool((NAT)(x1) >= (NAT)(x2));}
#define ANat_Sle(x1,x2,x3) {x3=pack_clean_bool((NAT)(x1) <= (NAT)(x2));}
#define ANat_Aeven_(x1,x2) {x2=pack_clean_bool(!((NAT)(x1)&0x2));}
#define ANat_Aodd_(x1,x2) {x2=pack_clean_bool(!!((NAT)(x1)&0x2));}

