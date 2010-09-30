/* hand-coded interface part of Int */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/
  /* representation */

typedef SWORD INT;

#define pack_int(n)     pack_sword(n)
#define unpack_int(o)   unpack_sword(o)


  /* macro based implementations */

#define AInt_AuncheckedSucc(x1,x2) {x2=(OBJ)((INT)(x1)+2);}
#define AInt_AuncheckedPred(x1,x2) {x2=(OBJ)((INT)(x1)-2);}

#define AInt_AuncheckedMod(x1,x2,x3)\
        {x3=pack_int(unpack_int(x1)%unpack_int(x2));}
#define AInt_AuncheckedDiv(x1,x2,x3)\
        {x3=pack_int(unpack_int(x1)/unpack_int(x2));}
#define AInt_AuncheckedMul(x1,x2,x3)\
        {x3=pack_int(unpack_int(x1)*unpack_int(x2));}
#define AInt_AuncheckedSub(x1,x2,x3)\
        {x3=(OBJ)((INT)(x1)-(INT)(x2)+1);}
#define AInt_AuncheckedAdd(x1,x2,x3)\
        {x3=(OBJ)((INT)(x1)+(INT)(x2)-1);}

#define AInt_Sm_O1(x1,x2)\
	{x2=pack_int(-unpack_int(x1));}
#define AInt_Aabs(x1,x2)\
	{INT _t=unpack_int(x1);x2=pack_int(_t < 0 ? -_t : _t);}
#define AInt_Asign(x1,x2)\
	{INT _t=unpack_int(x1);\
	 if (_t < 0) x2=pack_int(-1); else \
	 if (_t == 0) x2=pack_int(0); else x2=pack_int(1);}
#define AInt_Aeven_(x1,x2)\
	{x2=pack_clean_bool(!((INT)(x1)&0x2));}
#define AInt_Aodd_(x1,x2)\
	{x2=pack_clean_bool(!!((INT)(x1)&0x2));}
#define AInt_Apos_(x1,x2)\
	{x2=pack_clean_bool(unpack_int(x1) > 0);}
#define AInt_Aneg_(x1,x2)\
	{x2=pack_clean_bool(unpack_int(x1) < 0);}
#define AInt_Azero_(x1,x2)\
	{x2=pack_clean_bool((x1) == pack_int(0));}

#define AInt_Se(x1,x2,x3)\
	{x3=pack_clean_bool((INT)(x1) == (INT)(x2));}
#define AInt_SSe(x1,x2,x3)\
	{x3=pack_clean_bool((INT)(x1) != (INT)(x2));}
#define AInt_Sg(x1,x2,x3)\
	{x3=pack_clean_bool(unpack_int(x1)>unpack_int(x2));}
#define AInt_Sl(x1,x2,x3)\
	{x3=pack_clean_bool(unpack_int(x1)<unpack_int(x2));}
#define AInt_Sle(x1,x2,x3)\
	{x3=pack_clean_bool(unpack_int(x1)<=unpack_int(x2));}
#define AInt_Sge(x1,x2,x3)\
	{x3=pack_clean_bool(unpack_int(x1)>=unpack_int(x2));}

