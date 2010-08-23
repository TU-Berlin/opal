/* hand-coded interface part of Char */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 2001-05-30 17:48:01 $ ($Revision: 1.2 $)
*/
  /* representation */

/* To use intptr_t, an int of size (void *). */
/* FIXME: check for stdint.h n configure script. */
#include <stdint.h>

typedef unsigned char OCSCHAR;

#define pack_char(x)   ((OBJ)((intptr_t)(((OCSCHAR)(x)<<1)+1)))
#define unpack_char(x) ((OCSCHAR)((WORD)(x)>>1))
#define convert2char(x) ((WORD)(x))


  /* macro based implementations */

#define AChar_AuncheckedSucc(x1,x2) {x2=(OBJ)(convert2char(x1)+2);}
#define AChar_AuncheckedPred(x1,x2) {x2=(OBJ)(convert2char(x1)-2);}

#define AChar_Se(x1,x2,x3)\
 	{x3=pack_clean_bool(convert2char(x1) == convert2char(x2));}
#define AChar_SSe(x1,x2,x3)\
	{x3=pack_clean_bool(convert2char(x1) != convert2char(x2));}
#define AChar_Sg(x1,x2,x3)\
	{x3=pack_clean_bool(convert2char(x1) > convert2char(x2));}
#define AChar_Sl(x1,x2,x3)\
	{x3=pack_clean_bool(convert2char(x1) < convert2char(x2));}
#define AChar_Sge(x1,x2,x3)\
	{x3=pack_clean_bool(convert2char(x1) >= convert2char(x2));}
#define AChar_Sle(x1,x2,x3)\
	{x3=pack_clean_bool(convert2char(x1) <= convert2char(x2));}


#include <ctype.h>

#define AChar_Alower_(x1,x2) {x2=pack_bool(islower(unpack_char(x1)));}
#define AChar_Aupper_(x1,x2) {x2=pack_bool(isupper(unpack_char(x1)));}
#define AChar_Acontrol_(x1,x2) {x2=pack_bool(iscntrl(unpack_char(x1)));}
#define AChar_Apunctuation_(x1,x2) {x2=pack_bool(ispunct(unpack_char(x1)));}
#define AChar_Aletter_(x1,x2) {x2=pack_bool(isalpha(unpack_char(x1)));}
#define AChar_Adigit_(x1,x2) {x2=pack_bool(isdigit(unpack_char(x1)));}
#define AChar_Adigit_(x1,x2) {x2=pack_bool(isdigit(unpack_char(x1)));}
#define AChar_Awhitespace_(x1,x2) {x2=pack_bool(isspace(unpack_char(x1)));}
#define AChar_Aprintable_(x1,x2) {x2=pack_bool(isprint(unpack_char(x1)));}

#define AChar_Alower(x1,x2){\
  OCSCHAR __t = unpack_char(x1);\
  x2 = isupper(__t) ? pack_char(tolower(__t)) : x1;\
}

#define AChar_Aupper(x1,x2){\
  OCSCHAR __t = unpack_char(x1);\
  x2 = islower(__t) ? pack_char(toupper(__t)) : x1;\
}

