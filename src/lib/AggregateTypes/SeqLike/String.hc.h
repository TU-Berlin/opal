/* hand-coded interface part of String */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:01 $ ($Revision: 1.1.1.1 $)
*/
#include "Nat.h"


  /* stuff for hand-coding */

extern int get_string(OBJ,char*,int);
    /* copy string into buffer of size (and free it).
       returns 1 if string totally fits into buffer,
       0 if not, in which case the string is truncated. */

#define make_string(cstr) _AString_AasString(make_denotation(cstr))
    /* create string from C string */

#define is_empty_string(s) ((s) == __AString_Slg)
    /* test for empty string */

#define unpack_chunck_string(str,start,data,rest){\
     start=unpack_nat(FLD1(str,1)); data = FLD1(str,2); rest = FLD1(str,3);\
     if (excl_structured(str,1)) { dispose_structured_flat(str); }\
     else {copy_denotation(data,1); copy_some(rest,1); decr_structured(str,1);}}
   /* unpack components of non-empty string, a chunck */

#define addr_rest_string(str, adr) adr = &FLD1(str,3);
    /* get address of rest field of chunck (for TMC) */ 


