/* hand-coded implementation part of IntConv */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "Nat.oc.h"
#include "Int.oc.h"
#include "Real.oc.h"

#include <string.h> // used for memset

#define MAX_LEN_OF_INT (11)

extern OBJ _AIntConv_Sq(OBJ x1) /* ` */
{OBJ r;
 INT i = unpack_int(x1);
 int negative = i < 0;

 static char b[MAX_LEN_OF_INT];
 char *p = b + 9; 

 if(negative) i = -i;

 memset(b, '\0', MAX_LEN_OF_INT);
 if(i == 0)
   { *p-- = '0' ;}
 else
   { for(; i != 0; i = i / 10)
       { *p-- = '0' + (i % 10); };
   }
 if(negative)
   *p = '-';
 else
   p++;
 
 return make_denotation(p);
}


extern OBJ _AIntConv_AasNat(OBJ x1) /* asNat */
{ INT cand = unpack_int(x1);
  if (cand < 0) 
    HLT("asNat'IntConv: integer negative");
  return pack_nat((NAT)cand);
}

extern OBJ _AIntConv_AasReal(OBJ x1) /* asReal */
{ OBJ r;
  make_real((double)unpack_int(x1),r);
  return r;
}

static void init_const_AIntConv()
{}
