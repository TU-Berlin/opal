/* hand-coded interface part of Denotation */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "Nat.h"
#include "Char.h"

  /* macro based implementations */

#define ADenotation_S3(x1,x2){\
   	x2=pack_nat(leng_denotation(x1)); free_denotation(x1,1);\
}
#define ADenotation_AuncheckedSel(x1,x2,x3){\
	x3=pack_char(data_denotation(x1)[unpack_nat(x2)]);\
	free_denotation(x1,1);\
}
extern OBJ dup_denotation(OBJ);
#define ADenotation_AuncheckedUpd(x1,x2,x3,x4){\
	if (excl_denotation(x1,1)) x4=x1; else {x4=dup_denotation(x1);}\
	data_denotation(x4)[unpack_nat(x2)] = unpack_char(x3);\
}
    
