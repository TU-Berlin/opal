/* hand-coded interface part of CharConv */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 15:59:58 $ ($Revision: 1.1.1.1 $)
*/

#include "Nat.h"
#include "Char.h"

#define ACharConv_AasNat(x1,x2) {x2=pack_nat(unpack_char(x1));}
