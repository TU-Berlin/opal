/* hand-coded interface part of CharConv */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include "Nat.h"
#include "Char.h"

#define ACharConv_AasNat(x1,x2) {x2=pack_nat(unpack_char(x1));}
