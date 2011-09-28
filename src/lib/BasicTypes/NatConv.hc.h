/* hand-coded interface part of NatConv */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "Nat.oc.h"
#include "Char.oc.h"

#define ANatConv_AuncheckedAsChar(x1,x2) {x2=pack_char(unpack_nat(x1));}

