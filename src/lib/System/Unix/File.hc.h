/* hand-coded interface part of File */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
  /* representation */

#include <stdio.h>

#define pack_file(file) pack_pointer(file)
#define unpack_file(ob) ((FILE*)unpack_pointer(ob))

