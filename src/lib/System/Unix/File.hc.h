/* hand-coded interface part of File */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:21 $ ($Revision: 1.1.1.1 $)
*/
  /* representation */

#include <stdio.h>

#define pack_file(file) pack_pointer(file)
#define unpack_file(ob) ((FILE*)unpack_pointer(ob))

