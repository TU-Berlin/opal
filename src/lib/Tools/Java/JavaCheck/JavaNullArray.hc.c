/* hand-coded implementation part of JavaNullArray */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "JavaVM.oc.h"


extern OBJ _AJavaNullArray_Anull_(OBJ x1) /* null? */
{
  int res = tst_sflag(x1, null_sflag);
  free_array(x1,1);
  return pack_bool(res);
}

static void init_const_AJavaNullArray()
{}
