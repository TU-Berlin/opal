/* hand-coded interface part of Tcl */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "tcl.h"

typedef struct sINTERP {
    Tcl_Interp * tcl;           /* the tcl control structure */
    OBJ State;                  /* the opal state */
} * INTERP;


#define pack_interpreter(x) pack_pointer((OBJ)(x))
#define unpack_interpreter(x) unpack_pointer((INTERP)(x))



