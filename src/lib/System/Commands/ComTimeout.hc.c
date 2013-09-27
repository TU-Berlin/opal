/* hand-coded implementation part of ComTimeout */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "ComAgent.oc.h"


extern OBJ _AComTimeout_AtimeoutProc(OBJ x1,OBJ x2) /* timeoutProc */ {
    return agent_timeoutProc(x1);
}

extern void init_AComAgent();

static void init_const_AComTimeout()
{ init_AComAgent(); }
