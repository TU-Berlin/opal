/* hand-coded implementation part of ComTimeout */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/
#include "ComAgent.h"


extern OBJ _AComTimeout_AtimeoutProc(OBJ x1,OBJ x2) /* timeoutProc */ {
    return agent_timeoutProc(x1);
}


static init_const_AComTimeout()
{ init_AComAgent(); }
