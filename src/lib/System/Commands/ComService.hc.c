/* hand-coded implementation part of ComService */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:17 $ ($Revision: 1.1.1.1 $)
*/
#include "ComAgent.h"


extern OBJ _AComService_AsapProc(OBJ x1) 
{ return agent_sapProc(); }

extern OBJ _AComService_ArequestProc(OBJ x1, OBJ x2, OBJ x3) 
{ return agent_requestProc(x1, x2); }

extern OBJ _AComService_AprovideProc(OBJ x1, OBJ x2, OBJ x3, OBJ x4) 
{ return agent_provideProc(x1, x2, x3); }


static init_const_AComService()
{ init_AComAgent(); }
