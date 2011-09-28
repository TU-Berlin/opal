/* hand-coded implementation part of ComService */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "ComAgent.oc.h"


extern OBJ _AComService_AsapProc(OBJ x1) 
{ return agent_sapProc(); }

extern OBJ _AComService_ArequestProc(OBJ x1, OBJ x2, OBJ x3) 
{ return agent_requestProc(x1, x2); }

extern OBJ _AComService_AprovideProc(OBJ x1, OBJ x2, OBJ x3, OBJ x4) 
{ return agent_provideProc(x1, x2, x3); }


static init_const_AComService()
{ init_AComAgent(); }
