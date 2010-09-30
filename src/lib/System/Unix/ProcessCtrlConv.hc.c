/* hand-coded implementation part of ProcessCtrlConv */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>
#include "ProcessCtrl.h"


extern OBJ _AProcessCtrlConv_Ahc_Aconvprocess(OBJ x1) /* hc_convprocess */
{OBJ r;
  (void)sprintf(charbuf,"%ld",(long)(((PROCESS)(x1))->value));
  free_process(x1,1);
  r=make_denotation(charbuf);
 return r;}

static init_const_AProcessCtrlConv()
{
 init_AProcessCtrl();
}
