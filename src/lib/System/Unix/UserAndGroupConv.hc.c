/* hand-coded implementation part of UserAndGroupConv */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

/* import prototypes */
#include <unixconfig.h>
/* import Users and Groups */
#include "UserAndGroup.oc.h"


extern OBJ _AUserAndGroupConv_Ahc_Aconvuserid(OBJ x1) /* hc_convuserid */
{OBJ r;
  (void)sprintf(charbuf,"%lu",(unsigned long)(((USERID)(x1))->value));
  free_userid(x1,1);
  r=make_denotation(charbuf);
 return r;}

extern OBJ _AUserAndGroupConv_Ahc_Aconvgroupid(OBJ x1) /* hc_convgroupid */
{OBJ r;
  (void)sprintf(charbuf,"%lu",(unsigned long)(((GROUPID)(x1))->value));
  free_groupid(x1,1);
  r=make_denotation(charbuf);
 return r;}

extern void init_AUserAndGroup();

static void init_const_AUserAndGroupConv()
{
 init_AUserAndGroup();
}
