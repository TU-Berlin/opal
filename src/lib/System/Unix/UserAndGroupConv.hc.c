/* hand-coded implementation part of UserAndGroupConv */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/

/* import prototypes */
#include <unixconfig.h>
/* import Users and Groups */
#include "UserAndGroup.h"


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

static init_const_AUserAndGroupConv()
{
 init_AUserAndGroup();
}
