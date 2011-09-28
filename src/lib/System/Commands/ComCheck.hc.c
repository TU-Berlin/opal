/* hand-coded implementation part of ComCheck */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include <unixconfig.h>
/* only stdio */

#include "Com.oc.h"

extern char ** start_argv;

extern OBJ _AComCheck_AcheckAbort(OBJ x1,OBJ x2) /* checkAbort */
{OBJ r;
 get_denotation(x1,charbuf,sizeof(charbuf));
 fprintf(stderr,"%s: %s\n",start_argv[0],charbuf);
 return_okay_nil;
}

static init_const_AComCheck()
{}
