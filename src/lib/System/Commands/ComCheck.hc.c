/* hand-coded implementation part of ComCheck */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:17 $ ($Revision: 1.1.1.1 $)
*/
#include <unixconfig.h>
/* only stdio */

#include "Com.h"

extern char ** start_argv;

extern OBJ _AComCheck_AcheckAbort(OBJ x1,OBJ x2) /* checkAbort */
{OBJ r;
 get_denotation(x1,charbuf,sizeof(charbuf));
 fprintf(stderr,"%s: %s\n",start_argv[0],charbuf);
 return_okay_nil;
}

static init_const_AComCheck()
{}
