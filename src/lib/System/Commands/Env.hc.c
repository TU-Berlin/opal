/* hand-coded implementation part of Env */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>

#include "Nat.h"
#include "Com.h"

extern int start_argc;           /* defined ... */
extern char **start_argv;       /* ... in _ostart.c */

extern OBJ _AEnv_AxargCount(OBJ unit) /* xargCount */ {
    return_okay(pack_nat(start_argc));
}

extern OBJ _AEnv_Axarg(OBJ no,OBJ unit) /* xarg */ {
    int n = unpack_nat(no);
    if (n < start_argc){
	return_okay(make_denotation(start_argv[n]));
    } else {
	return_fail(__AEnv_AnoSuchArg)
    }
}

extern OBJ _AEnv_Axenv(OBJ den,OBJ unit) /* xenv */ {
    char *e;
    get_denotation(den,charbuf,CHARBUFSIZE);
    if ( (e = getenv(charbuf)) != NULL ){
	return_okay(make_denotation(e));
    } else {
	return_fail(__AEnv_AnoSuchEnvVar);
    }
}

static init_const_AEnv()
{}
