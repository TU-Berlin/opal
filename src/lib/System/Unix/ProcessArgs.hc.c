/* hand-coded implementation part of ProcessArgs */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/



#include <unixconfig.h>

#include "Nat.h"
#include "Com.h"
#include "Seq.h"
#include "UnixFailures.h"

extern int start_argc;           /* defined ... */
extern char **start_argv;        /* ... in _ostart.c */

static OBJ Args;                 /* holding the current arguments as
				    a sequence of denotations */



extern OBJ _AProcessArgs_AcArgs(OBJ Unit) {
    copy_some(Args, 1);
    return_okay(Args);
}

extern OBJ _AProcessArgs_AcSetArgs(OBJ NewArgs,OBJ Unit){
    free_some(Args, 1);
    Args = NewArgs;
    return_okay_nil;
}

extern OBJ _AProcessArgs_AcEnv(OBJ Var,OBJ Unit) {
    char *val;
    val = getenv(data_denotation(Var));
    free_denotation(Var, 1);
    if (val != NULL){
	return_okay(make_denotation(val));
    } else {
	return_fail(VAR(ProcessArgs, noSuchEnvVar));
    }
}

extern OBJ _AProcessArgs_AcPutEnv(OBJ Def,OBJ Unit){
    int res;
    char *buf;

    /*     fprintf(stderr, "ProcessArgs-cPutEnv: %d >%s<\n", leng_denotation(Def), data_denotation(Def)); */ 
    buf = malloc_aux(leng_denotation(Def) + 1);
    if (buf == NULL) {
      HLT("cPutEnv'ProcessArgs.hc.c: out of memory");
    };
    strncpy(buf, data_denotation(Def), leng_denotation(Def) + 1);
    res = putenv(buf);
    free_denotation(Def, 1);
    if (res == 0){
	return_okay_nil;
    } else {
	return_unix_failure(errno);
    }
}
    
static init_const_AProcessArgs(){
    /* build up the initial argument sequence */
    int i;
    Args = VARs(Seq,lg);
    for (i = start_argc - 1; i >= 0; i--){
	Args = ENTs(Seq,ii)(make_denotation(start_argv[i]), Args);
    }
}
