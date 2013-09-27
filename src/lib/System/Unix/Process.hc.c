/* hand-coded implementation part of Process */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>
#include "Nat.oc.h"
#include "Array.oc.h"
#include "Com.oc.h"
#include "File.oc.h"
#include "UnixFailures.oc.h"
#include "Option.oc.h"

extern char ** start_env;

extern OBJ _AProcess_Aself_(OBJ x1) /* self? */
{OBJ r;
 AProcess_Aself_(x1,r);
 return r;}

extern OBJ _AProcess_Axfork(OBJ x1) /* xfork */ 
{int pid;
 pid = fork();
 if (pid == -1) {
   return_unix_failure(errno);
 } else {
   return_okay(pack_process(pid));
 }
}

static void patch(OBJ argv){
    /* patch an argument vector (initialized in Process.impl
       as an array of denotations) for call of execve.
    */
    int i;
    for (i = 0; i < leng_array(argv)-1; i++){
	/* skip header of denotation */
	data_array(argv)[i] = (OBJ)((DENOTATION)(data_array(argv)[i])+1);
    }
    data_array(argv)[i] = (void*)0; 	/* set up zero terminator */
}

static void unpatch(OBJ argv){
    /* undos patches an argument vector.
    */
    int i;
    for (i = 0; i < leng_array(argv)-1; i++){
	data_array(argv)[i] = (OBJ)((DENOTATION)(data_array(argv)[i])-1);
    }
    data_array(argv)[i] = NIL;
}


extern OBJ _AProcess_Axexecve(OBJ name,OBJ argv,OBJ envp,OBJ unit) {
    OBJ env;
    
    if ( ! get_denotation(name,charbuf,sizeof(charbuf)) ) {
	free_some(argv,1); free_some(envp,1);
	return_fail(__AUnixFailures_AnameTooLong);
    }

    patch(argv);
    if (!ISPRM(envp)){
      env = _AOption_Acont(envp);
      patch(env);
    } else env = NIL;

    if (execve(charbuf,(char**)data_array(argv),
               env == NIL ? start_env : (char**)data_array(env)) == -1){
	unpatch(argv); free_some(argv,1);
	if (env != NIL) { unpatch(env); free_some(env,1); }
	return_unix_failure(errno);
    } 
    HLT("execve'Process: internal error");
}

extern OBJ _AProcess_Axkill(OBJ p,OBJ s,OBJ unit) /* xkill */
{
 if (kill(unpack_process(p),unpack_nat(s)) == -1) {
   return_unix_failure(errno);
 } else {
   return_okay_nil;
 }
}

extern OBJ _AProcess_Axwait(OBJ x1) /* xwait */
{ int pid,status;
  pid = wait(&status);
  if (pid == -1) {
    return_unix_failure(errno);
  } else {
    return_okay(_AProcess_Apair(pack_process(pid),pack_nat(status)));
  }
}

extern OBJ _AProcess_Axpopen(OBJ name,OBJ type,OBJ unit) /* xpopen */ {
    FILE * f;
    char tbuf [ 10 ];
    if ( ! get_denotation(name,charbuf,sizeof(charbuf))  
       | ! get_denotation(type,tbuf,sizeof(tbuf)) ) {
	return_fail(__AUnixFailures_AnameTooLong);
    }
    if (f = popen(charbuf,tbuf)){
	return_okay(pack_file(f));
    } else {
	return_unix_failure(errno);
    }
}

extern OBJ _AProcess_Axpclose(OBJ file,OBJ unit) /* xpclose */ {
    if (pclose(unpack_file(file)) == -1) {
	return_unix_failure(errno);
    } else {
	return_okay_nil;
    }
}

extern void init_ANat();
extern void init_AArray();
extern void init_ACom();
extern void init_AFile();
extern void init_AUnixFailures();

static void init_const_AProcess()
{
  init_ANat();
  init_AArray();
  init_ACom();
  init_AFile();
  init_AUnixFailures();
 __AProcess_Aself = pack_process(0);
}
