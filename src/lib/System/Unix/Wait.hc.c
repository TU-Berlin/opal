/* hand-coded implementation part of Wait */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 2001-05-25 12:06:11 $ ($Revision: 1.3 $)
*/

#include <unixconfig.h>

/* import exit macros */
/* #include <stdlib.h> */
/* #include <unistd.h> */
/* import types */
/* #include <sys/types.h> */
/* import macros and prototypes */
/* #include <sys/wait.h> */


/* import Commands */
#include "Com.h"
/* import Failures */
#include "UnixFailures.h"
/* import Option */
#include "Option.h"
/* import Pair */
#include "Pair.h"
/* import ProcessCtrl */
#include "ProcessCtrl.h"
/* import Signals */
#ifdef OCS_FILENAMES_CASEFOLD
#include "Signal_ocs.h"
#else
#include "Signal.h"
#endif

static int make_signal( int sig, OBJ * res )
{
  switch(sig)
  {
  case SIGABRT:
    copy_some(__ASignal_AsigAbrt,1); *res=__ASignal_AsigAbrt; break;
  case SIGALRM:
    copy_some(__ASignal_AsigAlrm,1); *res=__ASignal_AsigAlrm; break;
  case SIGFPE :
    copy_some(__ASignal_AsigFPE,1); *res=__ASignal_AsigFPE; break;
  case SIGHUP :
    copy_some(__ASignal_AsigHUp,1); *res=__ASignal_AsigHUp; break;
  case SIGILL :
    copy_some(__ASignal_AsigIll,1); *res=__ASignal_AsigIll; break;
  case SIGINT:
    copy_some(__ASignal_AsigInt,1); *res=__ASignal_AsigInt; break;
  case SIGKILL:
    copy_some(__ASignal_AsigKill,1); *res=__ASignal_AsigKill; break;
  case SIGPIPE:
    copy_some(__ASignal_AsigPipe,1); *res=__ASignal_AsigPipe; break;
  case SIGQUIT:
    copy_some(__ASignal_AsigQuit,1); *res=__ASignal_AsigQuit; break;
  case SIGSEGV:
    copy_some(__ASignal_AsigSegV,1); *res=__ASignal_AsigSegV; break;
  case SIGTERM:
    copy_some(__ASignal_AsigTerm,1); *res=__ASignal_AsigTerm; break;
  case SIGUSR1:
    copy_some(__ASignal_AsigUsr1,1); *res=__ASignal_AsigUsr1; break;
  case SIGUSR2:
    copy_some(__ASignal_AsigUsr2,1); *res=__ASignal_AsigUsr2; break;
  case SIGCHLD:
    copy_some(__ASignal_AsigChld,1); *res=__ASignal_AsigChld; break;
  case SIGCONT:
    copy_some(__ASignal_AsigCont,1); *res=__ASignal_AsigCont; break;
  case SIGSTOP:
    copy_some(__ASignal_AsigStop,1); *res=__ASignal_AsigStop; break;
  case SIGTSTP:
    copy_some(__ASignal_AsigTStp,1); *res=__ASignal_AsigTStp; break;
  case SIGTTIN:
    copy_some(__ASignal_AsigTTIn,1); *res=__ASignal_AsigTTIn; break;
  case SIGTTOU:
    copy_some(__ASignal_AsigTTOu,1); *res=__ASignal_AsigTTOu; break;
  default:
    return(-1);
  }
  return(0);
}

static OBJ hc_wait( pid_t pid, int options )
{OBJ r;
 OBJ cs;
 OBJ sig;
 OBJ cp;
 OBJ pr;
 int statloc;
 pid_t tmppid;
  tmppid=waitpid(pid,&statloc,options|WUNTRACED);
  if(tmppid==(pid_t)-1) {
    return_unix_failure(errno);
  }
  if(tmppid!=(pid_t)0) {
    if(WIFEXITED(statloc)) {
      if(WEXITSTATUS(statloc)==EXIT_SUCCESS) {
        copy_some(__AProcessCtrl_Asuccess,1);
        AWait_Aexited(__AProcessCtrl_Asuccess,cs);
      }
      else {
        copy_some(__AProcessCtrl_Afailure,1);
        AWait_Aexited(__AProcessCtrl_Afailure,cs);
      }
    }
    else {
      if(WIFSIGNALED(statloc)) {
        if(make_signal(WTERMSIG(statloc),&sig)) {
          copy_some(__AWait_AsignalledUnknown,1);
          cs=__AWait_AsignalledUnknown;
        }
        else {
          AWait_Asignalled(sig,cs);
        }
      }
      else {
        if(WIFSTOPPED(statloc)) {
          if(make_signal(WSTOPSIG(statloc),&sig)) {
            copy_some(__AWait_AstoppedUnknown,1);
            cs=__AWait_AstoppedUnknown;
          }
          else {
            AWait_Astopped(sig,cs);
          }
        }
        else {
          HLT("hc_wait\'Wait: Unknown process exit status");
        }
      }
    }
    make_process(tmppid,cp);
    APair_S7(cp,cs,pr);
    if(options&WNOHANG) {
      AOption_Aavail(pr,r);
    }
    else {
      r=pr;
    }
  }
  else {
    if(options&WNOHANG) {
      copy_some(__AOption_Anil,1);
      r=__AOption_Anil;
    }
    else {
      HLT("hc_wait\'Wait: waitpid returns (pid_t)0 in blocking mode");
    }
  }
 return_okay(r);
}

extern OBJ _AWait_Ahc_Awaitpid_Aany(OBJ x1) /* hc_waitpid_any */
{OBJ r;
  free_some(x1,1);
  r=hc_wait((pid_t)-1,0);
 return r;
}

extern OBJ _AWait_Ahc_Awaitpid_Aany_Anb(OBJ x1) /* hc_waitpid_any_nb */
{OBJ r;
  free_some(x1,1);
  r=hc_wait((pid_t)-1,WNOHANG);
 return r;}

extern OBJ _AWait_Ahc_Awaitpid(OBJ x1,OBJ x2) /* hc_waitpid */
{OBJ r;
 pid_t tmppid;
  free_some(x2,1);
  tmppid=((PROCESS)(x1))->value;
  free_process(x1,1);
  r=hc_wait(tmppid,0);
 return r;}

extern OBJ _AWait_Ahc_Awaitpid_Anb(OBJ x1,OBJ x2) /* hc_waitpid_nb */
{OBJ r;
 pid_t tmppid;
  free_some(x2,1);
  tmppid=((PROCESS)(x1))->value;
  free_process(x1,1);
  r=hc_wait(tmppid,WNOHANG);
 return r;}

extern OBJ _AWait_Ahc_Awaitgrp_Aany(OBJ x1) /* hc_waitgrp_any */
{OBJ r;
  free_some(x1,1);
  r=hc_wait((pid_t)0,0);
 return r;}

extern OBJ _AWait_Ahc_Awaitgrp_Aany_Anb(OBJ x1) /* hc_waitgrp_any_nb */
{OBJ r;
  free_some(x1,1);
  r=hc_wait((pid_t)0,WNOHANG);
 return r;}

extern OBJ _AWait_Ahc_Awaitgrp(OBJ x1,OBJ x2) /* hc_waitgrp */
{OBJ r;
 pid_t tmppid;
  free_some(x2,1);
  tmppid=((PROCESS)(x1))->value;
  free_process(x1,1);
  r=hc_wait(-tmppid,0);
 return r;}

extern OBJ _AWait_Ahc_Awaitgrp_Anb(OBJ x1,OBJ x2) /* hc_waitgrp_nb */
{OBJ r;
 pid_t tmppid;
  free_some(x2,1);
  tmppid=((PROCESS)(x1))->value;
  free_process(x1,1);
  r=hc_wait(-tmppid,WNOHANG);
 return r;}

static init_const_AWait()
{
 init_ACom();
 init_AUnixFailures();
 init_AOption();
 init_APair();
 init_AProcessCtrl();
 init_ASignal();
}
