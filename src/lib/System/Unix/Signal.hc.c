/* hand-coded implementation part of Signal */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:20 $ ($Revision: 1.1.1.1 $)
*/

#include <unixconfig.h>

/* import NULL */
/* #include <stdlib.h> */
/* import Prototypes */
/* #include <unistd.h> */

/* import Commands */
#include "Com.h"
/* import Failures */
#include "UnixFailures.h"
/* import Option */
#include "Option.h"
/* import Nats */
#include "Nat.h"
/* import Processes */
#include "ProcessCtrl.h"


static int convert_signal(OBJ osig)
{OBJ r;
  /* NOTE that this function keeps the number of osig references constant */
  copy_some(osig,1);
  ASignal_AsigAbrt_(osig,r);
  if(unpack_bool(r)) return SIGABRT;
  copy_some(osig,1);
  ASignal_AsigAlrm_(osig,r);
  if(unpack_bool(r)) return SIGALRM;
  copy_some(osig,1);
  ASignal_AsigFPE_(osig,r);
  if(unpack_bool(r)) return SIGFPE;
  copy_some(osig,1);
  ASignal_AsigHUp_(osig,r);
  if(unpack_bool(r)) return SIGHUP;
  copy_some(osig,1);
  ASignal_AsigIll_(osig,r);
  if(unpack_bool(r)) return SIGILL;
  copy_some(osig,1);
  ASignal_AsigInt_(osig,r);
  if(unpack_bool(r)) return SIGINT;
  copy_some(osig,1);
  ASignal_AsigKill_(osig,r);
  if(unpack_bool(r)) return SIGKILL;
  copy_some(osig,1);
  ASignal_AsigPipe_(osig,r);
  if(unpack_bool(r)) return SIGPIPE;
  copy_some(osig,1);
  ASignal_AsigQuit_(osig,r);
  if(unpack_bool(r)) return SIGQUIT;
  copy_some(osig,1);
  ASignal_AsigSegV_(osig,r);
  if(unpack_bool(r)) return SIGSEGV;
  copy_some(osig,1);
  ASignal_AsigTerm_(osig,r);
  if(unpack_bool(r)) return SIGTERM;
  copy_some(osig,1);
  ASignal_AsigUsr1_(osig,r);
  if(unpack_bool(r)) return SIGUSR1;
  copy_some(osig,1);
  ASignal_AsigUsr2_(osig,r);
  if(unpack_bool(r)) return SIGUSR2;
  copy_some(osig,1);
  ASignal_AsigChld_(osig,r);
  if(unpack_bool(r)) return SIGCHLD;
  copy_some(osig,1);
  ASignal_AsigCont_(osig,r);
  if(unpack_bool(r)) return SIGCONT;
  copy_some(osig,1);
  ASignal_AsigStop_(osig,r);
  if(unpack_bool(r)) return SIGSTOP;
  copy_some(osig,1);
  ASignal_AsigTStp_(osig,r);
  if(unpack_bool(r)) return SIGTSTP;
  copy_some(osig,1);
  ASignal_AsigTTIn_(osig,r);
  if(unpack_bool(r)) return SIGTTIN;
  copy_some(osig,1);
  ASignal_AsigTTOu_(osig,r);
  if(unpack_bool(r)) return SIGTTOU;
  /* Hmmm... */
  HLT("convert_signal\'Signal: Internal error (unknown signal constructor)");
  return 0;
}

extern OBJ _ASignal_Sl_O1(OBJ x1,OBJ x2) /* <,1 */
{OBJ r;
  /* use macro expansion entry */
  ASignal_Sl_O1(x1,x2,r);
 return r;}

extern OBJ _ASignal_Se_O1(OBJ x1,OBJ x2) /* =,1 */
{OBJ r;
  /* use macro expansion entry */
  ASignal_Se_O1(x1,x2,r);
 return r;}

extern OBJ _ASignal_Ahc_Asigaddset(OBJ x1,OBJ x2) /* hc_sigaddset */
{OBJ r;
 int sig;
  sig=convert_signal(x2);
  free_some(x2,1);
  if(excl_sigmask(x1,1)) {
    r=x1;
  }
  else {
    decr_sigmask(x1,1);
    alloc_sigmask(r);
    memcpy(&(((SIGMASK)(r))->value),&(((SIGMASK)(x1))->value),size_sigmask);  
  }
  if(sigaddset(&(((SIGMASK)(r))->value),sig)) {
    /* this should never happen */
    HLT("hc_sigaddset'Signal: sigaddset() returns nonzero value");
  }
 return r;}

extern OBJ _ASignal_Ahc_Asigdelset(OBJ x1,OBJ x2) /* hc_sigdelset */
{OBJ r;
 int sig;
  sig=convert_signal(x2);
  free_some(x2,1);
  if(excl_sigmask(x1,1)) {
    r=x1;
  }
  else {
    decr_sigmask(x1,1);
    alloc_sigmask(r);
    memcpy(&(((SIGMASK)(r))->value),&(((SIGMASK)(x1))->value),size_sigmask);  
  }
  if(sigdelset(&(((SIGMASK)(r))->value),sig)) {
    /* this should never happen */
    HLT("hc_sigdelset'Signal: sigdelset() returns nonzero value");
  }
 return r;}

extern OBJ _ASignal_Ahc_Asigismember(OBJ x1,OBJ x2) /* hc_sigismember */
{OBJ r;
 int sig;
 int ismstat;
  sig=convert_signal(x2);
  free_some(x2,1);
  ismstat=sigismember(&(((SIGMASK)(x1))->value),sig);
  free_sigmask(x1,1);
  if (ismstat < 0) {
    /* this should never happen */
    HLT("hc_sigismember\'Signal: sigismember() returned negative result");
  }
  r=pack_clean_bool(ismstat);
 return r;}

extern OBJ _ASignal_Ahc_Akill_Ap(OBJ x1,OBJ x2,OBJ x3) /* hc_kill_p */
{OBJ r;
 int sig;
 int killstat;
  free_some(x3,1);
  copy_some(x2,1);
  AOption_Anil_(x2,r);
  if(unpack_bool(r)) {
    sig=0;
  }
  else {
    AOption_Acont(x2,r);
    sig=convert_signal(r);
    free_some(r,1);
  }
  killstat=kill(((PROCESS)(x1))->value,sig);
  free_process(x1,1);
  if(killstat) {
    return_unix_failure(errno);
  }
  return_okay_nil;
}

extern OBJ _ASignal_Ahc_Akill_Agrp(OBJ x1,OBJ x2,OBJ x3) /* hc_kill_grp */
{OBJ r;
 int sig;
 int killstat;
  free_some(x3,1);
  copy_some(x2,1);
  AOption_Anil_(x2,r);
  if(unpack_bool(r)) {
    sig=0;
  }
  else {
    AOption_Acont(x2,r);
    sig=convert_signal(r);
    free_some(r,1);
  }
  killstat=kill(-(((PROCESS)(x1))->value),sig);
  free_process(x1,1);
  if(killstat) {
    return_unix_failure(errno);
  }
  return_okay_nil;
}

extern OBJ _ASignal_Ahc_Aalarm(OBJ x1,OBJ x2) /* hc_alarm */
{OBJ r;
  free_some(x2,1);
  r=pack_nat(alarm((unsigned int)(unpack_nat(x1))));
 return_okay(r);
}

extern OBJ _ASignal_Ahc_Aexec_Ash(OBJ x1,OBJ x2) /* hc_exec_sh */
{OBJ r;
 FREE(x1,1);
 FREE(x2,1);
#line 215
 HLT("Signal at <215,5> : hc_exec_sh\'Signal:(sighandler->(void->ans[void])) not implemented");
#line 215
 return r;}

extern OBJ _ASignal_Ahc_Acaught_(OBJ x1,OBJ x2) /* hc_caught? */
{OBJ r;
 FREE(x2,1);
#line 218
 HLT("Signal at <218,5> : hc_caught?\'Signal:(signal->(void->ans[bool])) not implemented");
#line 218
 return r;}

extern OBJ _ASignal_Ahc_Asigaction(OBJ x1,OBJ x2,OBJ x3) /* hc_sigaction */
{OBJ r;
 FREE(x2,1);
 FREE(x3,1);
#line 140
 HLT("Signal at <140,5> : hc_sigaction\'Signal:((signal**option[sigaction])->(void->ans[sigaction])) not implemented");
#line 140
 return r;}

extern OBJ _ASignal_Ahc_Asigprocmask(OBJ x1,OBJ x2,OBJ x3) /* hc_sigprocmask */
{OBJ r;
 int how;
 sigset_t setdata;
 sigset_t * set;
  free_some(x3,1);
  copy_some(x2,1);
  AOption_Anil_(x2,r);
  if(unpack_bool(r)) {
    set=NULL;
    free_some(x2,1);
  }
  else {
    AOption_Acont(x2,r);
    memcpy(&setdata,&(((SIGMASK)(r))->value),size_sigmask);
    set=&setdata;
    free_sigmask(r,1);
  }
  copy_some(x1,1);
  ASignal_Ama_Ablock_(x1,r);
  if(unpack_bool(r)) {
    how=SIG_BLOCK;
    free_some(x1,1);
  }
  else {
    copy_some(x1,1);
    ASignal_Ama_Aunblock_(x1,r);
    if(unpack_bool(r)) {
      how=SIG_UNBLOCK;
      free_some(x1,1);
    }
    else {
      ASignal_Ama_Aset_(x1,r);
      if(unpack_bool(r)) {
        how=SIG_SETMASK;
      }
      else {
        HLT("hc_sigprocmask\'Signal: illegal maskaction constructor");
      }
    }
  }
  alloc_sigmask(r);
  if(sigprocmask(how,set,&(((SIGMASK)(r))->value))) {
    free_sigmask(r,1);
    return_unix_failure(errno);
  }
 return_okay(r);
}

extern OBJ _ASignal_Ahc_Asigpending(OBJ x1) /* hc_sigpending */
{OBJ r;
  alloc_sigmask(r);
  if(sigpending(&(((SIGMASK)(r))->value))) {
    free_sigmask(r,1);
    return_unix_failure(errno);
  }
 return_okay(r);
}

extern OBJ _ASignal_Ahc_Asigsuspend(OBJ x1,OBJ x2) /* hc_sigsuspend */
{OBJ r;
  free_some(x2,1);
  (void)sigsuspend(&(((SIGMASK)(x1))->value));
  free_sigmask(x1,1);
 return_okay_nil;
}

static init_const_ASignal()
{
sigset_t tmpsigset;
 init_ACom();
 init_AUnixFailures();
 init_AOption();
 init_ANat();
 init_AProcessCtrl();
 /* __ASignal_Ahc_Asigemptyset */
 if(sigemptyset(&tmpsigset)) {
   /* this should never happen */
   HLT("init_const\'Signal: sigemptyset() returns nonzero value");
 }
 make_sigmask(&tmpsigset,__ASignal_Ahc_Asigemptyset);
 /* __ASignal_Ahc_Asigfillset */
 if(sigfillset(&tmpsigset)) {
   /* this should never happen */
   HLT("init_const\'Signal: sigfillset() returns nonzero value");
 }
 make_sigmask(&tmpsigset,__ASignal_Ahc_Asigfillset);
}
