/* hand-coded implementation part of ProcessCtrl */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>

/* import prototypes and NULL */
/* #include <stdlib.h> */
/* #include <unistd.h> */
/* #include <string.h> */
/* import clock_t */
/* #include <time.h> */
/* import struct tms */
/* #include <sys/times.h> */
/* import error macros */
/* #include <errno.h> */

/* import Failures */
#include "UnixFailures.h"
/* import Nats */
#include "Nat.h"
/* import Reals */
#include "Real.h"
/* import Option */
#include "Option.h"
/* import Arrays */
#include "Array.h"
/* import Commands */
#include "Com.h"
/* import Filesystem */
#include "FileSystem.h"
/* import Users and Groups */
#include "UserAndGroup.h"

#ifndef CLK_TCK
  #define CLK_TCK sysconf(_SC_CLK_TCK)
#endif



/* local procedure for copying an OPAL-array[denotation] to a char** C-array
   the OPAL-OBJ is only read, the referencee count must be handled by the 
   caller. The memory needed for the C-array is allocated here and must be
   freed (with free_aux) by the caller!
*/
char ** opal2c_array_deno(OBJ array)
{ WORD tmplen, i;
  char **tmpargv;

  tmplen = leng_array(array);
  tmpargv=(char**)malloc_aux((tmplen+1)*sizeof(char*));
  for(i = 0; i < tmplen; i++) {
    tmpargv[i] = data_denotation(data_array(array)[i]);
  };
  tmpargv[tmplen]=(char*)0;
  return tmpargv;
}

extern OBJ _AProcessCtrl_Sl(OBJ x1,OBJ x2) /* < */
{OBJ r;
  /* use macro expansion entry */
  AProcessCtrl_Sl(x1,x2,r);
 return r;}

extern OBJ _AProcessCtrl_Se(OBJ x1,OBJ x2) /* = */
{OBJ r;
  /* use macro expansion entry */
  AProcessCtrl_Se(x1,x2,r);
 return r;}

extern OBJ _AProcessCtrl_Ahc_Afork(OBJ x1) /* hc_fork */
{OBJ r;
 OBJ p;
 pid_t tmppid;
  free_some(x1,1);
  tmppid=fork();
  switch(tmppid)
  {
  case -1:
    return_unix_failure(errno);
  case 0:
    copy_some(__AProcessCtrl_Achild,1);
    r=__AProcessCtrl_Achild;
    break;
  default:
    make_process(tmppid,p);
    AProcessCtrl_Aparent(p,r);
  }
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Aexit(OBJ x1,OBJ x2) /* hc_exit */
{OBJ r;
 int tmpstat;
  free_some(x2,1);
  copy_some(x1,1);
  AProcessCtrl_Asuccess_(x1,r);
  if(unpack_bool(r)) {
    tmpstat=EXIT_SUCCESS;
    free_some(x1,1);
  }
  else {
    AProcessCtrl_Afailure_(x1,r);
    if(unpack_bool(r)) {
      tmpstat=EXIT_FAILURE;
    }
    else {
      HLT("hc_exit\'ProcessCtrl: Unknown status constructor");
    }
  }
  exit(tmpstat);
  /* never returns */
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Aexecv(OBJ x1,OBJ x2,OBJ x3) /* hc_execv */
{char **tmpargv;

  free_some(x3,1);
  tmpargv = opal2c_array_deno(x2);
  free_array(x2,1);
  (void)execv(data_denotation(x1),tmpargv);
  free_aux(tmpargv);
  free_denotation(x1,1);
  return_unix_failure(errno);
}

extern OBJ _AProcessCtrl_Ahc_Aexecve(OBJ x1,OBJ x2,OBJ x3,OBJ x4) /* hc_execve */
{char **tmpargv;
 char **tmpenv;
 int tmplen;
  free_some(x4,1);
  tmpargv = opal2c_array_deno(x2);
  tmpenv  = opal2c_array_deno(x3);
  free_array(x2,1);
  free_array(x3,1);
  (void)execve(data_denotation(x1),tmpargv,tmpenv);
  free_aux(tmpargv);
  free_aux(tmpenv);
  free_denotation(x1,1);
  return_unix_failure(errno);
}

extern OBJ _AProcessCtrl_Ahc_Aexecvp(OBJ x1,OBJ x2,OBJ x3) /* hc_execvp */
{char **tmpargv;

  free_some(x3,1);
  tmpargv = opal2c_array_deno(x2);
  free_array(x2,1);
  (void)execvp(data_denotation(x1),tmpargv);
  free_aux(tmpargv);
  free_denotation(x1,1);
  return_unix_failure(errno);
}

extern OBJ _AProcessCtrl_Ahc_Agetpid(OBJ x1) /* hc_getpid */
{OBJ r;
  free_some(x1,1);
  make_process(getpid(),r);
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Agetppid(OBJ x1) /* hc_getppid */
{OBJ r;
  free_some(x1,1);
  make_process(getppid(),r);
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Aclock(OBJ x1) /* hc_clock */
{
#ifndef HAVE_CLOCK
 return_fail(__AUnixFailures_AnotImplemented);
#else
 OBJ r;
 unsigned long tmpclk;
  free_some(x1,1);
  tmpclk=clock();
  if(tmpclk==(unsigned long)-1) {
    make_real(-1.0,r);
  }
  else {
    make_real(((double)tmpclk)/((double)CLK_TCK),r);
  }
 return_okay(r);
#endif
}

extern OBJ _AProcessCtrl_Ahc_Atimes(OBJ x1) /* hc_times */
{
#ifndef HAVE_TIMES
 return_fail(__AUnixFailures_AnotImplemented);
#else
 OBJ r;
 struct tms tmptms;
 unsigned long tmpuptm;
 OBJ tmpup, tmput, tmpst, tmpcut, tmpcst;
  free_some(x1,1);
  tmpuptm=times(&tmptms);
  if(tmpuptm==(unsigned long)-1) {
    return_unix_failure(errno);
  }
  make_real(((double)tmpuptm)/((double)CLK_TCK),tmpup);
  make_real(((double)tmptms.tms_utime)/((double)CLK_TCK),tmput);
  make_real(((double)tmptms.tms_stime)/((double)CLK_TCK),tmpst);
  make_real(((double)tmptms.tms_cutime)/((double)CLK_TCK),tmpcut);
  make_real(((double)tmptms.tms_cstime)/((double)CLK_TCK),tmpcst);
  AProcessCtrl_Ai_Atimes(
   /* parameters */
   tmpup,
   tmput,
   tmpst,
   tmpcut,
   tmpcst,
   /* result */
   r
  );
 return_okay(r);
#endif
}

extern OBJ _AProcessCtrl_Ahc_Apause(OBJ x1) /* hc_pause */
{
  pause();
  return_unix_failure(errno);
}

extern OBJ _AProcessCtrl_Ahc_Asleep(OBJ x1,OBJ x2) /* hc_sleep */
{OBJ r;
 unsigned int tmps;
  free_some(x2,1);
  tmps=sleep((unsigned int)(unpack_nat(x1)));
  r=pack_nat(tmps);
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Achdir(OBJ x1,OBJ x2) /* hc_chdir */
{int cdstat;
  free_some(x2,1);
  cdstat=chdir(data_denotation(x1));
  free_denotation(x1,1);
  if(cdstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
}

extern OBJ _AProcessCtrl_Ahc_Agetcwd(OBJ x1) /* hc_getcwd */
{OBJ r;
#ifndef HAVE_GETCWD
  return_fail(__AUnixFailures_AnotImplemented);
#else
 char *tmpcwd;
 int tmpsiz;
  free_some(x1,1);
  if(getcwd(charbuf,CHARBUFSIZE)==NULL) {
    if(errno!=ERANGE) {
      return_unix_failure(errno);
    }
    tmpsiz=CHARBUFSIZE*2;
    for(;;) {
      tmpcwd=(char*)malloc_aux(tmpsiz);
      if(tmpcwd==NULL) {
        HLT("hc_getcwd\'ProcessCtrl: Not enough memory");
      }
      if(getcwd(tmpcwd,tmpsiz)!=NULL) {
        r=make_denotation(tmpcwd);
        free_aux(tmpcwd);
        return_okay(r);
      }
      free_aux(tmpcwd);
      if(errno!=ERANGE) {
        return_unix_failure(errno);
      }
      tmpsiz *= 2;
    }
  }
  r=make_denotation(charbuf);
 return_okay(r);
#endif
}

extern OBJ _AProcessCtrl_Ahc_Aaccess(OBJ x1,OBJ x2,OBJ x3) /* hc_access */
{int amode;
 int acstat;
  free_some(x3,1);
  amode=F_OK;
  if((((FILEMODE)(x2))->value)&S_IRUSR) amode |= R_OK;
  if((((FILEMODE)(x2))->value)&S_IWUSR) amode |= W_OK;
  if((((FILEMODE)(x2))->value)&S_IXUSR) amode |= X_OK;
  free_filemode(x2,1);
  acstat=access(data_denotation(x1),amode);
  free_denotation(x1,1);
  if(acstat) {
    if(errno==EACCES) {
      copy_some(__ABUILTIN_Afalse,1);
      return_okay(__ABUILTIN_Afalse);
    }
    return_unix_failure(errno);
  }
  copy_some(__ABUILTIN_Atrue,1);
 return_okay(__ABUILTIN_Atrue);
}


extern OBJ _AProcessCtrl_Ahc_Aumask(OBJ x1,OBJ x2) /* hc_umask */
{OBJ r;
 mode_t tmpmsk;
  free_some(x2,1);
  tmpmsk=umask( ((FILEMODE)(x1))->value );
  if(excl_filemode(x1,1)) {
    r=x1;
  }
  else {
    decr_filemode(x1,1);
    alloc_filemode(r);
  }
  ((FILEMODE)(r))->value = tmpmsk;
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Agetuid(OBJ x1) /* hc_getuid */
{OBJ r;
  free_some(x1,1);
  make_userid(getuid(),r);  
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Ageteuid(OBJ x1) /* hc_geteuid */
{OBJ r;
  free_some(x1,1);
  make_userid(geteuid(),r);
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Asetuid(OBJ x1,OBJ x2) /* hc_setuid */
{int sustat;
  free_some(x2,1);
  sustat=setuid(((USERID)(x1))->value);
  free_userid(x1,1);
  if(sustat) {
    return_unix_failure(errno);
  }
 return_okay_nil;}

extern OBJ _AProcessCtrl_Ahc_Agetgid(OBJ x1) /* hc_getgid */
{OBJ r;
  free_some(x1,1);
  make_groupid(getgid(),r);
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Agetegid(OBJ x1) /* hc_getegid */
{OBJ r;
  free_some(x1,1);
  make_groupid(getegid(),r);
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Asetgid(OBJ x1,OBJ x2) /* hc_setgid */
{int sgstat;
  free_some(x2,1);
  sgstat=setgid(((GROUPID)(x1))->value);
  free_groupid(x1,1);
  if(sgstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;}

extern OBJ _AProcessCtrl_Ahc_Agetpgrp(OBJ x1) /* hc_getpgrp */
{OBJ r;
  free_some(x1,1);
#ifdef GETPGRP_VOID
  make_process((*(pid_t (*)(void))getpgrp)(),r);
#else
  make_process((*(pid_t (*)(pid_t))getpgrp)(getpid()),r);
#endif
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Asetpgid(OBJ x1,OBJ x2,OBJ x3) /* hc_setpgid */
{OBJ r;
 pid_t tmpproc;
 pid_t tmppgrp;
  free_some(x3,1);
  copy_some(x1,1);
  AOption_Aavail_(x1,r);
  if(unpack_bool(r)) {
    AOption_Acont(x1,r);
    tmpproc=((PROCESS)(r))->value;
    free_process(r,1);
  }
  else {
    tmpproc=0;
    free_some(x1,1);
  }
  copy_some(x2,1);
  AOption_Aavail_(x2,r);
  if(unpack_bool(r)) {
    AOption_Acont(x2,r);
    tmppgrp=((PROCESS)(r))->value;
    free_process(r,1);
  }
  else {
    tmppgrp=0;
    free_some(x2,1);
  }
  if(setpgid(tmpproc,tmppgrp)) {
    return_unix_failure(errno);
  }
 return_okay_nil;}

extern OBJ _AProcessCtrl_Ahc_Asetsid(OBJ x1) /* hc_setsid */
{OBJ r;
  free_some(x1,1);
  make_process(setsid(),r);
 return_okay(r);
}

extern OBJ _AProcessCtrl_Ahc_Agetlogin(OBJ x1) /* hc_getlogin */
{OBJ r;
  free_some(x1,1);
  r=make_denotation(getlogin());
 return_okay(r);
}

static init_const_AProcessCtrl()
{
 init_AUnixFailures();
 init_AOption();
 init_ANat();
 init_AReal();
 init_AArray();
 init_ACom();
 init_AFileSystem();
 init_AUserAndGroup();
}
