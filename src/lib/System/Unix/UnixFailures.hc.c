/* hand-coded implementation part of UnixFailures */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/

/* don´t know why this is necessary, but it *does* help */
#ifdef OCS_CYGWIN
extern char *strerror(int);
#endif


#include <unixconfig.h>


#include "Com.h"


OBJ unix_failure_cache[cached_unix_failure_codes];

extern OBJ cache_unix_failure(int code){
    OBJ r;
    r = unix_failure_cache[code] = _ACom_Afail(make_denotation(strerror(code)));
    copy_structured(r,1);
    return r;
}

extern OBJ nocache_unix_failure(int code){
    return _ACom_Afail(make_denotation(strerror(code)));
}

static OBJ init_failure(code){
    return_unix_failure(code);
}


static init_const_AUnixFailures()
{ int i; OBJ notavail;
  init_ACom();

  for (i = 0; i < cached_unix_failure_codes; i++)
     unix_failure_cache[i] = NIL;

  __AUnixFailures_AsomeError  =
  	_ACom_Afail(make_denotation("operation failed, unknown reason"));

  __AUnixFailures_AioError  =
#ifdef EIO
	init_failure(EIO);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_ApermissionDenied  =
#ifdef EACCES
	init_failure(EACCES);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AinvalidArgument  =
#ifdef EINVAL
	init_failure(EINVAL);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AtooManyLinks  =
#ifdef EMLINK
	init_failure(EMLINK);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoSuchDeviceOrAddr  =
#ifdef ENXIO
	init_failure(ENXIO);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AbadAddress  =
#ifdef EFAULT
	init_failure(EFAULT);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AfileTableOverflow  =
#ifdef ENFILE
	init_failure(ENFILE);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AbrokenPipe  =
#ifdef EPIPE
	init_failure(EPIPE);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AargListToLong  =
#ifdef E2BIG
	init_failure(E2BIG);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AdeviceBusy  =
#ifdef EBUSY
	init_failure(EBUSY);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AtooManyOpenFiles  =
#ifdef EMFILE
	init_failure(EMFILE);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnameTooLong  =
#ifdef ENAMETOOLONG
	init_failure(ENAMETOOLONG);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AexecFormat  =
#ifdef ENOEXEC
	init_failure(ENOEXEC);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AfileExists  =
#ifdef EEXIST
	init_failure(EEXIST);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoTTY  =
#ifdef ENOTTY
	init_failure(ENOTTY);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AdirectoryNotEmpty  =
#ifdef ENOTEMPTY
	init_failure(ENOTEMPTY);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnotOwner  =
#ifdef EPERM
	init_failure(EPERM);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AbadFileNumber  =
#ifdef EBADF
	init_failure(EBADF);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AcrossDeviceLink  =
#ifdef EXDEV
	init_failure(EXDEV);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AfileTooLarge  =
#ifdef EFBIG
	init_failure(EFBIG);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_Adeadlock  =
#ifdef EDEADLK
	init_failure(EDEADLK);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoEntity  =
#ifdef ENOENT
	init_failure(ENOENT);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoChildren  =
#ifdef ECHILD
	init_failure(ECHILD);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoSuchDevice  =
#ifdef ENODEV
	init_failure(ENODEV);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoSpaceOnDevice  =
#ifdef ENOSPC
	init_failure(ENOSPC);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoRecordLocks  =
#ifdef ENOLCK
	init_failure(ENOLCK);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoSuchProcess  =
#ifdef ESRCH
	init_failure(ESRCH);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoMoreProcesses  =
#ifdef EAGAIN
	init_failure(EAGAIN);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnoDirectory  =
#ifdef ENOTDIR
	init_failure(ENOTDIR);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AillegalSeek  =
#ifdef ESPIPE
	init_failure(ESPIPE);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AinterruptedSystemCall  =
#ifdef EINTR
	init_failure(EINTR);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnotEnoughMemory  =
#ifdef ENOMEM
	init_failure(ENOMEM);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AisDirectory  =
#ifdef EISDIR
	init_failure(EISDIR);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AreadOnlyFileSys  =
#ifdef EROFS
	init_failure(EROFS);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif

  __AUnixFailures_AnotImplemented  =
#ifdef ENOSYS
	init_failure(ENOSYS);
#else
	(copy_structured(__AUnixFailures_AsomeError,1),__AUnixFailures_AsomeError);
#endif


}
