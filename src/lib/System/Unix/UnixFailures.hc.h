/* hand-coded interface part of UnixFailures */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 2001-05-30 17:48:01 $ ($Revision: 1.2 $)
*/
  /* errno and creating failure answers */

#include <unixconfig.h>
/* überflüssig 
extern int errno; */

#define cached_unix_failure_codes 128		 
extern OBJ unix_failure_cache[cached_unix_failure_codes];
extern OBJ cache_unix_failure(int), nocache_unix_failure(int);

#define get_unix_failure(code,r){\
    if (code < cached_unix_failure_codes){\
	r = unix_failure_cache[code];\
	if (r != NIL) { copy_structured(r,1); }\
		 else { r = cache_unix_failure(code); }\
    } else { \
	r = nocache_unix_failure(code);\
    }\
}

#define return_unix_failure(code) {\
    OBJ __r; get_unix_failure(code,__r); return __r;\
}
	
