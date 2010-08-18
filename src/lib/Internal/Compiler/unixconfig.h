/* src/lib/Internal/Compiler/unixconfig.h.  Generated from unixconfig.h.in by configure.  */
/* Unix System Configuration */

/* include this header to access any standard C or system functionality */

#ifndef UNIXCONFIG_H_INCLUDED
#define UNIXCONFIG_H_INCLUDED

/* ------------------------------------------------------------------------ */
/* Basic Includes */

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have to include the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <string.h>
#ifdef HAVE_MEMORY_H
#  include <memory.h>
#endif
#include <errno.h>
#include <signal.h>
#include <stdio.h>


/* ------------------------------------------------------------------------ */
/* System types */

/* Define if you have the <sys/types.h> header file.  */
#define HAVE_SYS_TYPES_H 1

/* Define if you have the <limits.h> header file.  */
#define HAVE_LIMITS_H 1

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef uid_t  */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef gid_t */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef mode_t */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define to the type of elements in the array set by `getgroups'.
   Usually this is either `int' or `gid_t'.  */
#define GETGROUPS_T gid_t


#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif

#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif

/* ------------------------------------------------------------------------ */
/* User and Groups */

/* Define if you have <pwd.h> */
#define HAVE_PWD_H 1

/* Define if you have <grp.h> */
#define HAVE_GRP_H 1

#ifdef HAVE_PWD_H
#  include <pwd.h>
#endif

#ifdef HAVE_GRP_H
#  include <grp.h>
#endif

/* ------------------------------------------------------------------------ */
/* Processes */

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#define HAVE_SYS_WAIT_H 1


#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif

#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#ifndef EXIT_SUCCESS
#  define EXIT_SUCCESS (0)
#endif

#ifndef EXIT_FAILURE
#  define EXIT_FAILURE (1)
#endif


/* ------------------------------------------------------------------------ */
/* Time */

/* define if you have sys/time.h */
#define HAVE_SYS_TIME_H 1

/* define if time.h and sys/time may be both included */
#define TIME_WITH_SYS_TIME 1

/* define if you have sys/times.h */
#define HAVE_SYS_TIMES_H 1

/* define if you have utime.h */
#define HAVE_UTIME_H 1

/* Define if utime(file, NULL) sets file's timestamp to the present.  */
#define HAVE_UTIME_NULL 1


#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#ifdef HAVE_UTIME_H
# include <utime.h>
#endif

/* define if ANSI CLOCKS_PER_SEC isn't defined */
#ifndef CLOCKS_PER_SEC
#  define CLOCKS_PER_SEC 60
#endif

/* ------------------------------------------------------------------------ */
/* Stat */

/* define if you have sys/stat.h */
#define HAVE_SYS_STAT_H 1

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

/* ------------------------------------------------------------------------ */
/* Directorys */

/* define if you have <dirent.h> */
#define HAVE_DIRENT_H 1

/* define if you have <sys/ndir.h> instead */
/* #undef HAVE_SYS_NDIR_H */

/* define if you have <sys/dir.h> instead */
/* #undef HAVE_SYS_DIR_H */

/* define if you have <ndir.h> instead */
/* #undef HAVE_NDIR_H */


#ifdef HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# ifdef HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif


/* ------------------------------------------------------------------------ */
/* Floats */

/* Define if we have DBL_MAX in <float.h> */
#define HAVE_DBL_MAX 1

/* Define if we have MAXDOUBLE in <values.h> */
/* #undef HAVE_MAXDOUBLE */

#ifdef HAVE_DBL_MAX
#  include <float.h>
#else
#ifdef HAVE_MAXDOUBLE
#  include <values.h>
#  define DBL_MAX MAXDOUBLE
#  define DBL_MAX MAXDOUBLE
#  define FLT_MIN MINDOUBLE
#  define FLT_MIN MINDOUBLE
#else
#  define DBL_MIN 2.2250738585072014e-308
#  define DBL_MAX 1.7976931348623157e+308
#  define FLT_MIN 1.17549435e-38f
#  define FLT_MAX 3.40282347e+38f
#endif
#endif

#include <math.h>


/* ------------------------------------------------------------------------ */
/* Diverse Library Functions */

/* Define if you have the difftime function.  */
#define HAVE_DIFFTIME 1

/* Define if you have the dlopen function.  */
#define HAVE_DLOPEN 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the mkdir function.  */
#define HAVE_MKDIR 1

/* Define if you have the mkfifo function.  */
#define HAVE_MKFIFO 1

/* Define if you have the rmdir function.  */
#define HAVE_RMDIR 1

/* Define if you have the strdup function.  */
#define HAVE_STRDUP 1

/* Define if you have the strerror function.  */
#define HAVE_STRERROR 1

#ifndef HAVE_STRERROR
  extern char * ocsstrerror(int);
# define strerror(no) ocsstrerror(no)
#endif

/* Define if you dont have strerror but sys_errlist  */
/* #undef HAVE_SYS_ERRLIST */

/* Define if you have the strftime function.  */
#define HAVE_STRFTIME 1

/* Define if you have the strtod function.  */
#define HAVE_STRTOD 1

/* Define if you have the times function.  */
#define HAVE_TIMES 1

/* Define if you have the clock function.  */
#define HAVE_CLOCK 1

/* Define if getpgrp is the POSIX.1 version without arguments */
#define GETPGRP_VOID 1

/* Define if memcmp is 8-bit clean */
/* #undef HAVE_CLEAN_MEMCMP  */

#ifndef HAVE_CLEAN_MEMCMP
  extern int ocsmemcmp(const void *, const void *, size_t);
# define memcmp(p1,p2,n) ocsmemcmp(p1,p2,n)
#endif

#endif /* UNIXCONFIG_H_INCLUDED */
