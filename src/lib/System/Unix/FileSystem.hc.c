/* hand-coded implementation part of FileSystem */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 2001-05-23 18:56:25 $ ($Revision: 1.3 $)
*/

#include <unixconfig.h>

/* import NULL */
/* #include <stdlib.h> */
/* import prototypes */
/* #include <stdio.h> */
/* #include <unistd.h> */
/* import struct dirent */
/* #include <dirent.h> */
/* import string functions */
/* #include <string.h> */
/* import error codes */
/* #include <errno.h> */

/* import Users and Groups */
#include "UserAndGroup.h"
/* import Time */
#ifdef OCS_FILENAMES_CASEFOLD
#include "Time_ocs.h"
#else
#include "Time.h"
#endif
/* import Arrays */
#include "Array.h"
/* import Options */
#include "Option.h"
/* import Commands */
#include "Com.h"
/* import Failures */
#include "UnixFailures.h"


/* type for collecting directory entries */
typedef struct {
  char * entry;
  void * nxt;
} localdirent;


static mode_t convert_permission(OBJ operm)
{OBJ r;
  /* NOTE that this function keeps the number of operm references constant */
  copy_some(operm,1);
  AFileSystem_AsetUIdOnExec_(operm,r);
  if(unpack_bool(r)) return S_ISUID;
  copy_some(operm,1);
  AFileSystem_AsetGIdOnExec_(operm,r);
  if(unpack_bool(r)) return S_ISGID;
  copy_some(operm,1);
  AFileSystem_AstickyBit_(operm,r);
  /* non-POSIX; ignore if undefined */
#ifdef S_ISVTX
  if(unpack_bool(r)) return S_ISVTX;
#else
  if(unpack_bool(r)) return ((mode_t)0);
#endif
  copy_some(operm,1);
  AFileSystem_AownerRead_(operm,r);
  if(unpack_bool(r)) return S_IRUSR;
  copy_some(operm,1);
  AFileSystem_AownerWrite_(operm,r);
  if(unpack_bool(r)) return S_IWUSR;
  copy_some(operm,1);
  AFileSystem_AownerExec_(operm,r);
  if(unpack_bool(r)) return S_IXUSR;
  copy_some(operm,1);
  AFileSystem_AgroupRead_(operm,r);
  if(unpack_bool(r)) return S_IRGRP;
  copy_some(operm,1);
  AFileSystem_AgroupWrite_(operm,r);
  if(unpack_bool(r)) return S_IWGRP;
  copy_some(operm,1);
  AFileSystem_AgroupExec_(operm,r);
  if(unpack_bool(r)) return S_IXGRP;
  copy_some(operm,1);
  AFileSystem_AworldRead_(operm,r);
  if(unpack_bool(r)) return S_IROTH;
  copy_some(operm,1);
  AFileSystem_AworldWrite_(operm,r);
  if(unpack_bool(r)) return S_IWOTH;
  copy_some(operm,1);
  AFileSystem_AworldExec_(operm,r);
  if(unpack_bool(r)) return S_IXOTH;
  HLT("convert_permission'FileSystem: Internal error (unknown permission constructor)");
}

static OBJ convert_filetype(mode_t fmode)
{
  if(S_ISREG(fmode)) {
    copy_some(__AFileSystem_Aregular,1);
    return(__AFileSystem_Aregular);
  }
  if(S_ISDIR(fmode)) {
    copy_some(__AFileSystem_Adirectory,1);
    return(__AFileSystem_Adirectory);
  }
  if(S_ISCHR(fmode)) {
    copy_some(__AFileSystem_AcharSpecial,1);
    return(__AFileSystem_AcharSpecial);
  }
  if(S_ISBLK(fmode)) {
    copy_some(__AFileSystem_AblockSpecial,1);
    return(__AFileSystem_AblockSpecial);
  }
  if(S_ISFIFO(fmode)) {
    copy_some(__AFileSystem_Afifo,1);
    return(__AFileSystem_Afifo);
  }
#ifdef S_ISSOCK
  if(S_ISSOCK(fmode)) {
    copy_some(__AFileSystem_Asocket,1);
    return(__AFileSystem_Asocket);
  }
#endif
#ifdef S_ISLNK
  if(S_ISLNK(fmode)) {
    copy_some(__AFileSystem_AsymLink,1);
    return(__AFileSystem_AsymLink);
  }
#endif
  copy_some(__AFileSystem_Aunknown,1);
  return(__AFileSystem_Aunknown);
}

static OBJ convert_status(struct stat *stp)
{OBJ r;
 OBJ tmpfilemode;
 OBJ tmpinode;
 OBJ tmpdevice;
 OBJ tmpuserid;
 OBJ tmpgroupid;
 OBJ tmpatime;
 OBJ tmpmtime;
 OBJ tmpctime;
#ifdef S_ISVTX
  make_filemode(stp->st_mode &
                (S_IRWXU | S_IRWXG | S_IRWXO | S_ISUID | S_ISGID | S_ISVTX),
                tmpfilemode);
#else
  make_filemode(stp->st_mode &
                (S_IRWXU | S_IRWXG | S_IRWXO | S_ISUID | S_ISGID),
                tmpfilemode);
#endif
  make_inode(stp->st_ino,tmpinode);
  make_device(stp->st_dev,tmpdevice);
  make_userid(stp->st_uid,tmpuserid);
  make_groupid(stp->st_gid,tmpgroupid);
  make_time(stp->st_atime,tmpatime);
  make_time(stp->st_ctime,tmpctime);
  make_time(stp->st_mtime,tmpmtime);
  AFileSystem_Afilestat(
   /* parameters */
   convert_filetype(stp->st_mode),
   tmpfilemode,
   tmpinode,
   tmpdevice,
   pack_nat(stp->st_nlink),
   tmpuserid,
   tmpgroupid,
   pack_nat(stp->st_size),
   tmpatime,
   tmpctime,
   tmpmtime,
   /* result */
   r
  );
 return r;
}

extern OBJ _AFileSystem_Sl(OBJ x1,OBJ x2) /* < */
{OBJ r;
  /* use macro expansion entry */
  AFileSystem_Sl(x1,x2,r);
 return r;}

extern OBJ _AFileSystem_Se(OBJ x1,OBJ x2) /* = */
{OBJ r;
  /* use macro expansion entry */
  AFileSystem_Se(x1,x2,r);
 return r;}

extern OBJ _AFileSystem_Sl_O1(OBJ x1,OBJ x2) /* <,1 */
{OBJ r;
  /* use macro expansion entry */
  AFileSystem_Sl_O1(x1,x2,r);
 return r;}

extern OBJ _AFileSystem_Se_O1(OBJ x1,OBJ x2) /* =,1 */
{OBJ r;
  /* use macro expansion entry */
  AFileSystem_Se_O1(x1,x2,r);
 return r;}

extern OBJ _AFileSystem_Sl_O3(OBJ x1,OBJ x2) /* <,3 */
{OBJ r;
  /* use macro expansion entry */
  AFileSystem_Sl_O3(x1,x2,r);
 return r;}

extern OBJ _AFileSystem_Se_O3(OBJ x1,OBJ x2) /* =,3 */
{OBJ r;
  /* use macro expansion entry */
  AFileSystem_Se_O3(x1,x2,r);
 return r;}

extern OBJ _AFileSystem_Ahc_Anonemode(OBJ x1) /* hc_nonemode */
{OBJ r;
#ifdef S_ISVTX
  r=pack_clean_bool( ( (((FILEMODE)(x1))->value) &
                       ( S_IRWXU | S_IRWXG | S_IRWXO |
                         S_ISVTX | S_ISUID | S_ISGID)) 	==  (mode_t)0 );
#else
  r=pack_clean_bool( ( (((FILEMODE)(x1))->value) &
                       ( S_IRWXU | S_IRWXG | S_IRWXO |
                         S_ISUID | S_ISGID)) 		==  (mode_t)0 );
#endif
  free_filemode(x1,1);
 return r;}

extern OBJ _AFileSystem_Ahc_Aaddmode(OBJ x1,OBJ x2) /* hc_addmode */
{OBJ r;
 mode_t md;
  md=convert_permission(x2);
  free_some(x2,1);
  if(excl_filemode(x1,1)) {
    r=x1;
  }
  else {
    decr_filemode(x1,1);
    alloc_filemode(r);
  }
  ((FILEMODE)(r))->value = ((FILEMODE)(x1))->value | md;
 return r;}

extern OBJ _AFileSystem_Ahc_Adelmode(OBJ x1,OBJ x2) /* hc_delmode */
{OBJ r;
 mode_t md;
  md=convert_permission(x2);
  free_some(x2,1);
  if(excl_filemode(x1,1)) {
    r=x1;
  }
  else {
    decr_filemode(x1,1);
    alloc_filemode(r);
  }
  ((FILEMODE)(r))->value = ((FILEMODE)(x1))->value & (~md);
 return r;}

extern OBJ _AFileSystem_Ahc_Aormode(OBJ x1,OBJ x2) /* hc_ormode */
{OBJ r;
  if(excl_filemode(x1,1)) {
    r=x1;
  }
  else {
    decr_filemode(x1,1);
    alloc_filemode(r);
  }
  ((FILEMODE)(r))->value = ((FILEMODE)(x1))->value | ((FILEMODE)(x2))->value;
  free_filemode(x2,1);
 return r;}

extern OBJ _AFileSystem_Ahc_Aandnegmode(OBJ x1,OBJ x2) /* hc_andnegmode */
{OBJ r;
  if(excl_filemode(x1,1)) {
    r=x1;
  }
  else {
    decr_filemode(x1,1);
    alloc_filemode(r);
  }
  ((FILEMODE)(r))->value = ((FILEMODE)(x1))->value &
                           ~(((FILEMODE)(x2))->value);
  free_filemode(x2,1);
 return r;}

extern OBJ _AFileSystem_Ahc_Aandmode(OBJ x1,OBJ x2) /* hc_andmode */
{OBJ r;
  if(excl_filemode(x1,1)) {
    r=x1;
  }
  else {
    decr_filemode(x1,1);
    alloc_filemode(r);
  }
  ((FILEMODE)(r))->value = ((FILEMODE)(x1))->value & ((FILEMODE)(x2))->value;
  free_filemode(x2,1);
 return r;}

extern OBJ _AFileSystem_Ahc_Ainmode(OBJ x1,OBJ x2) /* hc_inmode */
{OBJ r;
 mode_t md;
  md=convert_permission(x1);
  free_some(x1,1);
  r=pack_bool(((FILEMODE)(x2))->value & md);
  free_filemode(x2,1);
 return r;}

extern OBJ _AFileSystem_Ahc_Astat(OBJ x1,OBJ x2) /* hc_stat */
{OBJ r;
 struct stat stb;
 int ststat;
  free_some(x2,1);
  ststat=stat(data_denotation(x1),&stb);
  free_denotation(x1,1);
  if(ststat) {
    return_unix_failure(errno);
  }
  r=convert_status(&stb);
 return_okay(r);
}

extern OBJ _AFileSystem_Ahc_Alink(OBJ x1,OBJ x2,OBJ x3) /* hc_link */
{int lnstat;
  free_some(x3,1);
  lnstat=link(data_denotation(x1),data_denotation(x2));
  free_denotation(x1,1);
  free_denotation(x2,1);
  if(lnstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
}

extern OBJ _AFileSystem_Ahc_Aunlink(OBJ x1,OBJ x2) /* hc_unlink */
{int ulstat;
  free_some(x2,1);
  ulstat=unlink(data_denotation(x1));
  free_denotation(x1,1);
  if(ulstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
}

extern OBJ _AFileSystem_Ahc_Arename(OBJ x1,OBJ x2,OBJ x3) /* hc_rename */
{int rnstat;
  free_some(x3,1);
  rnstat=rename(data_denotation(x1),data_denotation(x2));
  free_denotation(x1,1);
  free_denotation(x2,1);
  if(rnstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
}

extern OBJ _AFileSystem_Ahc_Autime(OBJ x1,OBJ x2,OBJ x3,OBJ x4) /* hc_utime */
{
#ifndef HAVE_UTIME_H
  return_fail(__AUnixFailures_AnotImplemented);
#else
struct utimbuf utb;
 OBJ r;
 int utstat;
  free_some(x4,1);
  copy_some(x2,1);
  AOption_Anil_(x2,r);
  if(unpack_bool(r)) {
    free_some(x2,1);
    copy_some(x3,1);
    AOption_Anil_(x3,r);
    if(unpack_bool(r)) {
      free_some(x3,1);
# ifdef HAVE_UTIME_NULL
      utstat=utime(data_denotation(x1),NULL);
# else 
      return_fail(__AUnixFailures_AnotImplemented);
# endif
    }
    else {
      HLT("hc_utime\'FileSystem: Atime is nil but mtime is available");
    }
  }
  else {
    AOption_Acont(x2,r);
    utb.actime = ((TIME)(r))->value;
    free_time(r,1);
    copy_some(x3,1);
    AOption_Anil_(x3,r);
    if(unpack_bool(r)) {
      HLT("hc_utime\'FileSystem: Atime is available but mtime is nil");
    }
    AOption_Acont(x3,r);
    utb.modtime = ((TIME)(r))->value;
    free_time(r,1);
    utstat=utime(data_denotation(x1),&utb);
  }
  free_denotation(x1,1);
  if(utstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
#endif
}

extern OBJ _AFileSystem_Ahc_Achmod(OBJ x1,OBJ x2,OBJ x3) /* hc_chmod */
{mode_t md;
 int cmstat;
  free_some(x3,1);
  md=((FILEMODE)(x2))->value;
  free_filemode(x2,1);
  cmstat=chmod(data_denotation(x1),md);
  free_denotation(x1,1);
  if(cmstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
}

extern OBJ _AFileSystem_Ahc_Achown(OBJ x1,OBJ x2,OBJ x3,OBJ x4) /* hc_chown */
{uid_t usr;
 gid_t grp;
 OBJ r;
 int costat;
  free_some(x4,1);
  copy_some(x2,1);
  AOption_Anil_(x2,r);
  if(unpack_bool(r)) {
    usr= -1;
    free_some(x2,1);
  }
  else {
    AOption_Acont(x2,r);
    usr=((USERID)(r))->value;
    free_userid(r,1);
  }
  copy_some(x3,1);
  AOption_Anil_(x3,r);
  if(unpack_bool(r)) {
    grp= -1;
    free_some(x3,1);
  }
  else {
    AOption_Acont(x3,r);
    grp=((GROUPID)(r))->value;
    free_groupid(r,1);
  }
  costat=chown(data_denotation(x1),usr,grp);
  free_denotation(x1,1);
  if(costat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
}

extern OBJ _AFileSystem_Ahc_Amkdir(OBJ x1,OBJ x2,OBJ x3) /* hc_mkdir */
{
#ifndef HAVE_MKDIR
 free_some(x1, 1); free_some(x2, 1);
 return_fail(__AUnixFailures_AnotImplemented);
#else
 mode_t md;
 int mdstat;
  free_some(x3,1);
  md=((FILEMODE)(x2))->value;
  free_filemode(x2,1);
  mdstat=mkdir(data_denotation(x1),md);
  free_denotation(x1,1);
  if(mdstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
#endif
}

extern OBJ _AFileSystem_Ahc_Armdir(OBJ x1,OBJ x2) /* hc_rmdir */
{ 
#ifndef HAVE_RMDIR
  free_some(x1, 1);
#else
  int rmstat;
  free_some(x2,1);
  rmstat=rmdir(data_denotation(x1));
  free_denotation(x1,1);
  if(rmstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
#endif
}

extern OBJ _AFileSystem_Ahc_Areaddir(OBJ x1,OBJ x2) /* hc_readdir */
{DIR *dirp;
 struct dirent *dep;
 localdirent *ldeh, *tmp;
 localdirent *ldep;
 int direntries;
 int i;
 OBJ r;
  free_some(x2,1);
  dirp=opendir(data_denotation(x1));
  free_denotation(x1,1);
  if(dirp==NULL) {
    return_unix_failure(errno);
  }
  direntries = 0;
  ldeh = NULL;
  errno = 0;
  while((dep=readdir(dirp))!=NULL) {
    if(ldeh==NULL) {
      ldeh=ldep=malloc_aux(sizeof(localdirent));
    }
    else {
      ldep->nxt=malloc_aux(sizeof(localdirent));
      ldep = ldep->nxt;
    }
    ldep->entry=malloc_aux(strlen(dep->d_name)+1);
    strcpy(ldep->entry,dep->d_name);
    ldep->nxt = NULL;
    direntries++;
  }
  if(errno!=0) {
   int orgerr;
    orgerr=errno;
    (void)closedir(dirp);
    return_unix_failure(orgerr);
  }
  if(closedir(dirp)) {
    return_unix_failure(errno);
  }
  r=alloc_array(direntries);
  i=0;
  while(ldeh!=NULL){
    data_array(r)[i]=make_denotation(ldeh->entry);
    free_aux(ldeh->entry);
    tmp=ldeh;
    ldeh=ldeh->nxt;
    free_aux(tmp);
    i++;
  }
 return_okay(r);
}

extern OBJ _AFileSystem_Ahc_Asymlink(OBJ x1,OBJ x2,OBJ x3) /* hc_symlink */
{int slstat;
  free_some(x3,1);
  slstat=symlink(data_denotation(x1),data_denotation(x2));
  free_denotation(x1,1);
  free_denotation(x2,1);
  if(slstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
}

extern OBJ _AFileSystem_Ahc_Areadlink(OBJ x1,OBJ x2) /* hc_readlink */
{int rlstat;
 OBJ r;
  free_some(x2,1);
  rlstat=readlink(data_denotation(x1),charbuf,CHARBUFSIZE);
  free_denotation(x1,1);
  if(rlstat<0) {
    return_unix_failure(errno);
  }
  charbuf[rlstat]=0;
  r=make_denotation(charbuf);
 return_okay(r);
}

extern OBJ _AFileSystem_Ahc_Amkfifo(OBJ x1,OBJ x2,OBJ x3) /* hc_mkfifo */
{
#ifndef HAVE_MKFIFO
 free_some(x1, 1); free_some(x2, 1); 
 return_fail(__AUnixFailures_AnotImplemented);
#else
 mode_t md;
 int mfstat;
  free_some(x3,1);
  md=((FILEMODE)(x2))->value;
  free_filemode(x2,1);
  mfstat=mkfifo(data_denotation(x1),md);
  free_denotation(x1,1);
  if(mfstat) {
    return_unix_failure(errno);
  }
 return_okay_nil;
#endif
}

extern OBJ _AFileSystem_Ahc_Atmpnam(OBJ x1) /* hc_tmpnam */
{char *tmpn;
  free_some(x1,1);
  errno=0;
  if((tmpn=tmpnam(NULL))==NULL) {
    if(errno==0) {
      return_fail(__AUnixFailures_AsomeError);
    }
    else {
      return_unix_failure(errno);
    }
  }
  return_okay( make_denotation(tmpn) );
}

static init_const_AFileSystem()
{
 init_AUserAndGroup();
 init_ATime();
 init_AArray();
 init_AOption();
 init_ACom();
 init_AUnixFailures();
 /* __AFileSystem_Ahc_Aemptymode */
 make_filemode((mode_t)0,__AFileSystem_Ahc_Aemptymode);
}
