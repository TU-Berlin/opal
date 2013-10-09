/* hand-coded implementation part of UserAndGroup */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>

/* import NULL */
/* #include <stdlib.h> */
/* import supplementary groups */
/* #include <unistd.h> */
/* import struct passwd */
/* #include <pwd.h> */
/* import struct group */
/* #include <grp.h> */

/* import Option */
#include "Option.oc.h"
/* import Commands */
#include "Com.oc.h"
/* import Failures */
#include "UnixFailures.oc.h"
/* import Arrays */
#include "Array.oc.h"


static OBJ convert_chararray(char **car)
{OBJ r;
 int asz;
  asz=0;
  while(car[asz]!=NULL){
    asz++;
  }
  if(asz==0){
    r=alloc_array(0);
  }
  else{
   int i;
    r=alloc_array(asz);
    for(i=0;i<asz;i++){
      data_array(r)[i]=make_denotation(car[i]);
    }
  }
 return r;}

extern OBJ _AUserAndGroup_Sl(OBJ x1,OBJ x2) /* < */
{OBJ r;
  /* use macro expansion entry */
  AUserAndGroup_Sl(x1,x2,r);
 return r;}

extern OBJ _AUserAndGroup_Se(OBJ x1,OBJ x2) /* = */
{OBJ r;
  /* use macro expansion entry */
  AUserAndGroup_Se(x1,x2,r);
 return r;}

extern OBJ _AUserAndGroup_Sl_O2(OBJ x1,OBJ x2) /* <,2 */
{OBJ r;
  /* use macro expansion entry */
  AUserAndGroup_Sl_O2(x1,x2,r);
 return r;}

extern OBJ _AUserAndGroup_Se_O2(OBJ x1,OBJ x2) /* =,2 */
{OBJ r;
  /* use macro expansion entry */
  AUserAndGroup_Se_O2(x1,x2,r);
 return r;}

extern OBJ _AUserAndGroup_Ahc_Agetpwuid(OBJ x1,OBJ x2) /* hc_getpwuid */
{OBJ r;
 struct passwd *pwp;
 OBJ tmpuser;
  free_some(x2,1);
  pwp=getpwuid(((USERID)(x1))->value);
  free_userid(x1,1);
  if(pwp==NULL){
    copy_some(__AOption_Anil,1);
    r=__AOption_Anil;
  }
  else{
   OBJ newuid;
   OBJ newgid;
    make_userid(pwp->pw_uid,newuid);
    make_groupid(pwp->pw_gid,newgid);
    AUserAndGroup_Auser(
     /* arguments */
     make_denotation(pwp->pw_name),
     newuid,
     newgid,
     make_denotation(pwp->pw_dir),
     make_denotation(pwp->pw_shell),
     /* result */
     tmpuser
    );
    AOption_Aavail(tmpuser,r);
  }
 return_okay(r);
}

extern OBJ _AUserAndGroup_Ahc_Agetpwnam(OBJ x1,OBJ x2) /* hc_getpwnam */
{OBJ r;
 struct passwd *pwp;
 OBJ tmpuser;
  free_some(x2,1);
  pwp=getpwnam((char*)data_denotation(x1));
  free_denotation(x1,1);
  if(pwp==NULL){
    copy_some(__AOption_Anil,1);
    r=__AOption_Anil;
  }
  else{
   OBJ newuid;
   OBJ newgid;
    make_userid(pwp->pw_uid,newuid);
    make_groupid(pwp->pw_gid,newgid);
    AUserAndGroup_Auser(
     /* arguments */
     make_denotation(pwp->pw_name),
     newuid,
     newgid,
     make_denotation(pwp->pw_dir),
     make_denotation(pwp->pw_shell),
     /* result */
     tmpuser
    );
    AOption_Aavail(tmpuser,r);
  }
 return_okay(r);
}

extern OBJ _AUserAndGroup_Ahc_Agetgrgid(OBJ x1,OBJ x2) /* hc_getgrgid */
{OBJ r;
 struct group *grp;
 OBJ tmpgroup;
  free_some(x2,1);
  grp=getgrgid(((GROUPID)(x1))->value);
  free_groupid(x1,1);
  if(grp==NULL){
    copy_some(__AOption_Anil,1);
    r=__AOption_Anil;
  }
  else{
   OBJ newgid;
    make_groupid(grp->gr_gid,newgid);
    AUserAndGroup_Ahc_Agroup(
     /* parameters */
     make_denotation(grp->gr_name),
     newgid,
     convert_chararray(grp->gr_mem),
     /* result */
     tmpgroup
    );
    AOption_Aavail(tmpgroup,r);
  }
 return_okay(r);
}

extern OBJ _AUserAndGroup_Ahc_Agetgrnam(OBJ x1,OBJ x2) /* hc_getgrnam */
{OBJ r;
 struct group *grp;
 OBJ tmpgroup;
  free_some(x2,1);
  grp=getgrnam((char*)data_denotation(x1));
  free_denotation(x1,1);
  if(grp==NULL){
    copy_some(__AOption_Anil,1);
    r=__AOption_Anil;
  }
  else{
   OBJ newgid;
    make_groupid(grp->gr_gid,newgid);
    AUserAndGroup_Ahc_Agroup(
     /* parameters */
     make_denotation(grp->gr_name),
     newgid,
     convert_chararray(grp->gr_mem),
     /* result */
     tmpgroup
    );
    AOption_Aavail(tmpgroup,r);
  }
 return_okay(r);
}

extern OBJ _AUserAndGroup_Ahc_Agetgroups(OBJ x1) /* hc_getgroups */
{OBJ r;
 int gasize;
  free_some(x1,1);
  gasize=getgroups(0,NULL);
  if(gasize<0){
    return_unix_failure(errno);
  }
  if(gasize==0){
    r=alloc_array(0);
  }
  else{
   gid_t *gidarray;
   int i;
    gidarray=(gid_t*)malloc_aux(sizeof(gid_t)*gasize);
    if(getgroups(gasize,gidarray)!=gasize){
      return_unix_failure(errno);
    }
    r=alloc_array(gasize);
    for(i=0;i<gasize;i++){
      make_groupid(gidarray[i],data_array(r)[i]);
    }
    free_aux(gidarray);
  }
 return_okay(r);
}

extern void init_AOption();
extern void init_AArray();
extern void init_ACom();
extern void init_AUnixFailures();

static void init_const_AUserAndGroup()
{
  init_AOption();
  init_AArray();
  init_ACom();
  init_AUnixFailures();
}
