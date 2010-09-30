/* hand-coded interface part of UserAndGroup */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/

/* import uid_t and gid_t */
#include <unixconfig.h>


/* DATA userid == */

typedef struct sUSERID {
  struct sCELL header;
  uid_t        value;
} * USERID;

#define size_userid          sizeof_small(struct sUSERID)
#define alloc_userid(u)      alloc_small_flat(size_userid,u)
#define make_userid(v,u)     {alloc_userid(u);((USERID)(u))->value=v;}
#define copy_userid(u,n)     copy_structured(u,n)
#define free_userid(u,n)     free_structured(u,n)
#define excl_userid(u,n)     excl_structured(u,n)
#define decr_userid(u,n)     decr_structured(u,n)

/* DATA groupid == */

typedef struct sGROUPID {
  struct sCELL header;
  gid_t        value;
} * GROUPID;

#define size_groupid          sizeof_small(struct sGROUPID)
#define alloc_groupid(g)      alloc_small_flat(size_groupid,g)
#define make_groupid(v,g)     {alloc_groupid(g);((GROUPID)(g))->value=v;}
#define copy_groupid(g,n)     copy_structured(g,n)
#define free_groupid(g,n)     free_structured(g,n)
#define excl_groupid(g,n)     excl_structured(g,n)
#define decr_groupid(g,n)     decr_structured(g,n)

/* macro based implementations */

/* < : userid ** userid -> bool */
#define AUserAndGroup_Sl(u1,u2,r) {\
  r=pack_clean_bool(((USERID)(u1))->value < ((USERID)(u2))->value);\
  free_userid(u1,1); free_userid(u2,1);\
}
/* = : userid ** userid -> bool */
#define AUserAndGroup_Se(u1,u2,r) {\
  r=pack_clean_bool(((USERID)(u1))->value == ((USERID)(u2))->value);\
  free_userid(u1,1); free_userid(u2,1);\
}

/* < : groupid ** groupid -> bool */
#define AUserAndGroup_Sl_O2(g1,g2,r) {\
  r=pack_clean_bool(((GROUPID)(g1))->value < ((GROUPID)(g2))->value);\
  free_groupid(g1,1); free_groupid(g2,1);\
}
/* = : groupid ** groupid -> bool */
#define AUserAndGroup_Se_O2(g1,g2,r) {\
  r=pack_clean_bool(((GROUPID)(g1))->value == ((GROUPID)(g2))->value);\
  free_groupid(g1,1); free_groupid(g2,1);\
}

/* hc_getpwuid */
/* no macro expansion entry */

/* hc_getpwnam */
/* no macro expansion entry */

/* hc_getgrgid */
/* no macro expansion entry */

/* hc_getgrnam */
/* no macro expansion entry */

/* hc_getgroups */
/* no macro expansion entry */
