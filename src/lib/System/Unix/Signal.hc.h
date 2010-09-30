/* hand-coded interface part of Signal */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

/* import sigset_t */
/* import memcpy(), memcmp() */
#include <unixconfig.h>


/* DATA sigmask == */

typedef struct sSIGMASK {
  struct sCELL header;
  sigset_t     value;
} * SIGMASK;

#define size_sigmask          sizeof_small(struct sSIGMASK)
#define alloc_sigmask(s)      alloc_small_flat(size_sigmask,s)
#define make_sigmask(v,s)     {alloc_sigmask(s);\
                              memcpy(&(((SIGMASK)(s))->value),v,size_sigmask);}
#define copy_sigmask(s,n)     copy_structured(s,n)
#define free_sigmask(s,n)     free_structured(s,n)
#define excl_sigmask(s,n)     excl_structured(s,n)
#define decr_sigmask(s,n)     decr_structured(s,n)

/* macro based implementations */

/* < : sigmask ** sigmask -> bool */
#define ASignal_Sl_O1(s1,s2,r) {\
  r=pack_clean_bool(memcmp(&(((SIGMASK)(s1))->value),\
                           &(((SIGMASK)(s2))->value),size_sigmask)<0);\
  free_sigmask(s1,1); free_sigmask(s2,1);\
}
/* = : sigmask ** sigmask -> bool */
#define ASignal_Se_O1(s1,s2,r) {\
  r=pack_clean_bool(memcmp(&(((SIGMASK)(s1))->value),\
                           &(((SIGMASK)(s2))->value),size_sigmask)==0);\
  free_sigmask(s1,1); free_sigmask(s2,1);\
}
