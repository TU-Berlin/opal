/* hand-coded interface part of Time */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

/* import time_t */
#include <unixconfig.h>

/* import Reals */
#include "Real.h"


/* DATA time == */

typedef struct sTIME {
  struct sCELL header;
  time_t       value;
} * TIME;

#define size_time          sizeof_small(struct sTIME)
#define alloc_time(t)      alloc_small_flat(size_time,t)
#define make_time(v,t)     {alloc_time(t);((TIME)(t))->value=v;}
#define copy_time(t,n)     copy_structured(t,n)
#define free_time(t,n)     free_structured(t,n)
#define excl_time(t,n)     excl_structured(t,n)
#define decr_time(t,n)     decr_structured(t,n)

/* macro based implementations */

/* < */
#define ATime_Sl(t1,t2,r) {\
  r=pack_clean_bool(((TIME)(t1))->value < ((TIME)(t2))->value);\
  free_time(t1,1); free_time(t2,1);\
}
/* <= */
#define ATime_Sle(t1,t2,r) {\
  r=pack_clean_bool(((TIME)(t1))->value <= ((TIME)(t2))->value);\
  free_time(t1,1); free_time(t2,1);\
}
/* = */
#define ATime_Se(t1,t2,r) {\
  r=pack_clean_bool(((TIME)(t1))->value == ((TIME)(t2))->value);\
  free_time(t1,1); free_time(t2,1);\
}
/* |= */
#define ATime_SSe(t1,t2,r) {\
  r=pack_clean_bool(((TIME)(t1))->value != ((TIME)(t2))->value);\
  free_time(t1,1); free_time(t2,1);\
}
/* >= */
#define ATime_Sge(t1,t2,r) {\
  r=pack_clean_bool(((TIME)(t1))->value >= ((TIME)(t2))->value);\
  free_time(t1,1); free_time(t2,1);\
}
/* > */
#define ATime_Sg(t1,t2,r) {\
  r=pack_clean_bool(((TIME)(t1))->value > ((TIME)(t2))->value);\
  free_time(t1,1); free_time(t2,1);\
}

/* - */
#ifndef HAVE_DIFFTIME
#define ATime_Sm(t1,t2,r) {\
  make_real((double) (((TIME)(t1))->value - ((TIME)(t2))->value) ,r);\
  free_time(t1,1); free_time(t2,1);\
}
#else
#define ATime_Sm(t1,t2,r) {\
  make_real(difftime(((TIME)(t1))->value,((TIME)(t2))->value),r);\
  free_time(t1,1); free_time(t2,1);\
}
#endif

/* hc_time */
/* no macro expansion entry */

/* hc_asTm */
/* no macro expansion entry */
