/* hand-coded implementation part of Time */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>

/* import NULL */
/* #include <stdlib.h> */
/* #include <stdio.h> */

/* import Option */
#include "Option.oc.h"
/* import Nats */
#include "Nat.oc.h"
/* import Reals */
#include "Real.oc.h"
/* import Commands */
#include "Com.oc.h"


extern OBJ _ATime_Sl(OBJ x1,OBJ x2) /* < */
{OBJ r;
  /* use macro expansion entry */
  ATime_Sl(x1,x2,r);
 return r;}

extern OBJ _ATime_Sle(OBJ x1,OBJ x2) /* <= */
{OBJ r;
  /* use macro expansion entry */
  ATime_Sle(x1,x2,r);
 return r;}

extern OBJ _ATime_Se(OBJ x1,OBJ x2) /* = */
{OBJ r;
  /* use macro expansion entry */
  ATime_Se(x1,x2,r);
 return r;}

extern OBJ _ATime_SSe(OBJ x1,OBJ x2) /* |= */
{OBJ r;
  /* use macro expansion entry */
  ATime_SSe(x1,x2,r);
 return r;}

extern OBJ _ATime_Sge(OBJ x1,OBJ x2) /* >= */
{OBJ r;
  /* use macro expansion entry */
  ATime_Sge(x1,x2,r);
 return r;}

extern OBJ _ATime_Sg(OBJ x1,OBJ x2) /* > */
{OBJ r;
  /* use macro expansion entry */
  ATime_Sg(x1,x2,r);
 return r;}

extern OBJ _ATime_Sm(OBJ x1,OBJ x2) /* - */
{OBJ r;
  /* use macro expansion entry */
  ATime_Sm(x1,x2,r);
 return r;}

extern OBJ _ATime_Ahc_Atime(OBJ x1) /* hc_time */
{OBJ r;
  free_some(x1,1);
  make_time(time((time_t*)NULL),r);
  return_okay(r);
}

extern OBJ _ATime_Ahc_AasTm(OBJ t1) /* hc_asTm */
{OBJ r;
 struct tm *tmp;
 OBJ tmp_wday;
 OBJ tmp_isdst;
  tmp=localtime(&(((TIME)(t1))->value));
  free_time(t1,1);
  switch(tmp->tm_wday){
  case 0:  copy_some(__ATime_Asun,1); tmp_wday=__ATime_Asun; break; /* Sun */
  case 1:  copy_some(__ATime_Amon,1); tmp_wday=__ATime_Amon; break; /* Mon */
  case 2:  copy_some(__ATime_Atue,1); tmp_wday=__ATime_Atue; break; /* Tue */
  case 3:  copy_some(__ATime_Awed,1); tmp_wday=__ATime_Awed; break; /* Wed */
  case 4:  copy_some(__ATime_Athu,1); tmp_wday=__ATime_Athu; break; /* Thu */
  case 5:  copy_some(__ATime_Afri,1); tmp_wday=__ATime_Afri; break; /* Fri */
  case 6:  copy_some(__ATime_Asat,1); tmp_wday=__ATime_Asat; break; /* Sat */
  default: HLT("hc_asTm\'Time: illegal tm_wday value");
  }
  if (tmp->tm_isdst==0){
    AOption_Aavail(pack_clean_bool(0),tmp_isdst);
  }
  else{
    if (tmp->tm_isdst>0){
      AOption_Aavail(pack_clean_bool(1),tmp_isdst);
    }
    else{
      copy_some(__AOption_Anil,1);
      tmp_isdst=__AOption_Anil;
    }
  }
  ATime_Atm(
   /* arguments */
   pack_nat(tmp->tm_sec),
   pack_nat(tmp->tm_min),
   pack_nat(tmp->tm_hour),
   pack_nat(tmp->tm_mday),
   pack_nat(tmp->tm_mon+1),
   pack_nat(tmp->tm_year+1900),
   tmp_wday,
   pack_nat(tmp->tm_yday+1),
   tmp_isdst,
   /* result */
   r
  );
 return r;}

extern void init_ANat();
extern void init_AReal();
extern void init_AOption();
extern void init_ACom();

static void init_const_ATime()
{
  init_ANat();
  init_AReal();
  init_AOption();
  init_ACom();
}
