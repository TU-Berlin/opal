/* hand-coded implementation part of SysTime */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 2001-05-09 13:24:48 $ ($Revision: 1.1 $)
*/

#include <unixconfig.h>

/* import NULL */
/* #include <stdlib.h> */
/* #include <stdio.h> */

/* import Option */
#include "Option.h"
/* import Nats */
#include "Nat.h"
/* import Reals */
#include "Real.h"
/* import Commands */
#include "Com.h"


extern OBJ _ASysTime_Sl(OBJ x1,OBJ x2) /* < */
{OBJ r;
  /* use macro expansion entry */
  ASysTime_Sl(x1,x2,r);
 return r;}

extern OBJ _ASysTime_Sle(OBJ x1,OBJ x2) /* <= */
{OBJ r;
  /* use macro expansion entry */
  ASysTime_Sle(x1,x2,r);
 return r;}

extern OBJ _ASysTime_Se(OBJ x1,OBJ x2) /* = */
{OBJ r;
  /* use macro expansion entry */
  ASysTime_Se(x1,x2,r);
 return r;}

extern OBJ _ASysTime_SSe(OBJ x1,OBJ x2) /* |= */
{OBJ r;
  /* use macro expansion entry */
  ASysTime_SSe(x1,x2,r);
 return r;}

extern OBJ _ASysTime_Sge(OBJ x1,OBJ x2) /* >= */
{OBJ r;
  /* use macro expansion entry */
  ASysTime_Sge(x1,x2,r);
 return r;}

extern OBJ _ASysTime_Sg(OBJ x1,OBJ x2) /* > */
{OBJ r;
  /* use macro expansion entry */
  ASysTime_Sg(x1,x2,r);
 return r;}

extern OBJ _ASysTime_Sm(OBJ x1,OBJ x2) /* - */
{OBJ r;
  /* use macro expansion entry */
  ASysTime_Sm(x1,x2,r);
 return r;}

extern OBJ _ASysTime_Ahc_Atime(OBJ x1) /* hc_time */
{OBJ r;
  free_some(x1,1);
  make_time(time((time_t*)NULL),r);
  return_okay(r);
}

extern OBJ _ASysTime_Ahc_AasTm(OBJ t1) /* hc_asTm */
{OBJ r;
 struct tm *tmp;
 OBJ tmp_wday;
 OBJ tmp_isdst;
  tmp=localtime(&(((TIME)(t1))->value));
  free_time(t1,1);
  switch(tmp->tm_wday){
  case 0:  copy_some(__ASysTime_Asun,1); tmp_wday=__ASysTime_Asun; break; /* Sun */
  case 1:  copy_some(__ASysTime_Amon,1); tmp_wday=__ASysTime_Amon; break; /* Mon */
  case 2:  copy_some(__ASysTime_Atue,1); tmp_wday=__ASysTime_Atue; break; /* Tue */
  case 3:  copy_some(__ASysTime_Awed,1); tmp_wday=__ASysTime_Awed; break; /* Wed */
  case 4:  copy_some(__ASysTime_Athu,1); tmp_wday=__ASysTime_Athu; break; /* Thu */
  case 5:  copy_some(__ASysTime_Afri,1); tmp_wday=__ASysTime_Afri; break; /* Fri */
  case 6:  copy_some(__ASysTime_Asat,1); tmp_wday=__ASysTime_Asat; break; /* Sat */
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
  ASysTime_Atm(
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

static init_const_ASysTime()
{
  init_ANat();
  init_AReal();
  init_AOption();
  init_ACom();
}
