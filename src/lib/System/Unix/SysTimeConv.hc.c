/* hand-coded implementation part of SysTimeConv */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 2001-05-09 13:24:48 $ ($Revision: 1.1 $)
*/

/* import C string functions */
#include <unixconfig.h>
/* import SysTime */
#include "SysTime.h"


extern OBJ _ASysTimeConv_Ahc_Actime(OBJ x1) /* hc_ctime */
{OBJ r;
  (void)strcpy(charbuf,ctime(&(((TIME)(x1))->value)));
  free_time(x1,1);
  if(charbuf[0]){
    /* erase newline at end */
    charbuf[strlen(charbuf)-1]=0;
  }
  else{
    HLT("hc_ctime\'TimeConv: ctime() returns empty string");
  }
  r=make_denotation(charbuf);
 return r;}

extern OBJ _ASysTimeConv_Ahc_Astrftime(OBJ x1,OBJ x2) /* hc_strftime */
{OBJ r;
 struct tm *tmp;
 size_t tmpsize;
  tmp=localtime(&(((TIME)(x2))->value));
  free_time(x2,1);
  tmpsize=strftime(charbuf,sizeof(charbuf),data_denotation(x1),tmp);
  free_denotation(x1,1);
  if(tmpsize==0){
    HLT("hc_strftime\'TimeConv: strftime result too long");
  }
  r=make_denotation(charbuf);
 return r;}

static init_const_ASysTimeConv()
{
  init_ASysTime();
}
