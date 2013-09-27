/* hand-coded implementation part of String */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include <unixconfig.h>

#include "Nat.oc.h"


extern int get_string(OBJ string, char * buf, int bufsize){
    while (string != __AString_Slg){
        NAT start, cnt; OBJ data, rest;
	unpack_chunck_string(string, start,data,rest);
        cnt = leng_denotation(data) - start;
        if (cnt < bufsize){
            memcpy((void*)buf,(void*)(data_denotation(data)+start),cnt);
            buf += cnt;
            bufsize -= cnt;
            free_denotation(data,1);
            string = rest;
        } else {
            /* string doesn't fit completely */
            cnt = bufsize - 1;
            memcpy((void*)buf,(void*)(data_denotation(data)+start),cnt);
            *buf = 0;
            free_denotation(data,1);
            free_some(rest,1);
            return 0;
        }
    }
    *buf = 0;
    return 1;
}

/* compares two strigns; returns -1, 0, +1, 
   first is smaller, both are equal, first is larger

   frees strings itself
*/
static int cmpString(OBJ str1, OBJ str2)  
{	OBJ data1, rest1; NAT start1, leng1; char *d1; WORD l1, i1;
	OBJ data2, rest2; NAT start2, leng2; char *d2; WORD l2, i2;
	int delta;
	int newfirst = 1, newsecond = 1, breakflag, e1, e2;

  rest1 = str1; rest2 = str2;
  if( (!is_empty_string(str1)) && (!is_empty_string(str2)))
  {
  while(1)
  {	breakflag = 0;
	if(newfirst)
	{ str1 = rest1;
	  if(is_empty_string(str1)) 
	  { breakflag = 1; }
	  else
	  { unpack_chunck_string(str1, start1, data1, rest1);
  	    leng1 = leng_denotation(data1) - start1;
	    d1 = data_denotation(data1) + start1;
          }
        };
        if(newsecond)
	{ str2 = rest2;
	  if(is_empty_string(str2)) 
	  { breakflag = 1; }
	  else
	  { unpack_chunck_string(str2, start2, data2, rest2);
  	    leng2 = leng_denotation(data2) - start2;
	    d2 = data_denotation(data2) + start2;
          }
        };
/*
    fprintf(stderr, "now comparing(%d):\n", breakflag); fflush(stderr);
    fprintf(stderr, "den1:%s\n", data_denotation(data1)); fflush(stderr);
    fprintf(stderr, "start1:%d\n", start1); fflush(stderr);
    fprintf(stderr, "d1:%s\n", d1); fflush(stderr);
    fprintf(stderr, "leng1:%d\n", leng1); fflush(stderr);
    fprintf(stderr, "den2:%s\n", data_denotation(data2)); fflush(stderr);
    fprintf(stderr, "start2:%d\n", start2); fflush(stderr);
    fprintf(stderr, "d2:%s\n", d2); fflush(stderr);
    fprintf(stderr, "leng2:%d\n", leng2); fflush(stderr);
*/

	if(breakflag) break;
	if(leng1 < leng2)
	{  newfirst = 1; newsecond = 0;
	   delta = memcmp(d1, d2, leng1);

/*       fprintf(stderr, "leng1 < leng2: result: %d\n", delta); fflush(stderr);
*/
	   if(delta != 0)
	   {  free_denotation(data1, 1); free_some(rest1, 1);
	      free_denotation(data2, 1); free_some(rest2, 1);
	      return delta;
           }
	   else
	   {  d2 += leng1;
	      leng2 -= leng1;
           };
       continue;
        };
	if(leng1 == leng2)
	{ newfirst = newsecond = 1;
	  delta = memcmp(d1, d2, leng1);

/*       fprintf(stderr, "leng1 = leng2: result: %d\n", delta); fflush(stderr);
*/
	  if(delta != 0)
	   {  free_denotation(data1, 1); free_some(rest1, 1);
	      free_denotation(data2, 1); free_some(rest2, 1);
	      return delta;
           };
      continue;
        };
	if(leng1 > leng2)
	{  newfirst = 0; newsecond = 1;
	   delta = memcmp(d1, d2, leng2);

/*       fprintf(stderr, "leng1 > leng2: result: %d\n", delta); fflush(stderr);
*/
	   if(delta != 0)
	   {  free_denotation(data1, 1); free_some(rest1, 1);
	      free_denotation(data2, 1); free_some(rest2, 1);
	      return delta;
           }
	   else
	   {  d1 += leng2;
	      leng1 -= leng2;
           };
        };
  } /* while */
  /* ein string ist garantiert leer */
  e1 = e2 = 0;
  if(!is_empty_string(str1))
  {  free_denotation(data1, 1); free_some(rest1, 1);
     e1 = 1;
  };
  if(!is_empty_string(str2))
  {  free_denotation(data2, 1); free_some(rest2, 1);
     e2 = 1;
  };
  return (e1 - e2);
  }  /* then von aeusserem if */
  else
  {  e1 = e2 = 0;
     if(!is_empty_string(str1))
     {  FREE(str1, 1);
        e1 = 1;
     };
     if(!is_empty_string(str2))
     {  FREE(str2, 1);
        e2 = 1;
     };
     return (e1 - e2);
  };
  HLT("cmpString'String.hc.c: This cannot happen!");
  return 0;
}

extern OBJ _AString_Sl(OBJ x1,OBJ x2) /* < */
{
   return pack_bool( (cmpString(x1, x2)) < 0);
}

extern OBJ _AString_Sle(OBJ x1,OBJ x2) /* <= */
{
   return pack_bool( (cmpString(x1, x2)) <= 0);
}

extern OBJ _AString_Sge(OBJ x1,OBJ x2) /* >= */
{
   return pack_bool( (cmpString(x1, x2)) >= 0);
}

extern OBJ _AString_Sg(OBJ x1,OBJ x2) /* > */
{
   return pack_bool( (cmpString(x1, x2)) > 0);
}

extern OBJ _AString_Se(OBJ x1,OBJ x2) /* = */
{
   return pack_bool( (cmpString(x1, x2)) == 0);
}

extern OBJ _AString_SSe(OBJ x1,OBJ x2) /* |= */
{
   return pack_bool( (cmpString(x1, x2)) != 0);
}


static void init_const_AString()
{}

