/* hand-coded implementation part of Char */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 15:59:57 $ ($Revision: 1.1.1.1 $)
*/
#include <unixconfig.h>

extern OBJ _AChar_Sle(OBJ x1,OBJ x2) /* <= */
{OBJ r;
 AChar_Sle(x1,x2,r);
 return r;}

extern OBJ _AChar_Se(OBJ x1,OBJ x2) /* = */
{OBJ r;
 AChar_Se(x1,x2,r);
 return r;}

extern OBJ _AChar_Sge(OBJ x1,OBJ x2) /* >= */
{OBJ r;
 AChar_Sge(x1,x2,r);
 return r;}

extern OBJ _AChar_Sl(OBJ x1,OBJ x2) /* < */
{OBJ r;
 AChar_Sl(x1,x2,r);
 return r;}

extern OBJ _AChar_Sg(OBJ x1,OBJ x2) /* > */
{OBJ r;
 AChar_Sg(x1,x2,r);
 return r;}

extern OBJ _AChar_SSe(OBJ x1,OBJ x2) /* |= */
{OBJ r;
 AChar_SSe(x1,x2,r);
 return r;}

extern OBJ _AChar_Alower_(OBJ x1) /* lower? */
{OBJ r;
 AChar_Alower_(x1,r);
 return r;}

extern OBJ _AChar_Aupper_(OBJ x1) /* upper? */
{OBJ r;
 AChar_Aupper_(x1,r);
 return r;}

extern OBJ _AChar_Acontrol_(OBJ x1) /* control? */
{OBJ r;
 AChar_Acontrol_(x1,r);
 return r;}

extern OBJ _AChar_Apunctuation_(OBJ x1) /* punctuation? */
{OBJ r;
 AChar_Apunctuation_(x1,r);
 return r;}

extern OBJ _AChar_Aletter_(OBJ x1) /* letter? */
{OBJ r;
 AChar_Aletter_(x1,r);
 return r;}

extern OBJ _AChar_Adigit_(OBJ x1) /* digit? */
{OBJ r;
 AChar_Adigit_(x1,r);
 return r;}

extern OBJ _AChar_Awhitespace_(OBJ x1) /* whitespace? */
{OBJ r;
 AChar_Awhitespace_(x1,r);
 return r;}

extern OBJ _AChar_Aprintable_(OBJ x1) /* printable? */
{OBJ r;
 AChar_Aprintable_(x1,r);
 return r;}

extern OBJ _AChar_Alower(OBJ x1) /* lower */
{OBJ r;
 AChar_Alower(x1,r);
 return r;}

extern OBJ _AChar_Aupper(OBJ x1) /* upper */
{OBJ r;
 AChar_Aupper(x1,r);
 return r;}

extern OBJ _AChar_AuncheckedPred(OBJ x1) /* uncheckedPred */
{OBJ r;
 AChar_AuncheckedPred(x1,r);
 return r;}

extern OBJ _AChar_AuncheckedSucc(OBJ x1) /* uncheckedSucc */
{OBJ r;
 AChar_AuncheckedSucc(x1,r);
 return r;}

extern OBJ _AChar_S1(OBJ x1) /* ! */
{ OBJ r; 
  int l =  leng_denotation(x1); 
  if (l == 0) HLT("!'Char: denotation is empty");
  if (l > 1)  HLT("!'Char: extra characters in denotation");
  r = pack_char((CHAR)data_denotation(x1)[0]);
  free_denotation(x1,1);
  return r;
}

extern OBJ _AChar_AdenCharCode(OBJ d) /* denCharCode */ {
    /* no checks needed, used only locally */
    CHAR c1 = data_denotation(d)[0], c2 = data_denotation(d)[1];
    free_denotation(d,1);
    if (c1 == '\\'){
      switch(c2){
      case 'a' : return pack_char('\a');
      case 'b' : return pack_char('\b');
      case 'f' : return pack_char('\f');
      case 'n' : return pack_char('\n');
      case 'r' : return pack_char('\r');
      case 't' : return pack_char('\t');
      case 'v' : return pack_char('\v');
      default  : return pack_char(c2);
      }
    }else{
      return pack_char(c1);
    }
}


static init_const_AChar()
{
 __AChar_Amin = pack_char(0);
 __AChar_Amax = pack_char(UCHAR_MAX);
}

