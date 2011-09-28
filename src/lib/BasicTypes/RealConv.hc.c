/* hand-coded implementation part of RealConv */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include <unixconfig.h>

#include "Nat.oc.h"
#include "Int.oc.h"
#include "Real.oc.h"


#ifndef iszero
#define iszero(r) ((r) == 0.0)
#endif

/* the following do not include digits after point */
#define MAX_LEN_REAL_EXP 8
#define MAX_LEN_REAL_FLAT 333

static char* addFloatPart(char *b, double d, int maxprec, double *over);
static double normalize(double *d);


extern OBJ _ARealConv_Sq_O1(OBJ x1,OBJ x2) /* `,1 */
{OBJ r;
 REAL R = (REAL) x1;
 NAT  prec = unpack_nat(x2);   
 int aprec;
 double d = R->value;
 double over;

 double expo; int nolead = 0;
 int negative = 0;
 int intexpo;
 
 char *b, *p, *q;


 if(iszero(d))
   { free_real(x1,1);
     return make_denotation("0e+0");
   };

 b = (char *)  calloc(MAX_LEN_REAL_EXP + prec, sizeof(char));
 if(b == 0x0) 
   HLT("`'RealConv: not enough internal memory");
 if(d < 0)
   { d = -d;
     negative = 1;
   }
 expo = normalize(&d);   /* now 10 > d > 0 */

 /* fprintf(stderr, "nach normalisieren: d = %lf expo = %lf\n", d, expo); */
 

 p = b;
 if(negative)
   { *p++ = '-';}
 q = addFloatPart(p + 1, 10 * (d - floor(d)), prec, &over); 
 d = d + over; 
 if(d >= 10.0) 
   { d /= 10.0;
     expo += 1.0;
   };
 *p = '0' + floor(d);
 p = q;
 *p++ = 'e';
 *p++ = (expo < 0.0) ? '-' : '+' ;
 intexpo = (int) (fabs(expo) + 0.5);
 /* Annahme exp < 1000 */
 if(intexpo >= 100) 
   {*p++ = '0' + intexpo / 100; intexpo %= 100; nolead = 1;}
 if((intexpo >= 10) || nolead) 
   {*p++ = '0' + intexpo / 10; intexpo %= 10; nolead = 1;}
 *p++ = '0' + intexpo;
 
 r = make_denotation(b);
 free_real(x1,1);
 free(b);
 return r;}

/* add FloatPart of d to b
   return pointer to first character after number
   at beginning, 10 > d > 0 must hold
*/
static char* addFloatPart(char *b, double d, int maxprec, double *over)
  { char *p, *last;
    int aprec;

    p = b;
    if(!iszero(d))
      {*p++ = '.';}
    for(aprec = 0; (!iszero(d)) && aprec < maxprec; 
	                           aprec++, d = 10 * (d - floor(d)))
      { *p = '0' + floor(d);
/*	fprintf(stderr, "digit %c at %p\n", *p, p); */
	p++;
      };
    /* rounding */
/*    fprintf(stderr, "rounding?: %lf\n", d); */
    if(d < 5.0)         /* no rounding necessary */
      { *over = 0.0;
	return p; }
    last = p;
    do
      {p--;
/*       fprintf(stderr, "rounding: at %c %p\n", *p, p); */
       if(*p == '.') break;  /* responsibility is now by caller */
       if(*p < '9')         /* add 1 to character and stop */
	 { *p = *p + 1;
	   *over = 0.0;
	   return last;
	 }
       else
	 { *p = '0';       /* make '9' to '0' and continue with */
	 };                /* previous character */
     }while(1);
    *over = 1.0;
    return last;
  }
	  
    
/* multiply or divide by 10 until 1 <= d < 10 
   count these operations and return these
   d > 0 on entry
*/
static double normalize(double *d)    
{ double v = *d;
  int zwerg = 0;
  
  if(v < 1.0)
    {for(; v < 1.0; v *= 10.0)
       { zwerg--; };
   }
  else
    {for(; v >= 10.0; v /= 10.0)
       {zwerg++;}
   };
 /* fprintf(stderr, "normalized %lf to (%lf, %d)\n", *d, v, zwerg); */

 *d = v;
 return (double) zwerg;
}
    

extern OBJ _ARealConv_Sqq_O1(OBJ x1,OBJ x2) /* ``,1 */
{OBJ r;
 REAL R = (REAL) x1;
 NAT  prec = unpack_nat(x2);

 double d, v, o;
 int negative = 0;
 char *b,*p,*first,*last,*begin, c, *predot;

/* char buffer[500]; */

 d = R->value;
 if(iszero(d))
   { free_real(x1,1);
     return(make_denotation("0"));
   };

 if(d < 0)
   {negative = 1;
    d = -d;
  };
 b = (char *) calloc(MAX_LEN_REAL_FLAT + prec + 1, sizeof(char *)); 
/*  b = buffer; */
 if(b == 0x0) 
   HLT("`'RealConv: not enough internal memory");
 p = b + 1;  /* we need one extra character for uprounding */
 begin = p;
 if(negative)
   {*p++ = '-';};
 first = p;
 /* construct number reverted */
 if(iszero(floor(d))) 
   {*p++ ='0';}
 else
   {for(v = floor(d); !iszero(v); v = floor(v / 10))
      { *p++ = '0' + fmod(v, 10.0);
      }
  };
 last = p - 1;
 predot = last;

/* fprintf(stderr, "first points to %c  last points to %c\n", *first, *last);
 fprintf(stderr, "buffer: %.20s\n", begin);
*/
 /* now revert */
 for(; first < last; first++, last--)
   {c = *first; *first = *last; *last = c;
/* fprintf(stderr, "first points to %c  last points to %c\n", *first, *last);
 fprintf(stderr, "buffer: %.20s\n", begin);
*/
};


 addFloatPart(p, 10 * (d - floor(d)), prec, &o);
 if(!iszero(o))  /* rounding necessary */
   { while(*predot == '9')      /* find lowest significant non-'9' */
       { *predot-- = '0'; };
     if(isdigit(*predot))         /* found a number */
       { *predot = (*predot) + 1; }  /* increment it */
     else
       { begin--;           /* shift beginning one to the left */
	 if(*predot == '-')      /* if necessary shift minus */
	   { *begin = '-'; }
	 *predot = '1';    /* and add '1' in front */
       }
   }

 free_real(x1,1);
 r = make_denotation(begin);
 free(b); 
 return r;
}


extern OBJ _ARealConv_AasNat(OBJ x1) /* asNat */
{ OBJ r;
  REAL cand = (REAL)x1;
  if (cand->value < 0.0)
    HLT("asNat'RealConv: real negative");
  if (cand->value > (double)unpack_nat(__ANat_Amax))
    HLT("asNat'RealConv: real too large");
  r = pack_nat((NAT)floor(cand->value));
  free_real(x1,1);
  return r;
}

extern OBJ _ARealConv_AasInt(OBJ x1) /* asInt */
{ OBJ r;
  REAL cand = (REAL)x1;
  if (cand->value < 0.0){
    if (cand->value < (double)unpack_int(__AInt_Amin))
      HLT("asInt'RealConv: real too small");
    r = pack_int((INT)ceil(cand->value));
  } else {
    if (cand->value > (double)unpack_int(__AInt_Amax))
      HLT("asInt'RealConv: real too large");
    r = pack_int((INT)floor(cand->value));
  }
  free_real(x1,1);
  return r;
}

static init_const_ARealConv()
{}
