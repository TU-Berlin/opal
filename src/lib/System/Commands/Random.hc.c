/* hand-coded implementation part of Random */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include <unixconfig.h>

#include "Nat.oc.h"
#include "Real.oc.h"
#include "Com.oc.h"
#include "Void.oc.h"

/* assuming integers are 32 bit */
static int internal_seed = 0.0;

static double internal_random(void);

extern OBJ _ARandom_Ax_Aseed_(OBJ unit) /* x_seed? : void -> ans[real] */
{ OBJ r;

 make_real((double) internal_seed, r);
 return_okay(r);
}

extern OBJ _ARandom_Ax_Aseed(OBJ x1,OBJ unit) /* x_seed */
{OBJ r;

/*  fprintf(stderr, "now in seed ..."); fflush(stderr); */
 internal_seed = (int) ((REAL)x1)->value;
/*  fprintf(stderr,"value: %lf  internal_seed: %d\n", ((REAL)x1)->value, internal_seed); fflush(stderr); */

 FREE(x1,1);

 return_okay(__AVoid_Anil);}

extern OBJ _ARandom_Arandom_Areal(OBJ unit) /* random_real */
{OBJ r;

 make_real(internal_random(), r);
 return_okay(r);
}

extern OBJ _ARandom_Arandom_Anat_Anat(OBJ x1,OBJ unit) /* random_nat_nat */
{
 NAT n = unpack_nat(x1);
 double x;

 x = internal_random();
 
 return_okay(pack_nat(x * n));}

extern OBJ _ARandom_Arandom_Anat(OBJ unit) /* random_nat */
{ double x;
 
  x = internal_random();

  return_okay(pack_nat(max_word * x));
}


static double internal_random(void)
{ static const int a = 16807;
  static const int m = 2147483647;
  static const int q = 127773; /* m div a */
  static const int r = 2836;   /* m mod a */
  int lo, hi, test;

  hi = internal_seed / q;
  lo = internal_seed % q;
  test = a * lo - r * hi;
  if(test > 0)
    {  internal_seed = test; }
  else 
    {  internal_seed = test + m; };
  return (double) internal_seed / m;
}
  
static void init_const_ARandom()
{
  internal_seed = (int) time(NULL);
}
