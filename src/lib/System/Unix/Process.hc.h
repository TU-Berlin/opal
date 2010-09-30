/* hand-coded interface part of Process */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
  /* representation */

#define pack_process(p) pack_word(p)
#define unpack_process(p) unpack_word(p)

  /* macro based implementations */

#define AProcess_Aself_(x1,x2) {x2=pack_clean_bool(unpack_process(x1) == 0);}
