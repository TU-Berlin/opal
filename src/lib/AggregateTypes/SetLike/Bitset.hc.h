/* hand-coded interface part of Bitset */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>

#include "Nat.oc.h"

  /* data representation */

typedef struct sBITSET {
    struct sBCELL big;
    /* ... data ... */
} * BITSET;

typedef WORD BITWORD;	

#define data_bitset(x) ((BITWORD *)data_big(x))
#define free_bitset(x,n) free_structured(x,n)
#define copy_bitset(x,n) copy_structured(x,n)
#define excl_bitset(x,n) excl_structured(x,n)
#define decr_bitset(x,n) decr_structured(x,n)

#define words_bitset(s) size_big(s)

extern OBJ alloc_bitset(int);
extern OBJ dup_bitset(OBJ,int);


#define bits_bitset (sizeof(BITWORD)*CHAR_BIT)
		/* we dont use bits_per_word to make this a constant */
#define mask_bitset(b) ((BITWORD)1 << (b))

  /* macro based implementations */

#define ABitset_Ain(e,s,r){\
	int __e = unpack_nat(e);\
	int __i = __e / bits_bitset, __b = __e % bits_bitset;\
	if (__i < words_bitset(s)){\
	    r = pack_bool(mask_bitset(__b) & data_bitset(s)[__i]);\
	} else { r = pack_clean_bool(0); }\
	free_bitset(s,1);\
}

#define ABitset_Aincl(e,s,r){\
	int __e = unpack_nat(e);\
	int __i = __e / bits_bitset,  __b = __e % bits_bitset;\
	if (excl_bitset(s,1) && __i < words_bitset(s)) {r=s;}\
	else {r=dup_bitset(s,__i);}\
	data_bitset(r)[__i] |= mask_bitset(__b);\
}

#define ABitset_Aexcl(e,s,r){\
	int __e = unpack_nat(e);\
	int __i = __e / bits_bitset, __b = __e % bits_bitset;\
	if (__i < words_bitset(s)){\
	   if (excl_bitset(s,1)) {r=s;}\
	   else {r=dup_bitset(s,__i);}\
	   data_bitset(r)[__i] &= ~(mask_bitset(__b));\
	} else {\
	    r=s;\
	}\
}

