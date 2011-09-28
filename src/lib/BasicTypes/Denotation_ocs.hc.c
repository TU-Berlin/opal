/* hand-coded implementation part of Denotation */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include <unixconfig.h>
#include "Nat.h"
#include "Char.h"
#include "Int.h"


extern OBJ dup_denotation(OBJ d){
    OBJ r = alloc_denotation(leng_denotation(d));
    memcpy((void*)data_denotation(r),(void*)data_denotation(d),
	   leng_denotation(d));
    decr_denotation(d,1);
    return r;
}
  
extern OBJ _ADenotation_Ainit(OBJ x1,OBJ x2) /* init */{
    NAT l = unpack_nat(x1);
    OCSCHAR c = unpack_char(x2);
    OBJ r = alloc_denotation(l);
    while (l > 0){
	data_denotation(r)[--l] = c;
    }
    return r;
}
    
    
extern OBJ _ADenotation_Ainit_O1(OBJ x1,OBJ x2) /* init,1 */ {
    NAT l = unpack_nat(x1);
    OBJ r = alloc_denotation(l);
    while (l > 0){
	l--;
	copy_closure(x2,1);
	data_denotation(r)[l] = unpack_char( EVAL1(x2,pack_nat(l)) );
    }
    free_closure(x2,1);
    return r;
}

extern OBJ _ADenotation_S3(OBJ x1) /* # */
{OBJ r;
 ADenotation_S3(x1,r);
 return r;}

extern OBJ _ADenotation_Spp(OBJ x1,OBJ x2) /* ++ */ {
    NAT l1 = leng_denotation(x1), l2 = leng_denotation(x2);
    if (l1 == 0){
	free_denotation(x1,1);
	return x2;
    } else 
    if (l2 == 0){
	free_denotation(x2,1);
	return x1;
    } else {
    	OBJ r = alloc_denotation(l1 + l2);
	memcpy((void*)data_denotation(r),
       	       (void*)data_denotation(x1),l1);
    	memcpy((void*)(data_denotation(r) + l1),
                       (void*)data_denotation(x2),l2);
    	free_denotation(x1, 1); free_denotation(x2,1);
    	return r;
    }
}
    
extern OBJ _ADenotation_AuncheckedSel(OBJ x1,OBJ x2) /* uncheckedSel */
{OBJ r;
 ADenotation_AuncheckedSel(x1,x2,r);
 return r;}

extern OBJ _ADenotation_AuncheckedUpd(OBJ x1,OBJ x2,OBJ x3) /* uncheckedUpd */
{OBJ r;
 ADenotation_AuncheckedUpd(x1,x2,x3,r);
 return r;}

extern OBJ _ADenotation_Acompare(OBJ x1,OBJ x2) /* compare */ {
    INT l1 = leng_denotation(x1), l2 = leng_denotation(x2);
    INT l = l1 < l2 ? l1 : l2;
    INT i = 0, c;
    while (i < l 
	   && !(c = (INT)data_denotation(x1)[i]-(INT)data_denotation(x2)[i])) 
	i++;
    free_denotation(x1,1); free_denotation(x2,1);
    if (i >= l){
        return pack_int(l1-l2);
    } else {
        return pack_int(c);
    }
}

extern OBJ _ADenotation_Aslice(OBJ x1,OBJ x2, OBJ x3) /* slice */ {
    OBJ r;
    NAT l = leng_denotation(x1), i = unpack_nat(x2), j = unpack_nat(x3);
    if (j >= l) j = l - 1;
    if (i > j){
	r = alloc_denotation(0);
    } else {
	r = alloc_denotation(j - i + 1);
	memcpy((void*)data_denotation(r),
	       (void*)(data_denotation(x1)+i), j - i + 1);
    }
    free_denotation(x1,1);
    return r;
}

extern OBJ _ADenotation_Adelete(OBJ x1,OBJ x2, OBJ x3) /* delete */ {
    OBJ r;
    NAT l = leng_denotation(x1), i = unpack_nat(x2), j = unpack_nat(x3);
    if (j >= l) j = l - 1;
    if (i > j){
	r = x1;
    } else {
	r = alloc_denotation(l - (j - i + 1));
	memcpy((void*)data_denotation(r),
	       (void*)data_denotation(x1), i);
	memcpy((void*)(data_denotation(r) + i),
	       (void*)(data_denotation(x1) + j + 1), l - j - 1);
        free_denotation(x1,1);
    }
    return r;
}

extern OBJ _ADenotation_Ainsert(OBJ x1,OBJ x2, OBJ x3) /* insert */ {
    OBJ r;
    NAT l1 = leng_denotation(x1), p = unpack_nat(x2), l2 = leng_denotation(x3);
    if (l1 == 0){
	free_denotation(x1,1);
	return x3;
    }
    if (p >= l1) p = l1;
    r = alloc_denotation(l1 + l2);
    memcpy((void*)data_denotation(r),
	   (void*)data_denotation(x1), p);
    memcpy((void*)(data_denotation(r) + p),
	   (void*)(data_denotation(x3)), l2);
    memcpy((void*)(data_denotation(r) + p + l2),
	   (void*)(data_denotation(x1) + p), l1 - p);
    free_denotation(x1,1);
    free_denotation(x3,1);
    return r;
}


static init_const_ADenotation()
{}
