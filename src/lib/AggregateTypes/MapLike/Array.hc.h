/* hand-coded interface part of Array */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "Nat.h"

  /* representation */

typedef BCELL ARRAY;

#define data_array(a) data_big(a)
#define leng_array(a) size_big(a)
#define copy_array(a,n) copy_structured(a,n)
#define free_array(a,n) free_structured(a,n)
#define excl_array(a,n) excl_structured(a,n)
#define decr_array(a,n) decr_structured(a,n)
#define dispose_array(a) dispose_structured(a)
#define dispose_array_flat(a) dispose_structured_flat(a)

extern OBJ alloc_array(int);
extern OBJ dup_array(OBJ);


  /* macro based implementations */

#define AArray_S3(a,r){\
	r=pack_nat(leng_array(a)); free_array(a,1);\
}

#define AArray_Aempty_(a,r) {\
        r = pack_bool(leng_array(a) == 0); free_array(a,1);\
}		       

#define AArray_AuncheckedSel(a,i,r){\
	NAT __i=unpack_nat(i);\
	r=data_array(a)[__i];\
	if(excl_array(a,1)){data_array(a)[__i]=NIL; dispose_array(a);}\
	else {copy_some(r,1); decr_array(a,1);}\
}

#define AArray_AuncheckedUpd(i,d,a,r){\
	NAT __i=unpack_nat(i);\
	if(excl_array(a,1)){r=a;}else{r=dup_array(a);}\
	free_some(data_array(r)[__i],1); data_array(r)[__i]=d;\
}

#define AArray_AuncheckedUpdFun(i,f,a,r){\
	NAT __i=unpack_nat(i);\
	if(excl_array(a,1)){r=a;}else{r=dup_array(a);}\
	data_array(r)[__i]=\
		(*(OBJ (*)(OBJ,OBJ))METHOD(f,1))(f,data_array(r)[__i]);\
}

#define AArray_AuncheckedSwap(a,i1,i2,r){\
	NAT __i1=unpack_nat(i1), __i2=unpack_nat(i2); OBJ __t;\
	if(excl_array(a,1)){r=a;}else{r=dup_array(a);}\
	__t=data_array(r)[__i1]; data_array(r)[__i1] = data_array(r)[__i2];\
	data_array(r)[__i2] = __t;\
}

