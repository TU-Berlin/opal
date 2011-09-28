/* hand-coded implementation part of Array */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>
#include "Nat.oc.h"


#define check_rc(h,n,origin){\
	if((h).info.rc > _maxRc-(n)) HLT(origin ": RC overflow");\
}

extern OBJ alloc_array(int s){
    OBJ r;
    alloc_big(s,r);
    return r;
}

extern OBJ dup_array(OBJ a){
    register NAT i, l = leng_array(a);
    register OBJ r = alloc_array(l);
    for (i = 0; i < l; i++){
        data_array(r)[i] = data_array(a)[i]; copy_some(data_array(a)[i],1);
    }
    decr_array(a,1);
    return r;
}


extern OBJ _AArray_Ainit(OBJ l,OBJ o) {
    register NAT cl = unpack_nat(l), i;
    register OBJ r = alloc_array(cl);
    if (cl > 0){
        if (_isRef(o)){
           check_rc(_header(o),cl-1,"array initialization");
           _incRc(_header(o),cl-1);
        }
        for (i = 0; i < cl; i++){
            data_array(r)[i] = o;
        }
    } else {
        free_some(o,1);
    }
    return r;
}


extern OBJ _AArray_Ainit_O1(OBJ l,OBJ f) {
    NAT cl = unpack_nat(l), i;
    OBJ r = alloc_array(cl);
    for (i = 0; i < cl; i++){
        copy_closure(f,1);
        data_array(r)[i] = EVAL1(f,pack_nat(i));
    }
    free_closure(f,1);
    return r;
}

extern OBJ _AArray_S3(OBJ x1) /* # */
{OBJ r;
 AArray_S3(x1,r);
 return r;}

extern OBJ _AArray_Spp(OBJ a1,OBJ a2){
    NAT l1 = leng_array(a1), l2 = leng_array(a2), i;
    OBJ r = alloc_array(l1+l2);
    if (excl_array(a1,1)){
	memcpy((void*)data_array(r),data_array(a1),
	       l1 * sizeof(OBJ));
	dispose_array_flat(a1);
    } else { 
	for (i = 0; i < l1; i++){
	    OBJ t = data_array(a1)[i]; 
	    copy_some(t,1);
	    data_array(r)[i] = t; 
	}
	decr_array(a1,1);
    }
    if (excl_array(a2,1)){
	memcpy((void*)(data_array(r) + l1),data_array(a2),
	       l2 * sizeof(OBJ));
	dispose_array_flat(a2);
    } else { 
	for (i = 0; i < l2; i++){
	    OBJ t = data_array(a2)[i]; 
	    copy_some(t,1);
	    data_array(r)[l1+i] = t; 
	}
	decr_array(a2,1);
    }
    return r;
}

extern OBJ _AArray_Adelete(OBJ a,OBJ from,OBJ to){
    OBJ r;
    NAT l = leng_array(a), i = unpack_nat(from), j = unpack_nat(to),c;
    if (j >= l) j = l - 1;
    if (i > j){
        r = a;
    } else {
	NAT ofs = j - i + 1, newl = l - ofs;
        r = alloc_array(newl);
	if (excl_array(a,1)){
	    for (c = 0; c < i; c++){
		data_array(r)[c] = data_array(a)[c];
		data_array(a)[c] = NIL;
	    }
	    while (c < newl){
		data_array(r)[c] = data_array(a)[c+ofs];
		data_array(a)[c+ofs] = NIL;
		c++;
	    }
	    dispose_array(a);
	} else {
	    for (c = 0; c < i; c++){
		OBJ t = data_array(a)[c];
		copy_some(t,1);
		data_array(r)[c] = t;
	    }
	    while (c < newl){
		OBJ t = data_array(a)[c+ofs];
		copy_some(t,1);
		data_array(r)[c] = t;
		c++;
	    }
	    decr_array(a,1);
	}
    }
    return r;
}


extern OBJ _AArray_Ainsert(OBJ a1,OBJ at,OBJ a2){
    OBJ r;
    NAT l1 = leng_array(a1), p = unpack_nat(at), l2 = leng_array(a2), c;

    if (l1 == 0){
	free_array(a1,1);
	return a2;
    }
    if (p >= l1) p = l1;
    r = alloc_array(l1 + l2);
    if (excl_array(a1,1)){
	memcpy((void*)data_array(r),data_array(a1),p*sizeof(OBJ));
	memcpy((void*)(data_array(r) + p + l2),data_array(a1) + p,
	       (l1 - p)*sizeof(OBJ));
	dispose_array_flat(a1);
    } else {
	for (c = 0; c < p; c++){
	    OBJ t = data_array(a1)[c];
	    copy_some(t,1);
	    data_array(r)[c] = t;
	}
	while (c < l1){
	    OBJ t = data_array(a1)[c];
	    copy_some(t,1);
	    data_array(r)[l2 + c] = t;
	    c++;
	}
	decr_array(a1,1);
    }
    if (excl_array(a2,1)){
	memcpy((void*)(data_array(r) + p),data_array(a2),l2*sizeof(OBJ));
	dispose_array_flat(a2);
    } else {
	for (c = 0; c < l2; c++){
	    OBJ t = data_array(a2)[c];
	    copy_some(t,1);
	    data_array(r)[p+c] = t;
	}
	decr_array(a2,1);
    }
    return r;
}


extern OBJ _AArray_Aextend(OBJ a,OBJ n,OBJ o) {
    register NAT l = leng_array(a), cn = unpack_nat(n), i;
    register OBJ r = alloc_array(l+cn);
    if (excl_array(a,1)){
        memcpy((void*)data_array(r),(void*)data_array(a),l*sizeof(OBJ));
        dispose_array_flat(a);
        i = l;
    } else {
        for (i = 0; i < l; i++){
            data_array(r)[i] = data_array(a)[i]; copy_some(data_array(a)[i],1);
        }
        decr_array(a,1);
    }
    if (cn > 0){
        if (_isRef(o)) {
          _checkRc(_header(o),cn-1,"array initialization");
          _incRc(_header(o),cn-1);
        }
        do {
            data_array(r)[i] = o; i++;
        } while (--cn);
    } else {
        free_some(o,1);
    }
    return r;
}

extern OBJ _AArray_Aextend_O1(OBJ a,OBJ n,OBJ f){
    register NAT l = leng_array(a), cn = unpack_nat(n), i;
    register OBJ r = alloc_array(l+cn);
    if (excl_array(a,1)){
        memcpy((void*)data_array(r),(void*)data_array(a),l*sizeof(OBJ));
        dispose_array_flat(a);
    } else {
        for (i = 0; i < l; i++){
            data_array(r)[i] = data_array(a)[i]; copy_some(data_array(a)[i],1);
        }
        decr_array(a,1);
    }
    for (i = 0; i < cn; i++,l++){
        copy_closure(f,1);
        data_array(r)[l] = EVAL1(f,pack_nat(i));
    }
    free_closure(f,1);
    return r;
}

extern OBJ _AArray_AuncheckedSel(OBJ x1,OBJ x2) /* uncheckedSel */
{OBJ r;
 AArray_AuncheckedSel(x1,x2,r); 
 return r;}

extern OBJ _AArray_AuncheckedUpd(OBJ x1,OBJ x2,OBJ x3) /* uncheckedUpd */
{OBJ r;
 AArray_AuncheckedUpd(x1,x2,x3,r); 
 return r;}

extern OBJ _AArray_AuncheckedUpdFun(OBJ x1,OBJ x2,OBJ x3) /* uncheckedUpdFun */
{OBJ r;
 AArray_AuncheckedUpdFun(x1,x2,x3,r); 
 return r;}

extern OBJ _AArray_AuncheckedSwap(OBJ x1,OBJ x2,OBJ x3) /* uncheckedSwap */
{OBJ r;
 AArray_AuncheckedSwap(x1,x2,x3,r); 
 return r;}

static init_const_AArray()
{}
