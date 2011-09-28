/* hand-coded implementation part of ArrayReduce */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include "Nat.oc.h"
#include "Array.oc.h"


extern OBJ _AArrayReduce_Ss_O1(OBJ f,OBJ e,OBJ a) /* /,1 */ {
    NAT i = leng_array(a);
    if (i == 0){
	free_closure(f,1);
    } else {
	copy_closure(f,i-1);
    }
    if (excl_array(a,1)){
        while (i > 0){
            OBJ t;
	    i--;
	    t = data_array(a)[i]; 
	    e = EVAL3(f,pack_nat(i),t,e);
	}
	dispose_array_flat(a);
    } else {
        while (i > 0){
	    OBJ t;
	    i--;
	    t = data_array(a)[i]; copy_some(t,1);
	    e = EVAL3(f,pack_nat(i),t,e);
	}
	decr_array(a,1);
    }
    return e;
}

extern OBJ _AArrayReduce_Sb_O1(OBJ f,OBJ e,OBJ a) /* \,1 */ {
    NAT i = 0,l = leng_array(a);
    if (l == 0){
	free_closure(f,1);
    } else {
	copy_closure(f,l-1);
    }
    if (excl_array(a,1)){
        while (i < l){
	    OBJ t = data_array(a)[i]; 
	    e = EVAL3(f,pack_nat(i),t,e);
	    i++;
	}
	dispose_array_flat(a);
    } else {
        while (i < l){
	    OBJ t = data_array(a)[i]; copy_some(t,1);
	    e = EVAL3(f,pack_nat(i),t,e);
	    i++;
	}
	decr_array(a,1);
    }
    return e;
}

static init_const_AArrayReduce()
{}
