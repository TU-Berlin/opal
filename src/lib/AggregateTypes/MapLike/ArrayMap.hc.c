/* hand-coded implementation part of ArrayMap */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:05 $ ($Revision: 1.1.1.1 $)
*/

#include "Nat.h"
#include "Array.h"


extern OBJ _AArrayMap_S8(OBJ f,OBJ a){
    NAT l = leng_array(a), i; OBJ r;
    if (l > 0){
	copy_closure(f,l-1);
    } else {
	free_closure(f,1);
    }
    if (excl_array(a,1)){
        for (i = 0; i < l; i++){
            data_array(a)[i] = EVAL1(f,data_array(a)[i]);
        }
        return a;
    } else {
	r = alloc_array(l);
        for (i = 0; i < l; i++){
            OBJ t = data_array(a)[i];
            copy_some(t,1);
            data_array(r)[i] = EVAL1(f,t);
        }
        decr_array(a,1);
        return r;
    }
}

static init_const_AArrayMap()
{}
