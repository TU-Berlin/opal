/* hand-coded implementation part of ArrayFold */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:05 $ ($Revision: 1.1.1.1 $)
*/

#include "Nat.h"
#include "Array.h"


extern OBJ _AArrayFold_Ss(OBJ f,OBJ a) /* / */ {
    NAT i = leng_array(a); OBJ r;
    if (i == 0){
	HLT("/'ArrayFold: array is empty");
    }
    if (i > 1) { copy_closure(f,i-2); } else { free_closure(f,1); }
    if (excl_array(a,1)){
	--i; r = data_array(a)[i];
        while (i > 0){
            OBJ t;
            i--;
            t = data_array(a)[i];
            r = EVAL2(f,t,r);
        }
        dispose_array_flat(a);
    } else {
	--i; r = data_array(a)[i]; copy_some(r,1);
        while (i > 0){
            OBJ t;
            i--;
            t = data_array(a)[i]; copy_some(t,1);
            r = EVAL2(f,t,r);
        }
        decr_array(a,1);
    }
    return r;
}

extern OBJ _AArrayFold_Sb(OBJ f,OBJ a) /* \ */ {
    NAT i = 1,l = leng_array(a); OBJ r;
    if (l == 0){
	HLT("\\'ArrayFold: array is empty");
    }
    if (l > 1) { copy_closure(f,l-2); } else { free_closure(f,1); }
    if (excl_array(a,1)){
	r = data_array(a)[0]; 
        while (i < l){
            OBJ t = data_array(a)[i];
            r = EVAL2(f,t,r);
            i++;
        }
        dispose_array_flat(a);
    } else {
	r = data_array(a)[0]; copy_some(r,1);
        while (i < l){
            OBJ t = data_array(a)[i]; copy_some(t,1);
            r = EVAL2(f,t,r);
            i++;
        }
        decr_array(a,1);
    }
    return r;
}

static init_const_AArrayFold()
{}
