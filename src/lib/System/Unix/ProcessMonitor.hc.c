/* hand-coded implementation part of ProcessMonitor */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:20 $ ($Revision: 1.1.1.1 $)
*/
#include "Com.h"
#include "ProcessConnect.h"
#include "ProcessInterrupt.h"

extern OBJ _AProcessMonitor_Amonitor(OBJ Chan, OBJ Env, 
				     OBJ MonChan, OBJ MonSig) {
    if (ocs_read_interrupts() > 0){
	free_closure(MonChan, 1);
	return _ACom_AEXEC(EVAL1(MonSig, Env));
    } else {
	int retval;
	free_closure(MonSig, 1);
	retval = pconn_test_incoming(Chan);
	if (retval) {
	    return _ACom_AEXEC(EVAL1(MonChan, Env));
	} else {
	    free_closure(MonChan, 1);
	    return Env;
	}
    }
}

extern OBJ _AProcessMonitor_AmonitorForce(OBJ Env, OBJ Mon) {
    return _ACom_AEXEC(EVAL1(Mon, Env));
}

static init_const_AProcessMonitor(){
    init_ACom();
    init_AProcessConnect();
    init_AProcessInterrupt();
}
