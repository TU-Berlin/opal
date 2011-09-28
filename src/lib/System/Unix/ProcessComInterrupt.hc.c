/* hand-coded implementation part of ProcessComInterrupt */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "Com.oc.h"
#include "ProcessInterrupt.oc.h"

static int break_counter;
static OBJ (*old_monitor)(OBJ);

static OBJ monitor(OBJ Ans){
    int res = ocs_read_interrupts();
    OBJ Ans1 = (*old_monitor)(Ans);
    if (res > 0){
	/* throw away this answer, since operation was interrupted. */
	free_some(Ans1, 1);
	copy_some(VAR(ProcessComInterrupt,interrupted), 1);
	return VAR(ProcessComInterrupt,interrupted);
    } else {
	return Ans1;
    }
}

extern OBJ _AProcessComInterrupt_AcEnableBreak(OBJ Unit){
    if (break_counter == 0){
	old_monitor = ocs_com_ans_monitor_def_method(monitor);
    }
    break_counter++;
    return_okay_nil;
}

extern OBJ _AProcessComInterrupt_AcDisableBreak(OBJ x1){
    break_counter--;
    if (break_counter == 0){
	ocs_com_ans_monitor_def_method(old_monitor);
    }
    return_okay_nil;
}

static init_const_AProcessComInterrupt(){
    init_ACom();
    init_AProcessInterrupt();
    break_counter = 0;
}
