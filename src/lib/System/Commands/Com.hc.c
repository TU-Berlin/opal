/* hand-coded implementation part of Com */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/

#include <stdio.h>

#include "Nat.h"
#include "Void.h"

OBJ ans_okay_nil;
static OBJ (*com_ans_func)(OBJ);


extern OBJ _ACom_AyieldProc(OBJ ans, OBJ unit){
    return ans;
}

extern OBJ _ACom_AexitProc(OBJ code, OBJ unit){
    ocs_exit(unpack_nat(code));
    return _ACom_Afail(make_denotation("cannot exit!"));
}

extern OBJ _ACom_AEXEC(OBJ com){
    OBJ ans = _ACom_AexecCom(com);

    copy_ans(ans, 1);
    if (_ACom_Aokay_(ans) == pack_clean_bool(1)) {
	return _ACom_Adata(ans);
    } else {
	fprintf(stderr, data_denotation(_ACom_Aerror(ans)));
	ocs_exit(1);
    }
}



extern OBJ declare_failure_answer(char * message){
    return _ACom_Afail(make_denotation(message));
}

static int top_exec(OBJ topCom){
    OBJ ans = _ACom_AexecCom(topCom);
    if (_ACom_Aokay_(ans) == pack_clean_bool(1))
	return 0;
    else
	return 1;
}



extern OBJ ocs_com_ans_monitor(OBJ Ans){
    return (*com_ans_func)(Ans);
}

/* OPAL version: */
extern OBJ _ACom_Amonitor(OBJ Ans){
    return (*com_ans_func)(Ans);
}

extern OBJ (*ocs_com_ans_monitor_def_method(OBJ (*func)(OBJ)))(OBJ){
    OBJ (*old)(OBJ);
    old = com_ans_func;
    com_ans_func = func;
    return old;
}


static OBJ com_ans_default_func(OBJ Ans){
    return Ans;
}


extern int (*ocs_top_exec)(OBJ);

static init_const_ACom() { 
    init_ANat();
    ans_okay_nil = _ACom_Aokay(__AVoid_Anil);
    ocs_com_ans_monitor_def_method(com_ans_default_func);
    ocs_top_exec = top_exec;
}
