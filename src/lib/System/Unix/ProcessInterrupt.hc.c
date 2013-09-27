/* hand-coded implementation part of ProcessInterrupt */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include "unixconfig.h"
#include "Nat.oc.h"
#include "Com.oc.h"

static void (*oldhandler)();
static volatile int interrupted = 0;

extern int ocs_read_interrupts(){
    int res = interrupted;
    interrupted = 0;
    return res;
}

extern OBJ _AProcessInterrupt_AcInterrupts(OBJ Void) {
    return_okay(pack_nat(ocs_read_interrupts()));
}


static void handler(int sig){
    interrupted++;
    signal(SIGINT, handler);
}

extern void init_ANat();
extern void init_ACom();

static void init_const_AProcessInterrupt(){
    init_ANat();
    init_ACom();
    oldhandler = signal(SIGINT, handler);
}
