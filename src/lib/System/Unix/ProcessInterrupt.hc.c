/* hand-coded implementation part of ProcessInterrupt */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:20 $ ($Revision: 1.1.1.1 $)
*/
#include "unixconfig.h"
#include "Nat.h"
#include "Com.h"

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

static init_const_AProcessInterrupt(){
    init_ANat();
    init_ACom();
    oldhandler = signal(SIGINT, handler);
}
