/* hand-coded implementation part of Tk */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:25 $ ($Revision: 1.1.1.1 $)
*/

#include "Com.h"
#include "ComAgent.h"
#include "OCS/Tcl.h"
#include "tk.h"

#define TK_STANDALONE 

/* Extensions ------------------------------------------------------------- */

/* #define USERASTER */

#ifdef USERASTER
#include "tkRaster.c"
#include "tkRasterBuiltIn.c"
#endif



/* Application Creation --------------------------------------------------- */

extern OBJ _ATk_ACreate(OBJ Interp,OBJ Display,OBJ Name,OBJ Unit)  {
    INTERP interp = unpack_interpreter(Interp);
    Tk_Window main; 
    char name[256], display[256];

    get_denotation(Name, name, sizeof(name));
    get_denotation(Display, display, sizeof(display));
    
#ifdef TK_STANDLONE
    if (Tk_InitStandAlone(interp->tcl) != TCL_OK){
#else
    if (Tk_Init(interp->tcl) != TCL_OK){
#endif

	OBJ Error;
	Error = make_denotation(interp->tcl->result);
	Tcl_ResetResult(interp->tcl);
	return _ACom_Afail(Error);
    }

    main = Tk_MainWindow(interp->tcl);    

    if (main == NULL){
	OBJ Error;
	Error = make_denotation(interp->tcl->result);
	Tcl_ResetResult(interp->tcl);
	return _ACom_Afail(Error);
    }

    Tk_GeometryRequest(main, 200, 200); 

#ifdef USERASTER
    if (RasterInit(interp->tcl) != TCL_OK){
	OBJ Error;
	Error = make_denotation(interp->tcl->result);
	Tcl_ResetResult(interp->tcl);
	return _ACom_Afail(Error);
    }

     /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */

    Tcl_CreateCommand(interp->tcl, "raster", RasterCmd, (ClientData) main,
            (void (*)()) NULL);
#endif


    return_okay_nil;


}

/* Scheduling ------------------------------------------------------------ */

static int retained = 0;
static void doWhenIdle(ClientData);
static void timerHandler(ClientData);

extern OBJ _ATk_ARetain(OBJ Unit)  {
    retained++;
    return_okay_nil;
}

extern OBJ _ATk_ASubmit(OBJ Unit)  {
    retained--;
    return_okay_nil;
}


extern OBJ _ATk_AUpdate(OBJ Unit)  {
    Tk_CancelIdleCall(doWhenIdle, NULL);
    while( Tk_DoOneEvent(TK_ALL_EVENTS | TK_DONT_WAIT) != 0 );
    Tk_DoWhenIdle(doWhenIdle, NULL);
    return_okay_nil;
}


#define MAX_SCHEDULE_CYCLES 100

#define CLOCK_GRAN	    1
static int timeout;


static void doWhenIdle(ClientData dummy) {
    int i, res;
    /* schedule not more then MAX_SCHEDULE_CYCLES */
    for (i = 0; i < MAX_SCHEDULE_CYCLES || retained > 0; i++) {
	res = schedule_agent();
	if (res != 0 ) break;
    }
    if (res == 0) {
	/* more active agents. arrange for a timeout after CLOCK_GRAN */
	timeout = CLOCK_GRAN;
	Tk_CreateTimerHandler(CLOCK_GRAN, timerHandler, NULL);
    } else 
    if (res > 0) {
	/* no active agents, but a timeout is expected after res millseconds. 
	   arrange timer for this period. */
	timeout = res;
	Tk_CreateTimerHandler(res, timerHandler, NULL);
    } else {
	/* no agents to schedule.  sleep until external input occures.
        */
    }
}


static void timerHandler(ClientData dummy){
    agent_timeout_clock += timeout;
}


static int top_exec(OBJ topCom){
    OBJ MainAgent;

    MainAgent = create_agent(topCom);

    for (;;) {
        if (retained > 0) {
	    schedule_agent();
	} else {
	    Tk_DoWhenIdle(doWhenIdle, NULL);
	    Tk_DoOneEvent(TK_ALL_EVENTS | TK_DONT_WAIT);
	}
    }
}


extern int (*ocs_top_exec)(OBJ);
    
static init_const_ATk(){
    init_ACom();
    init_AComAgent();
    init_ATcl();

    ocs_top_exec = top_exec;
}
