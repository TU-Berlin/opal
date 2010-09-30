/* hand-coded interface part of ComAgent */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/
/* -- functions to redefine the scheduler ------------------------------- */

/* 
The structure Agent registers a default method to schedule
agents and to execute the top-level command at structure 
initialization time. This method may be overwritten to
let the scheduling of agents coincide with other schedulers like
event dispatchers of window APIs. The following functions
are provide for this purpose.
*/

/*
The function schedule_agent performs the next scheduling
cycle, that is, garbage collection of internal resources,
activating timed-out agents, and running the front command
of the next agent. Its return value indicates the state
of scheduling:

	r = -1	there are no active agents, which implies, that the
		scheduler idles until new agents are created elsewhere

	r = 0	there are active agents 

	r > 0	no active agents, but a timeout which activates one
		is expected in r milliseconds.

*/

extern long schedule_agent();


/*
The global variable agent_timeout_clock has to be maintained to
count real-time milliseconds. This value is reset internally
by each call to agent_schedule. An installed ualarm()
or whatsoever should just count it up frequently. However, not calling 
schedule_agent for around 24 days will cause on overflow!
*/ 

extern long agent_timeout_clock;


/*
The following function creates an (active) agent for administration
purposes:
*/

extern void * create_agent(OBJ command);




/* -- functions implemented for other structures --------------------------- */

extern OBJ agent_sapProc();
extern OBJ agent_provideProc(OBJ, OBJ, OBJ);
extern OBJ agent_requestProc(OBJ, OBJ);
extern OBJ agent_timeoutProc(OBJ);
