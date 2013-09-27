/* hand-coded implementation part of ComAgent */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/



#include <unixconfig.h>

#include "Nat.oc.h"
#include "Void.oc.h"
#include "Seq.oc.h"
#include "Com.oc.h"

#ifndef NULL
#define NULL ((void*)0)
#endif

static int debugComm;
static int debugTerm;


/* ------------------------------------------------------------------------ */
/* Failures */

OBJ agent_ans_sync, agent_ans_killed;

static void init_failures(){
    agent_ans_sync = declare_failure_answer("internal scheduler error");
    agent_ans_killed = 
    	declare_failure_answer("server killed during rendezvous");

    copy_ans(agent_ans_killed, 1);
    __AComAgent_AansKilled = agent_ans_killed;
}


/* ------------------------------------------------------------------------ */
/* Queues */

/* 
Queues are used as a general data type for maintaining collections
of objects. Elements of queues may be of any type starting with
the header struct sQCTX. An important feature of the implementation 
is that an element can be deleted from the queue it belongs
to without knowing about this queue. The implementation bases
on a double-linked, circular ring.
*/


typedef struct sQCTX {		/* header of elements of queues */
    struct sCELL header;		/* used for OCS memory management */
    struct sQCTX * prev, * next;	/* ring-pointers */
} * QCTX;

typedef struct sQCTX QUEUE;

#define dec_QUEUE(name)		QUEUE name = {{0,}, &name, &name}

#define init_queue(q)   (q)->next = (q)->prev = q;
#define is_empty(q) 	((q)->next == (q))
#define is_elem(q,e)    ((QCTX)(e) != (QCTX)(q))
#define first(q)     	((void*)(q)->next)
#define next(q,e)    	((void*)((QCTX)(e))->next)

static void dequeue(void * some){
    QCTX elem = (QCTX)some;
    elem->next->prev = elem->prev;
    elem->prev->next = elem->next;
    elem->next = elem->prev = elem;
}

static void insert(QUEUE * queue, void * before, void * some){
    QCTX b = before;
    QCTX e = some;
    e->prev = b->prev;
    e->next = b;
    b->prev->next = e;
    b->prev = e;
}

#define append(queue, some) insert(queue, queue, some)
#define prepend(queue, some) insert(queue, first(queue), some)



/* ------------------------------------------------------------------------ */
/* Working on Commands */

/* 
Given a sequence of continuations seq[contCom] and an answer,
the following function constructs a command by executing
the first continutation on the answer. If the sequence of
continuations is empty, then a return command will be created
on base of the answer. The function is actually implemented in OPAL.
*/

#define exec_cont(cont,ans) _AComAgent_AexecCont(cont,ans)


/*
Given an atomic command, the following function execuates it. It is actually
implemented in OPAL.
*/

#define exec_atom(atom) _AComAgent_AexecAtom(atom)


/* ------------------------------------------------------------------------ */
/* Agents */

/* 
An agent is described by a queue context, a state, a list of expects of
this agent, a queue of clients currently served by the agent, 
a queue of wait expects for the agents result, and the command to be 
executed next:
*/

typedef struct sAGENT {	
    struct sQCTX ctx;			/* queue context of agent */
    int state;				/* state */
    struct sEXPECT * expects;		/* expects */
    QUEUE clients;			/* clients served */
    QUEUE waits;			/* wait expects of other agents */
    OBJ value;				/* command / continuation / result */
} * AGENT;


static AGENT running_agent;
static dec_QUEUE(active_queue);
static dec_QUEUE(expect_queue);
static dec_QUEUE(exit_queue);	

/*
The state is defined as follows:
*/

enum { state_running, state_active, state_expect, state_client, state_exit };

/*
If an agent is running it is stored in the global
variable running_agent.

If an agent is active, expecting or exiting, it is queued into exactly 
one of the global queues active, expect or exit. 

If an agent is in state_client, it is queued into the clients queue
of the serving agent.

Upon state_running and state_client, the value field 
contains the continuation (of type seq[contCom]) of the agent.
Upon state_active, the value field contains the next command to execute.
Upon state_exit, the value field contains the result of the agents execution.

*/


/*
An agent is created by the following function. An initial
reference to the agent is grabbed for the scheduler itself:
*/

extern void * create_agent(OBJ command){
    AGENT agent;

    alloc_small_flat(sizeof_small(struct sAGENT), agent);
    agent->state = state_active;
    agent->expects = NULL;
    init_queue(&agent->clients);
    init_queue(&agent->waits);
    agent->value = command;
    append(&active_queue, agent);

    copy_structured(agent, 1);
    return agent;
}

/* %
The command procedure to create agents is based on the above function:
*/

extern OBJ _AComAgent_AagentProc(OBJ command, OBJ unit){
    return_okay((OBJ)create_agent(command));
}

/* %
The current running agent is retrieved with the following command
procedure:
*/

extern OBJ _AComAgent_AselfProc(OBJ unit){
    copy_structured(running_agent, 1);
    return_okay(running_agent);
}


/*
Terminating an agent is maintained by the following function. It
frees all expects of the agent and activates all served clients with a failure
condition. The agent is put into state_exit, and any clients expecting the 
result of the termination are activated. 
*/

static void free_expects(AGENT);
static void activate_agent(AGENT, OBJ);
static void activate_waits(AGENT, OBJ);

static void terminate_agent(AGENT agent, OBJ result){
    AGENT client;

    if (debugTerm) {
	copy_some(result, 1);
	if (_ACom_Afail_(result) == pack_bool(1)) {
	    copy_some(result, 1);
	    fprintf(stderr,"agent execution failed: %s\n", 
	    		   data_denotation(_ACom_Aerror(result)));
        }
    }

    free_expects(agent);

    /* activate clients */
    while (!is_empty(&agent->clients)) {
	client = first(&agent->clients);
	dequeue(client);
	copy_some(agent_ans_killed, 1);
	activate_agent(client, exec_cont(agent->value, agent_ans_killed));
    }

    if (agent->state != state_expect)
	free_some(agent->value, 1);
    agent->value = result;
    dequeue(agent);
    agent->state = state_exit;

    activate_waits(agent, result);

    if (excl_structured(agent, 1)) {
	/* no one outside of the scheduler referes to agent: dispose it */
	dispose_structured_flat(agent);
    } else {
	/* append it to the exit queue */
	append(&exit_queue, agent);
    }
}

/*
On the base of the function terminate_agent, a command procedure to
terminate the running agent is implemented. This procedure is only
used internally (by function eval_cont), since it cannot ensure type
safety.
*/

extern OBJ _AComAgent_AtermProc(OBJ result, OBJ unit){
    terminate_agent(running_agent, result);
    return_okay_nil;
}

/*
On the base of the function terminate_agent, killing an agent is 
implemented as a command procedure.
*/

extern OBJ _AComAgent_AkillProc(OBJ Agent, OBJ unit){
    free_structured(Agent, 1);
    copy_ans(agent_ans_killed, 1);
    terminate_agent((AGENT)Agent, agent_ans_killed);
    return_okay_nil;
}



/*
Garbage collection of agents is performed by calling
the function gc_agents at every scheduling cycle. The function
controls itself the actual granularity of GC by maintaining in internal
counter. On GC, it scans the queue of exited agents and frees all with 
a reference count of one, i.e. which are only referred to from 
inside of the scheduler:
*/

#define AGENT_GC_CYCLES 20 

static void gc_agents(){
    static int counter = 0;
    AGENT agent, agent1;

    if (counter >= AGENT_GC_CYCLES){
	counter = 0;

	for (agent = first(&exit_queue); is_elem(&exit_queue, agent); 
							    agent = agent1){
	    agent1 = next(&exit_queue, agent);
	    if (excl_structured(agent, 1)) {
		dequeue(agent);
		free_some(agent->value, 1);
		dispose_structured_flat(agent);
	    }
	}
    } else {
	counter++;
    }
}


/* ------------------------------------------------------------------------ */
/* Expects in General */

/*
An agent may be in the state of expecting simultaneously several
kinds of events. An expectation is in generally described 
by the following data type. Specializations for the kinds
of events are defined below.
*/

typedef struct sEXPECT {
    struct sQCTX ctx;			/* queue context of expect */
    struct sEXPECT * next;		/* next expect of agent */
    AGENT agent;			/* expecting agent */
    OBJ cont;				/* continuation */ 
    void (*freeSub)(struct sEXPECT *);	/* free sub-components */
} * EXPECT;

/*
The field ctx is the context of a queue associated with a medium which
satisfies the expects of the agent. The next field is used 
to link together the expects of one agent. The associated agent and 
the continuation to be called when this expect is satisified (which is 
a sequence of OPAL functions of type ans[a} -> com[b]) is contained; the
continuation is subject of RC. The function freeSub tells how to free 
subcomponets of the special variants of an expectation. Such 
specializations are defined for mediums such as timeouts and services below.
*/

/*
The following function frees the expect information associated
with an agent. Each expect is dequeued from the associated medium,
the continuation and subcomponents are freed, and the expect
record is deallocated:
*/

static void free_expects(AGENT agent){
    EXPECT exp, exp1;
    for (exp = agent->expects; exp != NULL; exp = exp1){
	dequeue(exp);
	free_some(exp->cont, 1);
	(*exp->freeSub)(exp);
	exp1 = exp->next;
	dispose_structured_flat(exp);
    }
    agent->expects = NULL;
}

/*
The following function puts an agent from any state into state
active. It frees all its expects.
*/

static void activate_agent(AGENT agent, OBJ nextCom){
    if (debugComm) {
	fprintf(stderr, "agent %x activated\n", agent);
    }
    dequeue(agent);
    free_expects(agent);
    agent->value = nextCom;
    agent->state = state_active;
    append(&active_queue, agent);
}




/* ------------------------------------------------------------------------ */
/* Timeout Expects */

/*
Timeout expects are realized by a global queue consisting
of expect specializations of the structure sTIMEOUT.
The timeouts in the queue are organized relatively: let a1, a2, a3 be 
agents expecting a timeout after 2, 4 and 6 ms, respectively, then they 
are inserted into the queue as follows:

	a1	a2	a3	
	2	2	2

*/

static dec_QUEUE(timeout_queue);

typedef struct sTIMEOUT {
    struct sEXPECT expect;
    int delay;
} * TIMEOUT;

static void freeSub_timeout(struct sEXPECT * exp){
    /* no subcomponents to free */
}

long agent_timeout_clock;

/*
The following command procedure creates a timeout-expect for the
running agent. A timeout expect is created regardeless of if
the timeout value is 0. Hence a timeout of 0 may be used 
for a non-blocking check of the availabilty of other 
events in a choice.
*/

extern OBJ agent_timeoutProc(OBJ Delay){
    int delay = unpack_nat(Delay);
    TIMEOUT t, new; 

    alloc_small_flat(sizeof_small(struct sTIMEOUT), new);
    new->expect.agent   	= running_agent;
    new->expect.cont    	= running_agent->value;
    running_agent->value 	= NIL;
    new->expect.freeSub 	= freeSub_timeout;
    new->expect.next    	= running_agent->expects;
    running_agent->expects	= (EXPECT)new;

    t = first(&timeout_queue);
    for (;;) {
	if (!is_elem(&timeout_queue, t)){
	    /* append at end */
	    append(&timeout_queue, new);
	    new->delay = delay;
	    break;
	} else 
	if (delay < t->delay) {
	    /* insert before t, adjusting t->delay */
	    t->delay -= delay;
	    insert(&timeout_queue, t, new);
	    new->delay = delay;
	    break;
	} else {
	    /* relativate delay and continue */ 
	    delay -= t->delay;
	    t = next(&timeout_queue, t);
	    continue;
	}
    }

    running_agent->state = state_expect;
    dequeue(running_agent);
    append(&expect_queue, running_agent);

    return_fail(agent_ans_sync);
} 


/*
The following function activates agents with an expired timeout.
It returns the delay until the next timeout, or -1, of no
such exists.
*/

static long activate_timeouts() {
    TIMEOUT t;

    for (;;) {
	if (is_empty(&timeout_queue)) {
	    return -1;
	} else {
	    t = first(&timeout_queue);
	    if (agent_timeout_clock >= t->delay){
		/* activate */
		agent_timeout_clock -= t->delay;
		copy_some(t->expect.cont, 1);
		copy_some(ans_okay_nil, 1);

		/* activate_agent will dequeue and free expect */
		activate_agent(t->expect.agent, 
			       exec_cont(t->expect.cont, ans_okay_nil));
	    } else {
		/* just decrement delay */
		t->delay -= agent_timeout_clock;
		agent_timeout_clock = 0;
		return t->delay;
	    }
	}
    }
}

/* ------------------------------------------------------------------------ */
/* Wait Expects */

/*
Wait expects are realized by the following specialization:
*/

typedef struct sWAIT {
    struct sEXPECT expect;
    /* no specific components */
} * WAIT;

static void freeSub_wait(struct sEXPECT * exp){
    /* no subcomponents to free */
}

/*
The following command procedure performs a wait for the result of
the passed agent. If the agent has exited, its result is just
returned. Otherwise a wait-expect is created for the running agent.
*/

extern OBJ _AComAgent_AwaitProc(OBJ Agent, OBJ unit){
    AGENT agent = (AGENT)Agent;

    if (agent->state == state_exit) {
	 OBJ result = agent->value;
	 copy_some(result, 1);
	 free_structured(Agent, 1);
	 return_okay(result);

    } else {
	WAIT wait;

	alloc_small_flat(sizeof_small(struct sWAIT), wait);

	wait->expect.agent   	= running_agent;
	wait->expect.cont    	= running_agent->value;
	running_agent->value 	= NIL;
	wait->expect.freeSub 	= freeSub_wait;
	wait->expect.next    	= running_agent->expects;
	running_agent->expects 	= (EXPECT)wait;

	append(&agent->waits, wait);
	free_structured(Agent, 1);

	running_agent->state 	= state_expect;
	dequeue(running_agent);
	append(&expect_queue, running_agent);

	return_fail(agent_ans_sync);
    }
} 

/*
The function below activates all agents waiting for the result of
the given agent:
*/

static void activate_waits(AGENT agent, OBJ result){
    WAIT wait;
    while(!is_empty(&agent->waits)){
	wait = first(&agent->waits);
	copy_ans(result, 1);
	copy_some(wait->expect.cont, 1);
        /* activate_agent will dequeue and free expect */
	activate_agent(wait->expect.agent, 
		       exec_cont(wait->expect.cont, result));
    }
}



/* ------------------------------------------------------------------------ */
/* Service Access Points and Provider / Requester Expects */

/*
The data type of service access points contains two queues: one for the
providers and one for the requesters. All SAPs are stored in a global
queue.
*/

static dec_QUEUE(sap_queue);

typedef struct sSAP {		/* service access point */
    struct sQCTX  ctx;			/* queue context */
    QUEUE provides;			/* queue of provides */ 
    QUEUE requests;			/* queue of requests */
} * SAP;

/*
The following invariant holds on SAPs: any provides and requests
existing at the same time in the queues cannot match against
each other because the servers condition on the clients input
doesn't hold.
*/

/*
A service access point is created by the following function.
One reference to the SAP is grabbed for the scheduler. A command
procedure and a side-effect function is implemented for structure
Service:
*/

static SAP create_sap(){
    SAP sap;

    alloc_small_flat(sizeof_small(struct sSAP), sap);
    init_queue(&sap->requests);
    init_queue(&sap->provides);

    copy_structured(sap, 1);
    append(&sap_queue, sap);

    return sap;
}

extern OBJ agent_sapProc(){
    return_okay(create_sap());
}



/*
Services are garbage collected similar to agents. The condition to be
isolated is that no references from outside of the scheduler exists.
Following from the invariant on SAPs, if in such a case 
the provide or request queues aren't empty, a deadlock of the
expects from this sap is detected. However, we do not
handle this condition currently, but instead let the SAP survive
as long as provides or requests exist.
*/

#define SAP_GC_CYCLES 20 

static void gc_saps(){
    static int counter = SAP_GC_CYCLES / 2;
    SAP sap, sap1;

    if (counter >= SAP_GC_CYCLES){
	counter = 0;

	for (sap = first(&sap_queue); is_elem(&sap_queue, sap); sap = sap1){
	    sap1 = next(&sap_queue, sap);
	    if (excl_structured(sap, 1) 
	    		&& is_empty(&sap->provides)
	    		&& is_empty(&sap->requests)) {
		dequeue(sap);
		dispose_structured_flat(sap);
	    }
	}
    } else {
	counter++;
    }
}

/*
A request describes the input data of the 
request, a provide the command to serve a request:
*/ 

typedef struct sREQUEST {
    struct sEXPECT expect;
    OBJ input;
} * REQUEST;


typedef struct sPROVIDE {
    struct sEXPECT expect;
    OBJ cond;
    OBJ service;
} * PROVIDE;

static void freeSub_request(EXPECT exp){
    free_some(((REQUEST)exp)->input, 1);
}

static void freeSub_provide(EXPECT exp){
    free_closure(((PROVIDE)exp)->cond, 1);
    free_some(((PROVIDE)exp)->service, 1);
}


/*
Requesting on a SAP is managed by the following command procedure.
If a rendezvous is immediatly possible, it is performed, otherwise
a request expect is created for the agent. The most work for the
rendezvous is done in an auxiliary function defined below.
*/

static void rendezvous(AGENT, OBJ, OBJ, AGENT, OBJ, OBJ);


extern OBJ agent_requestProc(OBJ Sap, OBJ input){
    SAP sap = (SAP)Sap;
    PROVIDE prov;

    prov = first(&sap->provides);
    while (is_elem(&sap->provides, prov)) {
	copy_some(input, 1);
	copy_closure(prov->cond, 1);
	if (EVAL1(prov->cond, input) == pack_bool(1)) {
	    /* found a server accepting input */
	    break;
	}
	prov = next(&sap->provides, prov);
    } 

    if (is_elem(&sap->provides, prov)) {
	/* immediate rendezvous */

	/* hold a hand on service data and continuation of provide */
	copy_some(prov->service, 1);
	copy_some(prov->expect.cont, 1);

	rendezvous(prov->expect.agent, prov->service, prov->expect.cont,
		   running_agent, input, running_agent->value);

    } else {
	/* create new request */
	REQUEST req;

	alloc_small_flat(sizeof_small(struct sREQUEST), req);
	req->expect.agent	= running_agent;
	req->expect.cont	= running_agent->value; 
	running_agent->value	= NIL;
	req->expect.freeSub	= freeSub_request;
	req->input		= input;
	req->expect.next	= running_agent->expects;
	running_agent->expects	= (EXPECT)req;
	append(&sap->requests, req);

	running_agent->state 	= state_expect;
	dequeue(running_agent);
	append(&expect_queue, running_agent);

    }
    return_fail(agent_ans_sync);
}

/*
Providing on a SAP is realized similar as to requesting:
*/

extern OBJ agent_provideProc(OBJ Sap, OBJ cond, OBJ service){
    SAP sap = (SAP)Sap;
    REQUEST req;

    req = first(&sap->requests);
    while (is_elem(&sap->requests, req)){
	copy_some(req->input, 1);
	copy_closure(cond, 1);
	if (EVAL1(cond, req->input) == pack_bool(1)) break;
	req = next(&sap->requests, req);
    }

    if (is_elem(&sap->requests, req)) {
	/* immediate rendezvous */
	free_closure(cond, 1);

	/* hold a hand on input data and continuation of request */
	copy_some(req->input, 1);
	copy_some(req->expect.cont, 1);

	rendezvous(running_agent, service, running_agent->value,
		   req->expect.agent, req->input, req->expect.cont);

    } else {
	/* create new provide */
	PROVIDE prov;

	alloc_small_flat(sizeof_small(struct sPROVIDE), prov);
	prov->expect.agent	= running_agent;
	prov->expect.cont	= running_agent->value;
	running_agent->value	= NIL;
	prov->expect.freeSub	= freeSub_provide;
	prov->cond		= cond;
	prov->service		= service;
	prov->expect.next	= running_agent->expects;
	running_agent->expects	= (EXPECT)prov;
	append(&sap->provides, prov);

	running_agent->state 	= state_expect;
	dequeue(running_agent);
	append(&expect_queue, running_agent);
    }
    return_fail(agent_ans_sync);
}


/* ------------------------------------------------------------------------ */
/* Rendezvous */

/*
A rendezvous is implemented as follows. 

1) The client is queued into the servers client queue.

2) The service command is calculated from the clients input data

3) The service command is extended by appending an activation
   command for the client. Let c be the command, then this would
   be in OPAL

   	LET serviceCom == c followedBy activateClient(Client)

   where activateClientCont is defined as follows:

   	FUN activateClient: agent -> ans -> com
   	DEF activateClient(Client)(Ans) ==
	    call(activateClientProc(Client, Ans)) 

   The function activateClient is actually implemented in OPAL
   to avoid hacking around with HOFs on the C level.
   The function activateClientProc itself is handcoded.

4) The service command is prepended to the servers continuation
   and the server is activated with the resulting command.  

        LET serverCom == serviceCom sequenceCompose serverCont
*/


static void rendezvous(AGENT server, OBJ service, OBJ serverCont,
		       AGENT client, OBJ input,   OBJ clientCont){

    OBJ serviceCom;

    if (debugComm) {
	fprintf(stderr, "server %x meets client %x\n", server, client);
    }

    /* free expects of both agents */
    free_expects(server);
    free_expects(client);

    /* put client into state_client */
    dequeue(client);
    client->value = clientCont;
    client->state = state_client;
    append(&server->clients, client);

    /* calculate service command */
    copy_closure(__AComAgent_AactivateClient, 1);
    copy_structured(client, 1);
    serviceCom = _ACom_AfollowedBy(
    		     EVAL1(service, input),
    		     EVAL1(__AComAgent_AactivateClient, client)
		 );

    /* activate server */
    activate_agent(server, _ACom_AsequenceCompose(serviceCom, serverCont));
}

OBJ _AComAgent_AactivateClientProc(OBJ Client, OBJ ans, OBJ unit){
    AGENT client = (AGENT)Client;

    /* remove from servers queue */
    dequeue(client);	

    /* activate client */
    copy_ans(ans, 1);
    activate_agent(client, exec_cont(client->value, ans));

    /* free reference associated with this function call */
    free_structured(client, 1);

    return ans;
}



/* ------------------------------------------------------------------------ */
/* Scheduling */

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

static void exec_sequence(OBJ);
static void exec_choice(OBJ);

extern long schedule_agent(){
    long nextTimeout;
    OBJ command;

    /* garbage-collect agents and SAPs */
    gc_agents();
    gc_saps();

    /* activate expired timeouts */
    nextTimeout = activate_timeouts();

    /* activate next agent from active queue */
    if (!is_empty(&active_queue)){

	running_agent = first(&active_queue);
	dequeue(running_agent);
	running_agent->state = state_running;

	/* execute next command of agent */
	command = running_agent->value; 

	if (is_com_sequence(command))
	    exec_sequence(command);
	else 
	    exec_choice(command);
    }

    if (!is_empty(&active_queue)) {
	return 0;
    } else {
	return nextTimeout;
    }
}


/*
The function to execute a sequential command of
an agent behaves as follows. The atomic command in front
of the sequence is executed. If on return the state of
the agent is still state_running (that is, the atomic command
has not implemented a specific state switch on the agent), the default 
behavior is to construct the next command by execuating the continuation 
with the given answer and appending the agent to the active queue.

*/

static void exec_sequence(OBJ command){
    OBJ proc, cont, ans;

    decons_com_sequence(command, proc,cont);
    running_agent->value = cont;

    ans = ocs_com_ans_monitor(EVAL1(proc, NIL));

    if (running_agent->state == state_running) {
	/* activate agent  */
	activate_agent(running_agent, exec_cont(running_agent->value, ans));
    }
}


/*
The function to execute a choice command is realized as follows.
The alternatives of the choice are executed one after another
as a sequential command (it is assumed that they aren't choices).
If any of this executions puts the agent in a different state
then state_expect, an alternative of the choice
has been immediatly enabled and the remaining alternatives are 
ignored. 
*/

static void exec_choice(OBJ command){
    OBJ alts, seq;

    decons_com_choice(command, alts);

    do {
	copy_some(alts, 1);
	seq  = _ASeq_Aft(alts);
	alts = _ASeq_Art(alts);
	exec_sequence(seq);
    } while (running_agent->state == state_expect && alts != __ASeq_Slg);
    free_some(alts, 1);
}


/* ------------------------------------------------------------------------ */
/* Initialization */

/*
The structure ComAgent overwrites the variable ocs_top_exec from
_ostart.c with the function below. 
*/

#ifdef HAVE_CLOCK

#include <time.h>

static int top_exec(OBJ topCom){
    unsigned long last_clock = clock();


    /* create top-level agent */
    create_agent(_AComAgent_AmakeTop(topCom));

    /* schedule forever */
    for (;;) {
	unsigned long current_clock;
	long r;

	current_clock = clock();
	agent_timeout_clock = current_clock - last_clock;
	last_clock = current_clock;
	
	r = schedule_agent();

	if (r < 0){
	    /* This is a deadlock, since there are no other
	       sources which may create active agents. */
	    HLT("<schedule>'ComAgent: deadlock");
	} else 
	if (r > 0){
	    /* sleep for r milliseconds */
	    /*
	    usleep(r * 1000);
	    */
	}
    }
}
	    
#else

#define CLOCK_GRAN 20

static void count_clock(int dummy){
    agent_timeout_clock += CLOCK_GRAN;
}


static int top_exec(OBJ topCom){


    /* register SIGALRM */
    ualarm(CLOCK_GRAN * 1000, CLOCK_GRAN * 1000);
    signal(SIGALRM, count_clock);

    /* create top-level agent */
    create_agent(_AComAgent_AmakeTop(topCom));

    /* schedule forever */
    for (;;) {
	
	long r = schedule_agent();
	if (r < 0){
	    /* This is a deadlock, since there are no other
	       sources which may create active agents. */
	    HLT("<schedule>'ComAgent: deadlock");
	} else 
	if (r > 0){
	    /* sleep for r milliseconds */
	    signal(SIGALRM, SIG_DFL);
	    usleep(r * 1000);
	    agent_timeout_clock += r;
	    signal(SIGALRM, count_clock);
	}
    }
}

#endif /* HAVE_CLOCK */

extern int (*ocs_top_exec)(OBJ);

extern void init_AVoid();
extern void init_ANat();
extern void init_ACom();
extern void init_ASeq();

static void init_const_AComAgent()
{ 
    init_AVoid();
    init_ANat();
    init_ACom();
    init_ASeq();

    init_failures();

    debugComm = getenv("OCS_DEBUG_AGENT_COMM") != NULL;
    debugTerm = getenv("OCS_DEBUG_AGENT_TERM") != NULL;


    /* synchronized clock of timeout */
    activate_timeouts();



    ocs_top_exec = top_exec;
}

