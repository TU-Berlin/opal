/* hand-coded interface part of Com */
/* + header of command agent scheduler */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:17 $ ($Revision: 1.1.1.1 $)
*/

/* Answers */

#define free_ans(ans,n) free_structured(ans, n)
#define copy_ans(ans,n) copy_structured(ans, n)

#define cons_ans_okay(data,r) /* coding scheme dependent ! */ \
    {PRD(1,0,r); FLD(r,1)=data;}

#define cons_ans_fail(msg,r)  /* coding scheme dependent ! */ \
    {PRD(1,1,r); FLD(r,1)=msg;}


extern OBJ ans_okay_nil;		
    /* the answer "okay(nil)".
    */

#define return_okay_nil \
	{ copy_ans(ans_okay_nil,1); return ans_okay_nil; }

#define return_okay(data)\
	{OBJ __r; cons_ans_okay(data, __r); return __r;}
    /* the answer "okay(data)" */

#define return_fail(failure) { copy_ans(failure,1); return failure; }
    /* return a given failure answer */

    
extern OBJ declare_failure_answer(char * message);
    /* creation of a new "fail" answer.
    */


/* Commands */

#define free_com(com,n) free_structured(com, n)
#define copy_com(com,n) copy_structured(com, n)

#define is_com_sequence(com) ISTGPRD(com, 0)

#define decons_com_sequence(com, atom,cont) {	\
    atom = FLD(com, 1); cont = FLD(com, 2); 	\
    if (excl_structured(com, 1)){		\
         dispose_structured_flat(com);		\
    } else {					\
         decr_structured(com, 1); 		\
         copy_some(atom, 1);copy_some(cont, 1); \
    } 						\
}

#define decons_com_choice(com, alts) {		\
    alts = FLD(com, 1); 			\
    if (excl_structured(com, 1)){		\
         dispose_structured_flat(com);		\
    } else {					\
         decr_structured(com, 1); 		\
         copy_some(alts, 1); 			\
    } 						\
}


/* Monitoring of command execution */

/* Function to monitor the result of command execution. This
function is called after each execution of an atomic command,
getting passed the answer. It may modify an OKAY answer to FAIL,
e.g. to track C-c interrrupts. */

extern OBJ ocs_com_ans_monitor(OBJ);

/* Defining the method for answer monitoring; returns the old
method. The default method is the identity function. */

extern OBJ (*ocs_com_ans_monitor_def_method(OBJ (*)(OBJ)))(OBJ);

