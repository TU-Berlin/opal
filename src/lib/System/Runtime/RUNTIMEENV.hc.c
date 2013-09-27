/* hand-coded implementation part of RUNTIMEENV */
/* coding scheme version acc-2.1 */

#ifndef NULL
#define NULL (void*)0
#endif

#include <stdarg.h>
#include <string.h>
#include <stdio.h>

#include "Nat.oc.h"
#include "Seq.oc.h"
#include "Com.oc.h"
#include "Pair.oc.h"


static OBJ ClosSymId;
static OBJ GlobalEnv = NULL;
static void (*saved_halt_handler)(char *) = NULL;
static void (*saved_exit_handler)(int) = NULL;
static OBJ HaltHandler = NULL;
static OBJ passv[MAXRANK];  
static CODE *method_table;

#define setup_halt_handler() {\
    if (HaltHandler != NULL){\
        saved_halt_handler = ocs_halt_def_method(halt_handler);\
        saved_exit_handler = ocs_exit_def_method(exit_handler);\
    }\
}

#define restore_halt_handler() {\
    if (saved_halt_handler != NULL) {\
    	ocs_halt_def_method(saved_halt_handler);\
    	ocs_exit_def_method(saved_exit_handler);\
	saved_halt_handler = NULL;\
    }\
}

static void halt_handler(char *msg){
    OBJ Env, Handler;
    Env = GlobalEnv;
    Handler = HaltHandler;
    GlobalEnv = NULL;
    HaltHandler = NULL;
    restore_halt_handler();
    EVAL2(Handler, Env, make_denotation(msg));
    HLT("<error-handler>'RUNTIMEENV: handler must never return");
}

static void exit_handler(int code){
    char buf[64];
    OBJ Env, Handler;
    Env = GlobalEnv;
    Handler = HaltHandler;
    GlobalEnv = NULL;
    HaltHandler = NULL;
    restore_halt_handler();
    sprintf(buf,"spontaneously exited with code %d", code);
    EVAL2(Handler, Env, make_denotation(buf));
    HLT("<error-handler>'RUNTIMEENV: handler must never return");
}

static int seq_to_argv(OBJ Seq, int at, OBJ argv[]){
    while (Seq != VARs(Seq,lg)){
	if (at >= MAXRANK){
	    HLT("<intern>'RUNTIMEENV: argument sequence too long");
	}
	copy_structured(Seq, 1);
	argv[at++] = ENT(Seq,ft)(Seq);
	Seq = ENT(Seq,rt)(Seq);
    }
    return at;
}

static OBJ argv_to_seq(int at, int cnt, OBJ argv[], OBJ Seq){
    at = at + cnt;
    while (cnt > 0){
	Seq = ENTs(Seq,ii)(argv[--at], Seq);
	cnt--;
    }
    return Seq;
}
	
static OBJ evalMethod(OBJ Clos, int argc, OBJ argv[]){
    WORD info = unpack_word(((CLOSURE)Clos)->info);
    int old_argc = argc_closure_info(info);
    int rank = rank_closure_info(info);
    int i,j; 
    if (old_argc + argc < rank) {
    	/* just store arguments */
	if (!excl_structured(Clos,1)) {
	    OBJ OldClos = Clos;
	    Clos = _dpcls(Clos);
	    decr_structured(OldClos, 1);
	}
	((CLOSURE)Clos)->info = 
	    pack_word(make_closure_info(rank_closure_info(info),
					argc_closure_info(info) + argc));
	for (i = old_argc, j = 0; j < argc; i++, j++){
	    data_closure(Clos)[i] = argv[j];
	}
	return Clos;
    } else {
    	/* call evaluation entry */
	if (!GlobalEnv) {
	    HLT("<eval>'RUNTIMEENV: no environment");
	    return NIL;
	} else {
	    OBJ Entry = (OBJ)((CLOSURE)Clos)->entry;
	    int excl = excl_structured(Clos, 1);
	    OBJ ArgSeq, ResSeq, Repr, Meth, Env, Obj, RestArgs;
	    int rest_argc = old_argc + argc - rank;

	    ArgSeq = argv_to_seq(0, argc - rest_argc, argv, VARs(Seq,lg));
	    if (rest_argc > 0){
		/* we have to save the remaining 
		   arguments since argv may be static */
		alloc_small_flat(rest_argc, RestArgs);
		for (i = argc - rest_argc, j = 0; i < argc; i++, j++){
		    data_small(RestArgs)[j] = argv[i];
		}
	    }
	    ArgSeq = argv_to_seq(0, old_argc, data_closure(Clos), ArgSeq);
	    Repr = data_small(Entry)[0]; Meth = data_small(Entry)[1];
	    if (excl){
		free_denotation(((CLOSURE)Clos)->symbolid, 1);
		dispose_structured_flat(((CLOSURE)Clos)->entry);
		dispose_structured_flat(Clos);
	    } else {
		copy_some(Repr, 1); copy_some(Meth, 1);
		for (i = 0; i < old_argc; i++){
		    copy_some(data_closure(Clos)[i],1);
		}
		decr_structured(Clos, 1);
	    }
	    Env = GlobalEnv;
	    GlobalEnv = NULL;

	    restore_halt_handler();
	    Obj = EVAL3(Meth, Repr, ArgSeq, Env);
	    setup_halt_handler();

	    ResSeq = data_small(Obj)[0];
	    GlobalEnv = data_small(Obj)[1];
	    dispose_structured_flat(Obj);

	    if (rest_argc > 0){
		/* Eval with rest args */
		Obj = ocs_var_eval(ENT(Seq,ft)(ResSeq), rest_argc, 
				   data_small(RestArgs));
		dispose_structured_flat(RestArgs);
	    } else {
		copy_some(ResSeq,1);
		i = unpack_nat(ENTs(Seq,3)(ResSeq));
		if (i > 1) {
		    alloc_small_flat(i, Obj);
		    seq_to_argv(ResSeq, 0, data_small(Obj));
		} else {
		    Obj = ENT(Seq,ft)(ResSeq);
		}
	    }
	    return Obj;
	}
    }
}


extern OBJ _ARUNTIMEENV_Aclosure(OBJ Rank, OBJ Repr, OBJ Mth) {
    CLOSURE clos; OBJ Entry; int i;
    int rank = unpack_nat(Rank);

    alloc_small(2, Entry);
    data_small(Entry)[0] = Repr; data_small(Entry)[1] = Mth;

    alloc_small(sizeof_closure(rank),clos); 
    set_sflag(clos,closure_sflag);
    clos->entry = Entry; 
    clos->mttab = pack_pointer(method_table);
    copy_denotation(ClosSymId, 1);
    clos->symbolid = ClosSymId;
    clos->info = pack_word(make_closure_info(rank,0));
    for (i = 0; i < rank-1; i++) { /* closure holds one less then rank ! */
        data_closure(clos)[i] = NIL;
    }
    return clos;
}


extern TUP2 _ARUNTIMEENV_Ainternapply(OBJ Clos, OBJ Dim, OBJ Args, 
				      OBJ Env, OBJ Handler){
    TUP2 tup2;
    OBJ Res;
    int argc;
    if (GlobalEnv){
    	HLT("apply'RUNTIMEENV: environment already defined");
    }
    GlobalEnv = Env;

    copy_some(Handler, 1);
    if (unpack_bool(ENT(RUNTIMEENV, special_)(Handler))) {
	HaltHandler = ENT(RUNTIMEENV, func)(Handler);
    } else
	HaltHandler = NULL;

    argc = seq_to_argv(Args, 0, passv);
    setup_halt_handler();
    Res = ocs_var_eval(Clos, argc, passv);
    restore_halt_handler();

    if (HaltHandler != NULL){
	free_some(HaltHandler, 1);
	HaltHandler = NULL;
    }

    tup2.c2 = GlobalEnv;
    GlobalEnv = NULL;
    if (unpack_nat(Dim) > 1){
	tup2.c1 = 
	    argv_to_seq(0, unpack_nat(Dim), data_small(Res), VARs(Seq,lg));
	dispose_structured_flat(Res);
    } else {
	tup2.c1 = ENTs(Seq,ii)(Res, VARs(Seq,lg));
    }
    return tup2;
}


extern OBJ _ARUNTIMEENV_AcExec(OBJ Com, OBJ Env, OBJ Handler, OBJ Void) {
    OBJ Ans;
    if (GlobalEnv){
    	HLT("exec'RUNTIMEENV: environment already defined");
    }
    GlobalEnv = Env;

    copy_some(Handler, 1);
    if (unpack_bool(ENT(RUNTIMEENV, special_)(Handler))) {
	HaltHandler = ENT(RUNTIMEENV, func)(Handler);
    } else
	HaltHandler = NULL;

    setup_halt_handler();
    Ans = ENT(Com, execCom)(Com);
    restore_halt_handler();
    if (HaltHandler != NULL){
	free_some(HaltHandler, 1);
	HaltHandler = NULL;
    }
    Env = GlobalEnv; 
    GlobalEnv = NULL;
    return_okay(ENTs(Pair,7)(Ans, Env));
}

extern int (*ocs_top_exec)(OBJ);

extern OBJ _ARUNTIMEENV_AcTopExec(OBJ Com, OBJ Env, OBJ Handler, OBJ Void) {
    int ans;
    if (GlobalEnv){
    	HLT("exec'RUNTIMEENV: environment already defined");
    }
    GlobalEnv = Env;

    copy_some(Handler, 1);
    if (unpack_bool(ENT(RUNTIMEENV, special_)(Handler))) {
	HaltHandler = ENT(RUNTIMEENV, func)(Handler);
    } else
	HaltHandler = NULL;

    setup_halt_handler();
    ans = (*ocs_top_exec)(Com);
    restore_halt_handler();
    if (HaltHandler != NULL){
	free_some(HaltHandler, 1);
	HaltHandler = NULL;
    }
    Env = GlobalEnv; 
    GlobalEnv = NULL;
    return_okay(ENTs(Pair,7)(_ACom_Aokay(pack_nat(ans)), Env));
}

extern TUP2 _ARUNTIMEENV_Acontents(OBJ Clos) {
    TUP2 r;
    WORD info = unpack_word(((CLOSURE)Clos)->info);
    int i = argc_closure_info(info) - 1;
    OBJ Args = VARs(Seq,lg);

    while (i >= 0){
	copy_some(data_closure(Clos)[i], 1);
	Args = ENTs(Seq,ii)(data_closure(Clos)[i], Args);
	i--;
    }
    r.c2 = Args;
    if (strcmp(data_denotation(((CLOSURE)Clos)->symbolid), 
	       data_denotation(ClosSymId)) == 0){
	OBJ  Repr = data_small(((CLOSURE)Clos)->entry)[0];
	copy_some(Repr, 1);
	r.c1 = ENT(RUNTIMEENV,dyn)(Repr);
    } else {
	r.c1 = ENT(RUNTIMEENV,coded)(((CLOSURE)Clos)->symbolid);
	copy_denotation(r.c1, 1);
    }
    free_structured(Clos, 1);
    return r;
}

extern OBJ _ARUNTIMEENV_Arank(OBJ Clos) /* rank */ {
    OBJ r;
    r = pack_nat(rank_closure_info(unpack_word(((CLOSURE)Clos)->info)));
    free_structured(Clos, 1);
    return r;
}

/* But in oc2 handcoding templates; this stuff belongs
   to OCS/<Struct>.c */

extern OBJ _ARUNTIMEENV_20_c(OBJ x1) /* contents */
{OBJ r;
 {TUP2 t=_ARUNTIMEENV_20(x1);
  PRD1(2,r);
  FLD1(r,1)=t.c1;
  FLD1(r,2)=t.c2;}
 return r;}

extern OBJ _ARUNTIMEENV_35_c(OBJ x1,OBJ x2,OBJ x3,OBJ x4,OBJ x5) /* internapply */
{OBJ r;
 {TUP2 t=_ARUNTIMEENV_35(x1,x2,x3,x4,x5);
  PRD1(2,r);
  FLD1(r,1)=t.c1;
  FLD1(r,2)=t.c2;}
 return r;}

extern void init_ANat();
extern void init_ACom();
extern void init_ASeq();
extern void init_APair();

static void init_const_ARUNTIMEENV() {
    init_ANat();
    init_ACom();
    init_ASeq();
    init_APair();
    method_table = ocs_alloc_var_method_table(evalMethod);
    ClosSymId = make_denotation("<runtime-closure>");
}
