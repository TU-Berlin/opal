/* hand-coded implementation part of Tcl */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-10-12 18:41:32 $ ($Revision: 1.2 $)
*/

#include "Nat.h"
#include "Int.h"
#include "Real.h"
#include "String.h"
#include "Seq.h"
#include "Com.h"

#include <stdlib.h>
#include <stdio.h>

#undef TCL_STANDALONE 

/* globals */

static Tcl_Interp * convInterp;	/* interpreter used for conversion functions */
static OBJ NatNegative;		/* failure answer for asNat */


#undef malloc_aux
#define malloc_aux malloc

#undef free_aux
#define free_aux free

static int traceExec = 0;

/* ***************************************************************************
 * auxiliary functions 
 */


	/* convert from string to cstring */

#define STATIC  1
#define DYNAMIC 0

static char * str2cstr(OBJ Str, int mode){
    OBJ Len;
    int size;
    char * buf;

    copy_some(Str, 1);
    AString_S3(Str, Len);
    size = unpack_nat(Len) + 1;
    if (mode == STATIC && size <= CHARBUFSIZE){
	buf = charbuf;
    } else {
	buf = (char*)malloc_aux(size);
    }
    get_string(Str, buf, size);
    return buf;
}

	/* free result from str2cstr */

static void free_str2cstr(char * str){
    if (str != charbuf){
	free_aux(str);
    }
}


	/* convert from argc,argv to seq[string] */

static OBJ argv2seq(int argc, char * argv[]){
    OBJ Res, Str, Tmp;
    Res = __ASeq_Slg;
    while (argc > 0){
	argc--;
	Str = make_string(argv[argc]);
	ASeq_Sii(Str, Res, Tmp);
	Res = Tmp;
    }
    return Res;

}
	/* convert from seq[string] to argc,argv */

static void seq2argv(OBJ Seq, int * argcp, char ** argvp[]){
    int argc, i; char ** argv;
    OBJ Len;

    copy_some(Seq, 1);
    ASeq_S3(Seq, Len);

    argc = unpack_nat(Len);

    argv = (char**)malloc_aux((argc+1) * sizeof(char*));
    argv[argc] = NULL;

    for (i = 0; i < argc; i++){
	OBJ Str, Tmp;
	copy_some(Seq, 1);
	ASeq_Aft(Seq, Str); ASeq_Art(Seq, Tmp); Seq = Tmp;
	argv[i] = str2cstr(Str, DYNAMIC);
    }
    *argcp = argc; *argvp = argv;
}

    /* free argc,argv obtained from seq2argv */

static void free_seq2argv(int argc, char * argv[]){
    int i;

    for (i = argc; i > 0; i--){
	free_str2cstr(argv[i-1]);
    }
    free_aux((void *)argv);
}


    /* return failure stored in tcl interpreter */

#define return_tcl_fail(interp){\
    OBJ _Error;						\
    _Error = make_denotation(interp->result);		\
    Tcl_ResetResult(interp);				\
    return _ACom_Afail(_Error);				\
}

    /* return string stored in tcl interpreter */

#define return_tcl_string(interp){\
    OBJ _Str;						\
    _Str = make_string(interp->result);			\
    Tcl_ResetResult(interp);				\
    return_okay(_Str); 					\
}

    /* return okay(nil) */

#define return_tcl_okay(interp){\
    Tcl_ResetResult(interp);				\
    return_okay_nil; 					\
}


/* ***************************************************************************
 * creating interpreters
 */



extern OBJ _ATcl_AcreateIntrp(OBJ State, OBJ Unit) /* createIntrp */
{   INTERP interp;
    interp = (INTERP)malloc_aux(sizeof(struct sINTERP));
    interp->tcl = Tcl_CreateInterp();
#ifdef TCL_STANDALONE
    Tcl_InitStandAlone(interp->tcl);
#else
    Tcl_Init(interp->tcl);
#endif
    interp->State = State;
    return_okay(pack_interpreter(interp));
}

extern OBJ _ATcl_AdeleteIntrp(OBJ Interp,OBJ Unit) /* deleteIntrp */
{
    INTERP interp = unpack_interpreter(Interp);
    Tcl_DeleteInterp(interp->tcl);
    free_some(interp->State,1);
    free_aux(interp);
    return_okay_nil;
}


/* ***************************************************************************
 * accessing the Opal state
 */



extern OBJ _ATcl_AgetState(OBJ Interp, OBJ Unit) /* getState */ {
    INTERP interp = unpack_interpreter(Interp);
    copy_some(interp->State,1);
    return_okay(interp->State);
}

extern OBJ _ATcl_AupdState(OBJ Interp,OBJ UpdFun, OBJ Unit) /* updState */ {
    INTERP interp = unpack_interpreter(Interp);
    interp->State = EVAL1(UpdFun, interp->State);
    return_okay_nil;
}


/* ***************************************************************************
 * defining commands
 */


static Tcl_CmdProc execFun, execCom;
static Tcl_CmdDeleteProc deleteFun,deleteCom;	/* forward declarations */

typedef struct sCMDDEF {	/* client data used for commands */
    INTERP interp;	/* pointer to interpreter, inclusive opal state */
    OBJ Clos;		/* closure describing command */
} * CMDDEF;
    

extern OBJ _ATcl_AdefineFun(OBJ Interp,OBJ Cmd,OBJ Clos,OBJ Unit)/* defineFun */
{
    INTERP interp = unpack_interpreter(Interp);
    CMDDEF def = (CMDDEF)malloc_aux(sizeof(struct sCMDDEF));
    
    def->interp = interp;
    def->Clos = Clos;

    get_denotation(Cmd, charbuf, CHARBUFSIZE);

    Tcl_CreateCommand(interp->tcl, charbuf, 
			  execFun, def, deleteFun);
    return_tcl_okay(interp->tcl);
}

extern OBJ _ATcl_AdefineCom(OBJ Interp,OBJ Cmd,OBJ Clos,OBJ Unit)/* defineCom */
{
    INTERP interp = unpack_interpreter(Interp);
    CMDDEF def = (CMDDEF)malloc_aux(sizeof(struct sCMDDEF));

    def->interp = interp;
    def->Clos = Clos;

    get_denotation(Cmd, charbuf, CHARBUFSIZE);

    Tcl_CreateCommand(interp->tcl, charbuf, 
			  execCom, def, deleteCom);
    return_tcl_okay(interp->tcl);
}

static int execFun(ClientData xdef, Tcl_Interp * interp_tcl, 
		   int argc, char * argv[]){
    OBJ ArgS = argv2seq(argc, argv); 
    CMDDEF def = (CMDDEF)xdef; 
    OBJ Res, Ok;

    if (traceExec)
        fprintf(stderr,"Tcl: opalfun %s\n", argv[0]);

    copy_closure(def->Clos, 1);
    Res = EVAL2(def->Clos, pack_interpreter(def->interp), ArgS);

    copy_some(Res,1);
    ACom_Aokay_(Res, Ok);

    if (unpack_bool(Ok)){
	OBJ Str; char * str; 
	ACom_Adata(Res, Str);
	str = str2cstr(Str, DYNAMIC);
	Tcl_SetResult(interp_tcl, str, (Tcl_FreeProc *) free_str2cstr);
	return TCL_OK;
    } else {
	OBJ Den;
	ACom_Aerror(Res, Den);
	/* use the initialized static interp->result */
	get_denotation(Den, interp_tcl->result, TCL_RESULT_SIZE);
	return TCL_ERROR;
    }
}


static int execCom(ClientData xdef, Tcl_Interp * interp_tcl, 
		   int argc, char * argv[]){
    OBJ ArgS = argv2seq(argc, argv); 
    CMDDEF def = (CMDDEF)xdef; 
    OBJ Com, Res, Ok;

    if (traceExec) {
	int i;
	fputs("Tcl-Opal > ", stderr);
	for (i = 0; i < argc; i++) {
	    fputs(argv[i], stderr); fputc(' ', stderr);
	}
	fputc('\n', stderr);
    }

    copy_closure(def->Clos, 1);
    Com = EVAL2(def->Clos, pack_interpreter(def->interp), ArgS);

    Res = _ACom_AexecCom(Com);

    copy_some(Res,1);
    ACom_Aokay_(Res, Ok);

    if (unpack_bool(Ok)){
	OBJ Str; char * str; 
	ACom_Adata(Res, Str);
	str = str2cstr(Str, DYNAMIC);
	if (traceExec) {
	    fputs("Tcl-Opal < ", stderr);
	    fputs(str, stderr);
	    fputc('\n', stderr);
	}
	Tcl_SetResult(interp_tcl, str, (Tcl_FreeProc *) free_str2cstr);
	return TCL_OK;
    } else {
	OBJ Den;
	ACom_Aerror(Res, Den);
	/* use the initialized static interp->result */
	get_denotation(Den, interp_tcl->result, TCL_RESULT_SIZE);
	if (traceExec) {
	    fputs("Tcl-Opal ? ", stderr);
	    fputs(interp_tcl->result, stderr);
	    fputc('\n', stderr);
	}
	return TCL_ERROR;
    }
}

static void deleteFun(ClientData xdef){
    CMDDEF def = (CMDDEF)xdef;

    free_closure(def->Clos,1);
    free_aux(def);
}

static void deleteCom(ClientData xdef){
    CMDDEF def = (CMDDEF)xdef;

    free_closure(def->Clos,1);
    free_aux(def);
}


extern OBJ _ATcl_AundefineIntrp(OBJ Interp,OBJ Cmd,OBJ Unit) {
    INTERP interp =  unpack_interpreter(Interp);

    get_denotation(Cmd, charbuf, CHARBUFSIZE);
    if (Tcl_DeleteCommand(interp->tcl, charbuf) == TCL_OK){
	return_tcl_okay(interp->tcl);
    } else {
	return_tcl_fail(interp->tcl);
    }
}


/* ***************************************************************************
 * converting Tcl data
 */


extern OBJ _ATcl_AasNat(OBJ Str) /* asNat */ {
    int val;
    get_string(Str, charbuf, CHARBUFSIZE);
    if (Tcl_GetInt(convInterp, charbuf, &val) == TCL_OK){
	if (val >= 0){
	    return_okay(pack_nat(val))
	} else {
	    return_fail(NatNegative);
	}
    }  else {
	return_tcl_fail(convInterp);
    }
}

extern OBJ _ATcl_AasInt(OBJ Str) /* asInt */ {
    int val;
    get_string(Str, charbuf, CHARBUFSIZE);
    if (Tcl_GetInt(convInterp, charbuf, &val) == TCL_OK){
	return_okay(pack_int(val))
    }  else {
	return_tcl_fail(convInterp);
    }
}

extern OBJ _ATcl_AasBool(OBJ Str) /* asBool */ {
    int val;
    get_string(Str, charbuf, CHARBUFSIZE);
    if (Tcl_GetBoolean(convInterp, charbuf, &val) == TCL_OK){
	return_okay(pack_bool(val))
    }  else {
	return_tcl_fail(convInterp);
    }
}


extern OBJ _ATcl_AasReal(OBJ Str) /* asReal */ {
    double val;
    get_string(Str, charbuf, CHARBUFSIZE);
    if (Tcl_GetDouble(convInterp, charbuf, &val) == TCL_OK){
	OBJ Res;
	make_real(val, Res);
	return_okay(Res);
    }  else {
	return_tcl_fail(convInterp);
    }
}


extern OBJ _ATcl_AasList(OBJ Str) /* asList */ {
    int argc; char ** argv;
    char * str;
    OBJ Res;

    str = str2cstr(Str, STATIC);

    if ( Tcl_SplitList(convInterp, str, &argc, &argv) == TCL_OK ){
	OBJ Res = argv2seq(argc, argv);
	free((void *)argv);
	free_str2cstr(str);
	return_okay(Res);
    } else {
	free_str2cstr(str);
	return_tcl_fail(convInterp);
    }
}

extern OBJ _ATcl_AasString(OBJ Seq) /* asString */ {
    int argc; char ** argv; 
    char * str;
    OBJ Res;

    seq2argv(Seq, &argc, &argv);
    str = Tcl_Merge(argc, argv);
    Res = make_string(str);
    free((void*)str);
    free_seq2argv(argc, argv);
    return Res;
}

extern OBJ _ATcl_AcommandCompleteDen(OBJ Den) {
    int res = Tcl_CommandComplete(data_denotation(Den));
    free_denotation(Den, 1);
    return pack_bool(res);
}

    
/* ***************************************************************************
 * evaluating scripts 
 */


extern OBJ _ATcl_AevalString(OBJ Interp,OBJ Str,OBJ Unit) /* evalString */ {
    INTERP interp = unpack_interpreter(Interp);
    char * str;
    OBJ Res;

    str = str2cstr(Str, DYNAMIC);

    if (traceExec) {
        fputs("Opal-Tcl > ", stderr); fputs(str, stderr); 
        fputc('\n', stderr);
    }

    if ( Tcl_Eval(interp->tcl, str) == TCL_OK ) {
	free_str2cstr(str);
	if (traceExec) {
	    fputs("Opal-Tcl < ", stderr); 
	    fputs(interp->tcl->result, stderr); 
	    fputc('\n', stderr);
	}
        return_tcl_string(interp->tcl);
    } else {
	free_str2cstr(str);
	if (traceExec) {
	    fputs("Opal-Tcl ? ", stderr); 
	    fputs(interp->tcl->result, stderr); 
	    fputc('\n', stderr);
	}
	return_tcl_fail(interp->tcl);
    }
}


extern OBJ _ATcl_AevalFile(OBJ Interp,OBJ File,OBJ Unit) /* evalFile */ {
    INTERP interp = unpack_interpreter(Interp);
    OBJ res;

    get_denotation(File, charbuf, CHARBUFSIZE);
    if ( Tcl_EvalFile(interp->tcl, charbuf) == TCL_OK ) {
        return_tcl_string(interp->tcl);
    } else {
	return_tcl_fail(interp->tcl);
    }
}


/* ***************************************************************************
 * accessing variables 
 */


extern OBJ _ATcl_Aget(OBJ Interp,OBJ Var) /* get */ {
    INTERP interp = unpack_interpreter(Interp);
    char * val;

    get_denotation(Var, charbuf, CHARBUFSIZE);

    val = Tcl_GetVar(interp->tcl, charbuf, TCL_LEAVE_ERR_MSG);

    if ( val != NULL ) {
	Tcl_ResetResult(interp->tcl);
	return_okay(make_string(val));
    } else {
	return_tcl_fail(interp->tcl);
    }
}

extern OBJ _ATcl_AgetGlob(OBJ Interp,OBJ Var) /* getGlob */ {
    INTERP interp = unpack_interpreter(Interp);
    char * val;

    get_denotation(Var, charbuf, CHARBUFSIZE);

    val = Tcl_GetVar(interp->tcl, charbuf, TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG);

    if ( val != NULL ) {
	Tcl_ResetResult(interp->tcl);
	return_okay(make_string(val));
    } else {
	return_tcl_fail(interp->tcl);
    }
}

extern OBJ _ATcl_AgetArr(OBJ Interp,OBJ Var,OBJ Key) /* getArr */ {
    INTERP interp = unpack_interpreter(Interp);
    char  * val;

    get_denotation(Var, charbuf, CHARBUFSIZE/2);
    get_denotation(Key, charbuf + CHARBUFSIZE/2, CHARBUFSIZE/2 - 1);

    val == Tcl_GetVar2(interp->tcl, charbuf, charbuf + CHARBUFSIZE/2,
    		       TCL_LEAVE_ERR_MSG);

    if ( val != NULL ) {
	Tcl_ResetResult(interp->tcl);
	return_okay(make_string(val));
    } else {
	return_tcl_fail(interp->tcl);
    }
}

extern OBJ _ATcl_AgetArrGlob(OBJ Interp,OBJ Var,OBJ Key) /* getArr */ {
    INTERP interp = unpack_interpreter(Interp);
    char  * val;

    get_denotation(Var, charbuf, CHARBUFSIZE/2);
    get_denotation(Key, charbuf + CHARBUFSIZE/2, CHARBUFSIZE/2 - 1);

    val = Tcl_GetVar2(interp->tcl, charbuf, charbuf + CHARBUFSIZE/2,
    		       TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG);

    if ( val != NULL ) {
	Tcl_ResetResult(interp->tcl);
	return_okay(make_string(val));
    } else {
	return_tcl_fail(interp->tcl);
    }
}


extern OBJ _ATcl_Aset(OBJ Interp,OBJ Var,OBJ Val,OBJ Unit) /* set */ {
    INTERP interp = unpack_interpreter(Interp);
    char * val;

    get_denotation(Var, charbuf, CHARBUFSIZE);
    val = str2cstr(Val, STATIC);

    if ( Tcl_SetVar(interp->tcl, charbuf, val, TCL_LEAVE_ERR_MSG) != NULL ){
	free_str2cstr(val);
	return_tcl_okay(interp->tcl);
    } else {
	free_str2cstr(val);
	return_tcl_fail(interp->tcl);
    }
}

extern OBJ _ATcl_AsetGlob(OBJ Interp,OBJ Var,OBJ Val,OBJ Unit) /* set */ {
    INTERP interp = unpack_interpreter(Interp);
    char * val;

    get_denotation(Var, charbuf, CHARBUFSIZE);
    val = str2cstr(Val, STATIC);

    if ( Tcl_SetVar(interp->tcl, charbuf, val, 
    		    TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG) != NULL ){
	free_str2cstr(val);
	return_tcl_okay(interp->tcl);
    } else {
	free_str2cstr(val);
	return_tcl_fail(interp->tcl);
    }
}

extern OBJ _ATcl_AsetArr(OBJ Interp,OBJ Var,OBJ Val,OBJ Key, OBJ Unit) 
								/* setArr */ {
    INTERP interp = unpack_interpreter(Interp);
    char * val;

    get_denotation(Var, charbuf, CHARBUFSIZE/2);
    get_denotation(Key, charbuf + CHARBUFSIZE/2, CHARBUFSIZE/2 - 1);
    val = str2cstr(Val, STATIC);

    if ( Tcl_SetVar2(interp->tcl, charbuf, charbuf + CHARBUFSIZE/2,
    		     val, TCL_LEAVE_ERR_MSG) != NULL ) {
	free_str2cstr(val);
	return_tcl_okay(interp->tcl);
    } else {
	free_str2cstr(val);
	return_tcl_fail(interp->tcl);
    }
}


extern OBJ _ATcl_AsetArrGlob(OBJ Interp,OBJ Var,OBJ Val,OBJ Key, OBJ Unit) 
								/* setArr */ {
    INTERP interp = unpack_interpreter(Interp);
    char * val;

    get_denotation(Var, charbuf, CHARBUFSIZE/2);
    get_denotation(Key, charbuf + CHARBUFSIZE/2, CHARBUFSIZE/2 - 1);
    val = str2cstr(Val, STATIC);

    if ( Tcl_SetVar2(interp->tcl, charbuf, charbuf + CHARBUFSIZE/2,
    		     val, TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG) != NULL ) {
	free_str2cstr(val);
	return_tcl_okay(interp->tcl);
    } else {
	free_str2cstr(val);
	return_tcl_fail(interp->tcl);
    }
}


static init_const_ATcl(){
    init_ANat();
    init_AInt();
    init_AReal();
    init_AString();
    init_ASeq();
    init_ACom();

    traceExec = getenv("OCS_DEBUG_TCL") != NULL;

    convInterp = Tcl_CreateInterp();
#ifdef TCL_STANDALONE
    Tcl_InitStandAlone(convInterp);
#else
    Tcl_Init(convInterp);
#endif
    NatNegative = declare_failure_answer("natural can't be negative");
}


/* supply an dummy application init -- dont know who is referring this ... */

extern int Tcl_AppInit(Tcl_Interp * interp){
    return TCL_OK;
}

