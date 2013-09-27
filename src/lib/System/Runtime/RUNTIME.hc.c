/* hand-coded implementation part of RUNTIME */
/* coding scheme version acc-2.1 */



#include "Nat.oc.h"
#include "Com.oc.h"
#include "Seq.oc.h"

#ifndef NULL
#define NULL (void*)0
#endif

#include "unixconfig.h"


/* --- Handcoding of Lookup Functions -------------------------------------- */

extern int (*ocs_top_exec)(OBJ);

extern OBJ _ARUNTIME_AcLink(OBJ Struct, OBJ Void) {

    int (*old_exec)(OBJ) = ocs_top_exec;

    if (ocs_dl_link(data_denotation(Struct))) {
        if (ocs_top_exec != old_exec){
	  char buf[256];
	  sprintf(buf, "usage of structure `%s' eventually changes command execution model (not supported currently)",
	  	  data_denotation(Struct));
          free_denotation(Struct, 1);
	  return _ACom_Afail(make_denotation(buf));
	} else {
          free_denotation(Struct, 1);
          return_okay_nil;
	}
    } else {
        free_denotation(Struct, 1);
    	return _ACom_Afail(make_denotation(ocs_dl_error()));
    }
}


extern OBJ _ARUNTIME_AcLookup(OBJ Sym, OBJ Void) {
    
    OBJ * var; 

    var = ocs_dl_link_and_resolve(data_denotation(Sym));
    
    if (var != NULL) {
        /* fprintf(stderr, "lookup %s = %x\n", data_denotation(Sym), *var); */
	free_denotation(Sym, 1);
	copy_some(*var,1);
    	return_okay(*var);
    } else {
        /* fprintf(stderr, "lookup %s = ??\n", data_denotation(Sym)); */
	free_denotation(Sym, 1);
    	return _ACom_Afail(make_denotation(ocs_dl_error()));
    }
}


extern OBJ _ARUNTIME_AcRedefine(OBJ Sym, OBJ Obj, OBJ Void) {

    OBJ * var;

    var = ocs_dl_link_and_resolve(data_denotation(Sym));
    free_denotation(Sym, 1);
    
    if (var != NULL) {
	free_some(*var,1);
	*var = Obj;
    	return_okay_nil;
    } else {
    	return _ACom_Afail(make_denotation(ocs_dl_error()));
    }
}


/* --- Free Types ------------------------------------------------------ */

extern OBJ _ARUNTIME_Acons(OBJ Tag, OBJ Comps) {
    OBJ Obj;
    int size, i;
    copy_some(Comps, 1);
    size = unpack_nat(ENTs(Seq,3)(Comps));
    if (size == 0) {
    	/* use Tag as representation */
	return Tag;
    } else {
    	if (size >= big_escape_ssize){
	    HLT("cons'RUNTIME: size of constructed object too large");
	}		
    	alloc_small(size + 1, Obj);
    	data_small(Obj)[0] = Tag;
    	for (i = 1; size > 0; i++, size--) {
		copy_some(Comps, 1);
		data_small(Obj)[i] = ENT(Seq,ft)(Comps);
		Comps = ENT(Seq,rt)(Comps);
    	}
    	return Obj;
    }
}


extern OBJ _ARUNTIME_Atest_(OBJ Obj, OBJ Tag) {
    OBJ Res;
    if (is_primitive(Obj)) {
    	return pack_clean_bool(Obj == Tag);
    } else {
    	Res = pack_clean_bool(Tag == data_small(Obj)[0]);
        free_structured(Obj, 1);
	return Res;
    }
}

extern OBJ _ARUNTIME_Atag(OBJ Obj) {
    OBJ Res;
    if (is_primitive(Obj)) {
    	return Obj;
    } else {
        OBJ Tag = data_small(Obj)[0];
        free_structured(Obj, 1);
	return Tag;
    }
}

extern OBJ _ARUNTIME_Asel(OBJ Obj, OBJ Inx) {
    if (is_primitive(Obj)) {
HLT("sel\'RUNTIME:obj**nat->obj: applied to 0-ary constructed value");
    } else {
	OBJ Res = data_small(Obj)[unpack_nat(Inx)+1];
	/* FIXME: exploit uniqueness for efficiency */
	copy_some(Res, 1);
	free_structured(Obj, 1);
	return Res;
    }
}

extern void init_ANat();
extern void init_ACom();
extern void init_ASeq();

static void init_const_ARUNTIME() {
    init_ANat();
    init_ACom();
    init_ASeq();
    __ARUNTIME_AundefObj = pack_word(max_word);
}

