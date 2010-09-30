/* hand-coded implementation part of EQUALITY */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/
#include <unixconfig.h>

/* es geht doch nichts ueber echtes debugging */
#if 0
static int tmp;
#include <stdio.h>
#define RETURN(d,x) {tmp = x; fprintf(stderr, "[" #d "] debug equal: %d", tmp); fflush(stderr); return tmp;}
#define AT(d) {fprintf(stderr, "at <" #d ">\n"); fflush(stderr);}
#else
#define RETURN(d,x) {return(x);}
#define AT(d)
#endif

static int equal(OBJ x1,OBJ x2, int abort){
    AT(@);
    if (x1 == x2) RETURN(a, 1)
    else if ((x1 == NIL) || (x2 == NIL)) RETURN(a-0, 0)
    else {
        AT(@1);
	if (tst_sflag(x1,closure_sflag) || tst_sflag(x2,closure_sflag)){
	  if (abort) {
	    HLT("equal'NEQUALITY: function values not comparable"); 
	  } else {
	    RETURN(b, 0);
	  };
	};
	AT(@2);
        if (is_structured(x1) && is_structured(x2)){
	    int i1 = _size(_header(x1)), i2 = _size(_header(x2));	    
	    AT(A);
	    if (i1 == i2){
		int n; OBJ *d1, *d2;
		if (is_big_structured(x1)){
		    AT(B);
		    /* these are big cells */
		    n = unpack_word(((BCELL)x1)->size);
		    if (n != unpack_word(((BCELL)x2)->size)) RETURN(b, 0);
		    AT(C);
		    d1 = data_big(x1);
		    d2 = data_big(x2);
		    AT(D);
		} else {
		    AT(E);
		    n = i1 % flat_offset_ssize;
		    d1 = data_small(x1);
		    d2 = data_small(x2);
		    AT(F);
		}
		if (is_flat_structured(x1)){
		    AT(G);
		    /* these are flat cells; we can do a memcmp */
		    RETURN(c, !memcmp((void*)d1,(void*)d2,n * sizeof(OBJ)));
		} else {
		    /* we do a recursive compare */
		    AT(H);
		    while (n > 0){
		      if((*d1 != NIL)||(*d2 != NIL)){ /* at least one != NIL */
		        if(*d1 == NIL) RETURN(d-1,0);
			if(*d2 == NIL) RETURN(d-2,0);
		  	if (!equal(*d1,*d2, abort)) RETURN(d,0);
		      };
		      AT(I);
		      d1++; d2++; n--;
		    }
		    AT(J);
		    RETURN(e, 1);
		}
	    } else RETURN(f, 0);
	} else RETURN(g, 0);
    }
}


extern OBJ _AEQUALITY_Aequal(OBJ x1, OBJ x2){
    int r;
    r = equal(x1,x2, 1);
    free_some(x1,1); free_some(x2,1);
    return pack_clean_bool(r);
}
		
extern OBJ _AEQUALITY_AwEqual(OBJ x1, OBJ x2){
    int r;
    r = equal(x1,x2, 0);
    free_some(x1,1); free_some(x2,1);
    return pack_clean_bool(r);
}
		
static init_const_AEQUALITY()
{}
