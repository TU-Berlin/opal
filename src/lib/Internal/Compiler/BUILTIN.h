/* subject: Ac unit "BUILTIN" -- provides also all compiler macros
 * author:  wg 7-92
 * version: $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/lib/Internal/Compiler/BUILTIN.h,v 1.1.1.1 1998-06-16 16:00:15 wg Exp $
 */
    

/* #define _MEMSTAT_  */



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* general types
 */

/* some object */
typedef void * OBJ;			

#define is_primitive(o)	 ((WORD)(o)&1)
#define is_structured(o) (!((WORD)(o)&1))

/* some code */
typedef void (*CODE)();			

  /* old fashioned test */
#define _isVal(o)	is_primitive(o)
#define _isRef(o)	is_structured(o)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * primitive objects 
 */

  /* unsigned */

typedef unsigned long WORD;
#define pack_word(n) 	((OBJ)(((WORD)(n)<<1)+1))
#define unpack_word(o)	((WORD)(o)>>1)
extern WORD max_word; 
extern WORD bits_per_word;

  /* old fashioned unsigned  */

typedef WORD IMG;
#define _packVal(n) 	pack_word(n)
#define _unpackVal(o)	unpack_word(o)


  /* signed */

typedef signed long SWORD;
#define pack_sword(n)   ((OBJ)( ((SWORD)(n)*2)^(SWORD)1 ))
#define unpack_sword(o) (((SWORD)(o)^(SWORD)1)/2)
extern SWORD min_sword;
extern SWORD max_sword;


  /* pointers */

#define pack_pointer(c)	((OBJ)((WORD)(c)+1))
#define unpack_pointer(o) ((void*)((WORD)(o)-1))

  /* old  fashioned pointers */

#define _packPtr(c)	pack_pointer(c)
#define _unpackPtr(o)	unpack_pointer(o)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * structured objects 
 */


  /* general header */

	/* 32 rc + 16bit flags + 16bit size */

typedef union {				/* header of some cell */
    struct { unsigned rc;			/* reference count */
	     short unsigned flags; 		/* cell flags */
	     short unsigned size;		/* size index */
	     } info;			/* info if cell is in use */
    void * link;			/* link if cell is not used */
} HEAD;

#define _incRc(h,n)	((h).info.rc+=(n))
#define _decRc(h,n)	((h).info.rc-=(n))
#define _tstRc(h,n)	((h).info.rc==(n))
#define _decTstRc(h,n)  (((h).info.rc-=(n))==0)
#define _size(h)	((h).info.size)
#define _flags(h)	((h).info.flags)
#define _mkHeader(h,s,r) {(h).info.size=s; (h).info.rc=r; (h).info.flags=0;}
#define _link(h)	((h).link)
#define _setLink(h,p)	((h).link = (p))
#define _maxIndex	(unsigned)0xffff
#define _maxRc		(unsigned)0xffffffff

#define _checkRc(h,n,origin) {if((h).info.rc > _maxRc-(n)) \
				   HLT(origin ": RC overflow"); }

#define _objSize(s) 	(((s) + sizeof(OBJ)-1)/sizeof(OBJ))


  /* cell size */

#define first_escape_ssize 126          /* where escape sizes start */
#define big_escape_ssize   126		/* escape size for big cells */
#define foreign_escape_ssize  127	/* escape size for foreign cells */
#define flat_offset_ssize  128		/* flat offset */	
#define off_flat_offset_ssize  127	/* mask to blend out flat offset */	

#define real_ssize(o) (_size(_header(o)) & off_flat_offset_ssize)
#define is_flat_structured(o) (_size(_header(o)) >= flat_offset_ssize)
#define is_big_structured(o) (real_ssize(o) == big_escape_ssize)
#define is_foreign_structured(o) (real_ssize(o) == foreign_escape_ssize)

#define make_flat_ssize(s) ((s) | flat_offset_ssize)


  /* cell flags */

#define byte_flat_sflag  	0
#define closure_sflag		1
#define lazy_closure_eval_sflag	2
#define usr_sflag		3

#define set_sflag(o,flag) {_flags(_header(o)) |= 1 << (flag);}
#define clr_sflag(o,flag) {_flags(_header(o)) &= ~(1 << (flag));}
#define tst_sflag(o,flag) (_flags(_header(o)) & (1 << (flag)))


  /* header of small cell */

typedef struct sCELL {	
    HEAD header;
} * CELL;

#define _header(o) ((CELL)(o))->header 
#define data_small(o) ((OBJ*)((CELL)(o)+1)) 
#define size_small(o) real_ssize(o)

  	/* old fashioned: */
#define _data(o) data_small(o)
#define _totalSize(n) _objSize(sizeof(struct sCELL) + sizeof(OBJ)*(n))


  /* header of big cell */

typedef struct sBCELL {			/* layout of some big cell */
    HEAD header;
    OBJ size;	/* packed size */
} * BCELL;

#define _totalBigSize(n) _objSize(sizeof(struct sBCELL) + sizeof(OBJ)*(n))

#define data_big(o) ((OBJ*)((BCELL)(o)+1)) 
#define size_big(o) unpack_word(((BCELL)(o))->size)


#define _bdata(o) data_big(o)     /* old fashioned */


  /* header of foreign cell */

typedef struct sFCELL {
    HEAD header;
    void (*dispose)(OBJ);
} * FCELL;

#define foreign_dispose(o) (*(((FCELL)(o))->dispose))((OBJ)(o))

	 

  /* header of closure */

typedef struct sCLOSURE {
    struct sCELL header;
    OBJ mttab;			/* pointer into attached evaluation method */
    OBJ entry;			/* direct call entry */
    OBJ symbolid;		/* symbolic identification (denotation) */
    OBJ info;			/* packed word containing the rank and
				   the number of closured arguments */
} * CLOSURE;

#define data_closure(o) ((OBJ*)((CLOSURE)(o)+1))
#define sizeof_closure(r)  (sizeof_small(struct sCLOSURE) \
				+ size_data(((r)-1)*sizeof(OBJ)))


#define make_closure_info(r,m) (((WORD)(r) << 16) | (WORD)(m))
#define rank_closure_info(i)   ((WORD)(i) >> 16)
#define argc_closure_info(i)   ((WORD)(i) & 0xffff)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* basic memory management
 */


#define _free_list_size   (2*flat_offset_ssize)	/* size of free list */

#ifdef _GCCGLOBREG_
extern CELL _freeListMem[_free_list_size];	/* free list mem */
register CELL * _freeList asm(_GCCGLOBREG_);	/* global register
						   containing free list
						   pointer */
#else
extern CELL _freeList[_free_list_size];		/* free list */
#endif

extern CELL _smallAlloc(WORD);		/* small alloc */
extern BCELL _bigAlloc(WORD);		/* big alloc */
extern void _forceAllNoneFlats();	/* force free of none-flats */
extern void _forceAllBigs();		/* force free of bigs */

#ifdef _MEMSTAT_
extern unsigned _ms_allocs[flat_offset_ssize];	/* total allocs per size */
extern unsigned _ms_const_allocs[flat_offset_ssize];	
						/* those performed during 
						   initialization*/
extern unsigned _ms_hard_allocs[flat_offset_ssize];	
						/* calls to _smallAlloc 
						   or _bigAlloc */
extern unsigned _ms_reallocs[flat_offset_ssize];
						/* reallocs of none-flat
						   cells */
extern unsigned _ms_deallocs[flat_offset_ssize*2];
						/* direct deallocs per size */
extern unsigned _ms_indir_deallocs[flat_offset_ssize*2];
						/* indirect deallocs per size */
extern unsigned _ms_chunks;			/* number of chunks */
extern unsigned _ms_forces;			/* number of none-flat
						   forces */
extern unsigned _ms_big_forces;			/* number of big forces */
#endif

extern void _deAllocClos(OBJ);

#ifndef _MEMSTAT_
#define _alloc(s,r) {\
	if (r = (OBJ)_freeList[make_flat_ssize(s)])		\
	     _freeList[make_flat_ssize(s)] = _link(_header(r)); \
	else r = (OBJ)_smallAlloc(s);}
#define _deAlloc(o) {\
	int __size = _size(_header(o));\
	_setLink(_header(o),_freeList[__size]); \
	_freeList[__size] = (CELL)(o);}
#define _deAllocFlat(o) {\
	int __size = _size(_header(o)) | flat_offset_ssize;\
	_setLink(_header(o),_freeList[__size]); \
	_freeList[__size] = (CELL)(o);}
#else
#define _alloc(s,r) {\
	_ms_allocs[s]++; \
	if (r = (OBJ)_freeList[make_flat_ssize(s)])		\
	     _freeList[make_flat_ssize(s)] = _link(_header(r)); \
	else r = (OBJ)_smallAlloc(s);}
#define _deAlloc(o) {\
	int __size = _size(_header(o));\
	_ms_deallocs[__size]++; \
	_setLink(_header(o),_freeList[__size]); \
	_freeList[__size] = (CELL)(o);}
#define _deAllocFlat(o) {\
	int __size = _size(_header(o)) | flat_offset_ssize;\
	_ms_deallocs[__size]++; \
	_setLink(_header(o),_freeList[__size]); \
	_freeList[__size] = (CELL)(o);}
#endif



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * program execution model 
 */

/* argument count, argument vector and environment at program startup. */

extern int start_argc;
extern char ** start_argv;
extern char ** start_env;


/* how to execute the top-level command, setup by libraries. */

extern int (*ocs_top_exec)(OBJ);       



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * dynamic linking
 */


/* entries for dynamic linking */

extern void * ocs_dl_resolve(char *sym);
extern int ocs_dl_link(char *structure);
extern int ocs_dl_unlink(char *structure);
extern char *ocs_dl_error();


/* defining a dynamic linking method */

extern void * ocs_dl_def_method(void *(*resolve)(char *),
				int   (*link)(char *),
				int   (*unlink)(char *),
				char *(*error)(void));
		     
/* run-time linking of symbol, automatically linking its structure. */

extern void * ocs_dl_link_and_resolve(char *sym);


/* run-time linking of a closure, automatically linking its structure. */

extern int ocs_dl_closure(OBJ); 

/* old fashioned closure linking; returns NULL on succeed, otherwise
   ocs_dl_error() */

extern char *link_closure(OBJ); 


/* generic support for dl modules */

extern int ocs_dl_parse_symbol(char *sym, 
			       char *structure, int structmaxlen,
			       char *name,   int namemaxlen);
extern int ocs_dl_parse_init_entry(char *sym, 
				   char *structure, int structmaxlen);
extern int ocs_dl_make_init_entry(char *structure, 
				  char *sym, int symmaxlen);
extern int ocs_dl_find_object_file (char *structure, 
				    char *fname, int fnamemaxlen);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * allocation for handcoding
 */

#define sizeof_small(type) (_objSize(sizeof(type) - sizeof(struct sCELL)))
#define sizeof_big(type)   (_objSize(sizeof(type) - sizeof(struct sBCELL)))
#define sizeof_foreign(type)  (_objSize(sizeof(type) - sizeof(struct sFCELL)))
#define size_data(dsize) _objSize(dsize)

#define alloc_small(sz,r) {_alloc(sz,r); _mkHeader(_header(r),sz,1);}
#define alloc_small_flat(sz,r) \
	{_alloc(sz,r); _mkHeader(_header(r),make_flat_ssize(sz),1);}
#define alloc_small_byte_flat(sz,r) \
	{_alloc(sz,r); _mkHeader(_header(r),make_flat_ssize(sz),1);\
	 set_sflag(r, byte_flat_sflag); }


#define alloc_big(sz,r) {r=_alloc_big(sz,0);}
#define alloc_big_flat(sz,r) {r=_alloc_big(sz,flat_offset_ssize);}
#define alloc_big_byte_flat(sz,r) {r=_alloc_big(sz,flat_offset_ssize);\
				   set_sflag(r, byte_flat_sflag); }

extern OBJ _alloc_big(WORD,WORD);

#define alloc_foreign(sz,disp,r) {\
    _alloc(sz+1,r); \
    _mkHeader(_header(r), foreign_escape_ssize, 1);\
    ((FCELL)(r))->dispose = disp;\
}

#define dispose_foreign_mem(o,sz){\
    int __size = make_flat_ssize(sz+1);\
    _setLink(_header(o),_freeList[__size]); \
    _freeList[__size] = (CELL)(o);\
}
    
    

extern void* malloc_aux(int);
extern void free_aux(void *);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * reference counting for handcoding
 */

#define copy_some(o,n) COPY(o,n)
#define free_some(o,n) FREE(o,n)

#define copy_structured(o,n) CPPRD(o,n)
#define free_structured(o,n) FRPRD(o,n)
#define excl_structured(o,n) EXPRD(o,n)
#define decr_structured(o,n) DCPRD(o,n)
#define dispose_structured(o) DSPRD(o)
#define dispose_structured_flat(o) DSPRDF(o)

#define copy_closure(o,n) CPCLS(o,n)
#define free_closure(o,n) FRCLS(o,n)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* foreign closures for handcoding
 */

#define NUMVARMETHODTABLES 3

CODE *ocs_alloc_var_method_table
           (OBJ (*method)(OBJ, int argc, OBJ argv[]));
     /* note that the argv passed to METHOD is not reenetrant */

OBJ ocs_var_eval(OBJ Closure, int argc, OBJ argv[]); 
    /* the argv passed to this function need not to be reenetrant */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* types used by the compiler
 */

typedef int FLAG;			/* boolean flag */

typedef struct{OBJ c1; OBJ c2;} TUP2;	/* tuples */ 
typedef struct{OBJ c1; OBJ c2; OBJ c3;} TUP3;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4;} TUP4;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5;} TUP5;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6;} TUP6;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;} TUP7;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8;} TUP8;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9;} TUP9;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10;} TUP10;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c11;} TUP11;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12;} TUP12;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13;} TUP13;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;} TUP14;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; } TUP15;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; OBJ c16; } TUP16;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; OBJ c16; OBJ c17; } TUP17;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; OBJ c16; OBJ c18; } TUP18;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; OBJ c16; OBJ c18; OBJ c19; } TUP19;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; OBJ c16; OBJ c18; OBJ c19; OBJ c20; } TUP20;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; OBJ c16; OBJ c18; OBJ c19; OBJ c20; OBJ c21; } TUP21;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; OBJ c16; OBJ c18; OBJ c19; OBJ c20; OBJ c21; 
	       OBJ c22; } TUP22;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; OBJ c16; OBJ c18; OBJ c19; OBJ c20; OBJ c21; 
	       OBJ c22; OBJ c23; } TUP23;
typedef struct{OBJ c1; OBJ c2; OBJ c3; OBJ c4; OBJ c5; OBJ c6; OBJ c7;
	       OBJ c8; OBJ c9; OBJ c10; OBJ c12; OBJ c13; OBJ c14;
	       OBJ c15; OBJ c16; OBJ c18; OBJ c19; OBJ c20; OBJ c21; 
	       OBJ c22; OBJ c23; OBJ c24; } TUP24;



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* compiler macros for data types
 */

#define PRM(t,r) 	r=pack_word(t)
#define PRM1(r)		r=pack_word(0)
#define ISPRM(o)	_isVal(o)
#define TGPRM(o)	unpack_word(o)
#define ISTGPRM(o,t)	((o) == pack_word(t))

#define DMD(s,c)
#define DMD1(s,c)

#define PRD(s,t,r)	{_alloc(s+1,r);\
			 _mkHeader(_header(r),s+1,1); \
			 _data(r)[0] = pack_word(t);}

#define PRDF(s,t,r)	{_alloc(s+1,r);\
			 _mkHeader(_header(r),make_flat_ssize(s+1),1); \
			 _data(r)[0] = pack_word(t);}

#define FLD(o,i)	(_data(o)[i])

#define PRD1(s,r)	{_alloc(s,r);\
			 _mkHeader(_header(r),s,1);}
#define PRD1F(s,r)	{_alloc(s,r);\
			 _mkHeader(_header(r),make_flat_ssize(s),1);}
#define FLD1(o,i)	(_data(o)[i-1])
	
#define ISPRD(o)	_isRef(o)
#define TGPRD(o)	unpack_word(_data(o)[0])
#define ISTGPRD(o,t)	(_data(o)[0]==pack_word(t))
#define CHGTG(o,t)	{_data(o)[0]=pack_word(t);}

#define TGOBJ(o)	(ISPRM(o)?TGPRM(o):TGPRD(o))
#define ISTGOBJ(o,t)	(ISPRM(o)?ISTGPRM(o,t):ISTGPRD(o,t))

#define EXPRD(o,n)	_tstRc(_header(o),n)
#define CPPRD(o,n)	_incRc(_header(o),n)
#define FRPRD(o,n)	{if (_decTstRc(_header(o),n)) _deAlloc(o);}
#define DSPRD(o)	_deAlloc(o)
#define DSPRDF(o)	_deAllocFlat(o)
#define DCPRD(o,n)	_decRc(_header(o),n)

#define RSOME(o,r)      r=(o)
#define RPRD(o,r)       {r=(o);_size(_header(r)) &= off_flat_offset_ssize;}
#define RPRDF(o,r)      {r=(o);_size(_header(r)) |= flat_offset_ssize;}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* compiler macros for closures
 */

#define MAXRANK		24

extern CODE * _mttabs[MAXRANK];		/* addresses of mttab's by rank */
extern CODE * _mttabs_l[MAXRANK];		

#define MTH(r,m,n,mth) \
  {extern CODE _mttab_##r[]; \
   _mttab_##r[(((2*MAXRANK-m+1)*m)/2)+n-1] = (CODE)mth; \
   _mttabs[r] = _mttab_##r;}

#define LZYMTH(r,m,n,mth) \
  {extern CODE _mttab_##r##_l[]; \
   _mttab_##r##_l[(((2*MAXRANK-m+1)*m)/2)+n-1] = (CODE)mth;\
   _mttabs_l[r] = _mttab_##r##_l; }



#define CLS(r,e,x) \
   {extern CODE _mttab_##r[]; x=_cls(#e,(CODE)(e),_mttab_##r,r);}

#define LCLS(r,e,x) \
   {extern CODE _mttab_##r##_l[]; x=_cls(#e,(CODE)(e),_mttab_##r##_l,r);}

extern OBJ _cls(char *, CODE, CODE *, WORD);


#define METHOD(o,n)     \
	(( (CODE*) unpack_pointer( ((CLOSURE)(o))->mttab ) )[(n)-1])
#define ENTRY(o)        ( (CODE)unpack_pointer( ((CLOSURE)(o))->entry ) )
#define FLDCLS(o,i)     ( data_closure(o)[(i)-1] )

#define INCCLS(o,m,n){\
    WORD __info = unpack_word( ((CLOSURE)(o))->info );                  \
    ((CLOSURE)(o))->info =                                              \
        pack_word( make_closure_info( rank_closure_info(__info),        \
                                      (m) + (n) ) );   			\
    ((CLOSURE)(o))->mttab =                                             \
        (OBJ) ( (WORD) (((CLOSURE)(o))->mttab) +                        \
                    sizeof(CODE) * (((2*MAXRANK - 2*(m) - (n) + 1)*(n))/2) ); \
} 			/* pointer arithmetic with tagging ON; 
			   might violate ANSI */


/*#define EXCLS(o,n)	_tstRc(_header(o),n)*/
#define EXCLS(o)	_tstRc(_header(o),1)
#define FRCLS(o,n)	{if (_decTstRc(_header(o),n)) _deAllocClos(o);}
#define CPCLS(o,n)	_incRc(_header(o),n)
#define DSCLS(o)	_deAlloc(o)
#define DSCLSF(o)	_deAllocFlat(o)
#define DCCLS(o,n)	_decRc(_header(o),n)

#define DPCLS(o,r)	r=_dpcls(o)
extern OBJ _dpcls(OBJ);


#define LZYCLS(o,r){\
  ((CLOSURE)(o))->mttab = pack_pointer(_lmttab);	\
  ((CLOSURE)(o))->entry = r;				\
  set_sflag(o,lazy_closure_eval_sflag);			\
}

extern CODE _lmttab[];


#define EVAL1(c,a1) (* (OBJ (*)(OBJ,OBJ))METHOD(c,1))(c,a1)
#define EVAL2(c,a1,a2) (* (OBJ (*)(OBJ,OBJ,OBJ))METHOD(c,2))(c,a1,a2)
#define EVAL3(c,a1,a2,a3) (* (OBJ (*)(OBJ,OBJ,OBJ,OBJ))METHOD(c,3))(c,a1,a2,a3)
#define EVAL4(c,a1,a2,a3,a4)\
   (* (OBJ (*)(OBJ,OBJ,OBJ,OBJ,OBJ))METHOD(c,4))(c,a1,a2,a3,a4)
#define EVAL5(c,a1,a2,a3,a4,a5)\
   (* (OBJ (*)(OBJ,OBJ,OBJ,OBJ,OBJ,OBJ))METHOD(c,5))(c,a1,a2,a3,a4,a5)
#define EVAL6(c,a1,a2,a3,a4,a5,a6)\
   (* (OBJ (*)(OBJ,OBJ,OBJ,OBJ,OBJ,OBJ,OBJ))METHOD(c,6))(c,a1,a2,a3,a4,a5,a6)
#define EVAL7(c,a1,a2,a3,a4,a5,a6,a7)\
   (* (OBJ (*)(OBJ,OBJ,OBJ,OBJ,OBJ,OBJ,OBJ,OBJ))METHOD(c,7))(c,a1,a2,a3,a4,a5,a6,a7)
#define EVAL8(c,a1,a2,a3,a4,a5,a6,a7,a8)\
   (* (OBJ (*)(OBJ,OBJ,OBJ,OBJ,OBJ,OBJ,OBJ,OBJ,OBJ))METHOD(c,8))(c,a1,a2,a3,a4,a5,a6,a7,a8)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* compiler macros for arbitrary objects
 */

#define FREE(o,n) {if(_isRef(o) && _decTstRc(_header(o),n)) _deAlloc(o);}
#define COPY(o,n) {if(_isRef(o)) _incRc(_header(o),n);}

#define NIL 	pack_word(0)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* compiler macros for debugging 
 */

extern void ocs_halt(char*), 
            ocs_exit(int),
            ocs_trace_enter(char*), ocs_trace_exit (char*),
	    ocs_trace_msg(char *);

#define HLT(m) ocs_halt(m)
#define ENTER(m) ocs_trace_enter(m)
#define EXIT(m) ocs_trace_exit(m)
#define TRACE(m) ocs_trace_msg(m)

extern void (*ocs_halt_def_method(void (*)(char *)))(char *);
extern void (*ocs_exit_def_method(void (*)(int)))(int);
extern void ocs_trace_def_method(void (*enter)(char *), 
				 void (*exit)(char *),
				 void (*msg)(char *));


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * composing function names for handcoding
 */

#define MAC(orig,name) A##orig##_A##name
#define MACs(orig,name) A##orig##_S##name
#define ENT(orig,name) _A##orig##_A##name
#define ENTs(orig,name) _A##orig##_S##name
#define VAR(orig,name) __A##orig##_A##name
#define VARs(orig,name) __A##orig##_S##name
#define AUX(orig,name) orig##_##name

#define INIT(orig) init_A##orig


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* tuples from Ac unit BUILTIN
 */

#define	__ABUILTIN_Atup0 pack_word(0)

  /* aliases */
#define __ABUILTIN_20 __ABUILTIN_Atup0


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* boolean type from Ac unit BUILTIN
 */

  /* aliases */
#define __ABUILTIN_Afalse __ABUILTIN_3 
#define __ABUILTIN_Atrue __ABUILTIN_5

#define ABUILTIN_4 ABUILTIN_Afalse_
#define __ABUILTIN_Afalse_ __ABUILTIN_4 
#define _ABUILTIN_Afalse_ _ABUILTIN_4 

#define ABUILTIN_6 ABUILTIN_Atrue_ 
#define __ABUILTIN_Atrue_ __ABUILTIN_6
#define _ABUILTIN_Atrue_ _ABUILTIN_6

#define ABUILTIN_7 ABUILTIN_St
#define __ABUILTIN_St __ABUILTIN_7 
#define _ABUILTIN_St _ABUILTIN_7 

#define ABUILTIN_8 ABUILTIN_Aor
#define __ABUILTIN_Aor __ABUILTIN_8 
#define _ABUILTIN_Aor _ABUILTIN_8 

#define ABUILTIN_9 ABUILTIN_Aand
#define __ABUILTIN_Aand __ABUILTIN_9 
#define _ABUILTIN_Aand _ABUILTIN_9 

#define ABUILTIN_10 ABUILTIN_Se
#define __ABUILTIN_Se __ABUILTIN_10 
#define _ABUILTIN_Se _ABUILTIN_10 

#define ABUILTIN_11 ABUILTIN_SSe
#define __ABUILTIN_SSe __ABUILTIN_11 
#define _ABUILTIN_SSe _ABUILTIN_11 


  /* constants */
#define __ABUILTIN_3	pack_word(0)
#define __ABUILTIN_5	pack_word(1)

  /* macros */
#define ABUILTIN_Afalse_(x,r) {\
  r = x==__ABUILTIN_Afalse ? __ABUILTIN_Atrue : __ABUILTIN_Afalse;\
}
#define ABUILTIN_Atrue_(x,r) {\
  r = x==__ABUILTIN_Atrue ? __ABUILTIN_Atrue : __ABUILTIN_Afalse;\
}
#define ABUILTIN_Aand(x1,x2,r) {\
  r = x1==__ABUILTIN_Atrue ? x2 : __ABUILTIN_Afalse;\
}
#define ABUILTIN_Aor(x1,x2,r) {\
  r = x1==__ABUILTIN_Afalse ? x2 : __ABUILTIN_Atrue;\
}
#define ABUILTIN_St(x,r) {\
  r = x==__ABUILTIN_Afalse ? __ABUILTIN_Atrue : __ABUILTIN_Afalse;\
}
#define ABUILTIN_Se(x1,x2,r) {\
  r = x1==x2 ? __ABUILTIN_Atrue : __ABUILTIN_Afalse;\
}
#define ABUILTIN_SSe(x1,x2,r) {\
  r = x1!=x2 ? __ABUILTIN_Atrue : __ABUILTIN_Afalse;\
}



  /* hand-coding */
#define pack_bool(x) ((x) ? __ABUILTIN_Atrue : __ABUILTIN_Afalse)
#define unpack_bool(x) ((x) == __ABUILTIN_Atrue)

#define pack_clean_bool(x) pack_word(x)	/* use only if x is exactly 0 or 1 */

  /* closure variables */
extern OBJ __ABUILTIN_Atrue_,__ABUILTIN_Afalse_,__ABUILTIN_Aand,
	   __ABUILTIN_Aor,__ABUILTIN_St,__ABUILTIN_Se,__ABUILTIN_SSe;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* denotation type from Ac unit BUILTIN 
 */

  /* aliases */
#define ABUILTIN_1 ABUILTIN_Atl
#define __ABUILTIN_Atl __ABUILTIN_1 
#define _ABUILTIN_Atl _ABUILTIN_1 

#define ABUILTIN_2 ABUILTIN_SlS
#define __ABUILTIN_SlS __ABUILTIN_2 
#define _ABUILTIN_SlS _ABUILTIN_2 

  /* procedure entrys */

extern OBJ _ABUILTIN_Atl(OBJ),_ABUILTIN_SlS(OBJ,OBJ);

  /* macros */
#define ABUILTIN_Atl(x,r) r=_ABUILTIN_Atl(x)
#define ABUILTIN_SlS(x1,x2,r) r=_ABUILTIN_SlS(x1,x2)


  /* closure variables */
extern OBJ __ABUILTIN_Atl,__ABUILTIN_SlS;


  /* hand-coding, representation */

typedef struct sDENOTATION {
    struct sBCELL big;
    OBJ leng;			/* packed length */
    /* ... data ... */		/* data */
} * DENOTATION;


#define data_denotation(o) ((unsigned char*)((DENOTATION)(o)+1))
#define leng_denotation(o) unpack_word(((DENOTATION)(o))->leng)
#define free_denotation(o,n) free_structured(o,n)
#define copy_denotation(o,n) copy_structured(o,n)
#define decr_denotation(o,n) decr_structured(o,n)
#define excl_denotation(o,n) excl_structured(o,n)

extern OBJ make_denotation(char *);
extern OBJ alloc_denotation(int);

#define CHARBUFSIZE 1024
extern char charbuf[CHARBUFSIZE];

extern int get_denotation(OBJ,char *,int);
    /* returns 1 if denotation fits in buffer,
       0 (and truncated denotation) if not
    */


  /* compiler's macro to create denotations */
#define DEN(s,r) r=make_denotation(s)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* function ABORT from Ac unit BUILTIN
 */

#define ABUILTIN_12 ABUILTIN_AABORT
#define __ABUILTIN_AABORT __ABUILTIN_12 
#define _ABUILTIN_AABORT _ABUILTIN_12 

#define ABUILTIN_AABORT(x,r) _ABUILTIN_AABORT(x)


extern OBJ __ABUILTIN_AABORT;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
/* initialization
 */

extern void init_ABUILTIN();
extern OBJ soname_ABUILTIN;

