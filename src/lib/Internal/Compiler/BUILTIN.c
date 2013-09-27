/* subject: Ac unit "BUILTIN" -- provides also all compiler macros
 * author:  wg 7-92
 * version: $Id$
 */

/* FIXME: stub for strdup() */

#include "unixconfig.h"

#include "BUILTIN.h"

/*
#define DPRINT(s) fprintf(stderr,s) 
*/
#define DPRINT(s) 
/*
#define ASSERT(c,s) {if (!(c)) HLT(s); }
*/
#define ASSERT(c,s) 

WORD max_word, bits_per_word;
SWORD min_sword,max_sword;


int (*ocs_top_exec)(OBJ);       
int start_argc;
char ** start_argv;
char ** start_env;

/* =========================================================================
 * memory management
 */

#ifdef _GCCGLOBREG_
CELL _freeListMem[_free_list_size] = {NULL};
#else
CELL _freeList[_free_list_size] = {NULL};
#endif

#ifdef _MEMSTAT_
unsigned _ms_allocs[flat_offset_ssize];     
unsigned _ms_const_allocs[flat_offset_ssize];
unsigned _ms_reallocs[flat_offset_ssize];     
unsigned _ms_hard_allocs[flat_offset_ssize];
unsigned _ms_deallocs[flat_offset_ssize*2];  
unsigned _ms_indir_deallocs[flat_offset_ssize*2];  
unsigned _ms_chunks;                
unsigned _ms_forces;
unsigned _ms_big_forces;
#endif


#ifndef nextChunkSize
#define nextChunkSize		2048
#endif
#ifndef maxChunksUntilForce
#define maxChunksUntilForce	1
#endif
#ifndef bigWrapSize
#define bigWrapSize		1
#endif

/* padding: sun4 f.i. needs padding to 8-byte-boundaries (float) 
*/

#ifdef _ALIGN_DOUBLE_
#define _paddedSize(s)       (((s)+1)&(~1))  /* ??? */
#else
#define _paddedSize(s)       (s)
#endif

static OBJ * chunk;
static WORD chunkSize = 0;
static WORD chunksSinceForce = 0;

#define bigWrap(s)		(((s)+bigWrapSize-1)/bigWrapSize)




CELL _smallAlloc(WORD s) {	/* allocate small cell */
    register CELL p;	
    WORD totalSz;

    #ifdef _MEMSTAT_
	_ms_hard_allocs[s]++;
    #endif

    /* try to realloc none-flat cell */
    if ( (p = _freeList[s]) ) {
	/* none-flat cell avail: unlink, free components, and return */
        #ifdef _MEMSTAT_
	    _ms_reallocs[s]++;
        #endif

	_freeList[s] = _getLink(_header(p));
        { register OBJ *l = _data(p); register OBJ *r = l+s;
	  while (l < r) { 
	    if (_isRef(*l)){
		if (_decTstRc(_header(*l),1)){
		    register WORD subs = _size(_header(*l));
		    #ifdef _MEMSTAT_
        	    	_ms_indir_deallocs[subs]++;
        	    #endif
		    /* link to appropriate list */
		    _setLink(_header(*l),_freeList[subs]);
		    _freeList[subs] = (CELL)*l;
		}
	    }
	    l++;
	  }
	}
	return p;
    }

    /* try to allocate from chunk */
    totalSz = _paddedSize(_totalSize(s));
    if (chunkSize >= totalSz){
	p = (CELL)chunk; chunk += totalSz; chunkSize -= totalSz;
	return p;
    }

    /* if several chunks have been allocated since last forced free of all 
       none-flats force them now */
    if (chunksSinceForce >= maxChunksUntilForce){
        /* free all none-flat cells */
	_forceAllNoneFlats(); 
	if ( (p = _freeList[make_flat_ssize(s)]) ){
	    _freeList[make_flat_ssize(s)] = _getLink(_header(p));
	    return p;
	}
    }

    /* allocate new chunk */
    if (chunkSize > _totalSize(0)){
	/* free remaining chunk */
	WORD cellSize = chunkSize - _totalSize(0);
	_setLink(_header(chunk),_freeList[make_flat_ssize(cellSize)]);
	_freeList[make_flat_ssize(cellSize)] = (CELL)chunk;
    }
    #ifdef _MEMSTAT_
        _ms_chunks++;
    #endif

    _forceAllBigs();
    if (!(chunk = (OBJ*)malloc(nextChunkSize*sizeof(OBJ))))
	HLT("out of memory");
    chunkSize = nextChunkSize - totalSz;
    chunksSinceForce++;
    p = (CELL)chunk; chunk += totalSz;
    return p;
}

void _forceAllNoneFlats(){ /* force free of all none-flat cells */
    /* incremently force free of none-flats: start with s=1, reset s if
       smaller subcell is visited.
       NB. Stack grows too much for recursive algorithms, so we use
	   the slow incremental way. 
    */
    register CELL p;
    register WORD s = 1;

    #ifdef _MEMSTAT_
        _ms_forces++;
    #endif

    while (s <= big_escape_ssize){
	WORD nexts = s+1;
	while ((p = _freeList[s])){
	    _freeList[s] = _getLink(_header(p));	/* unlink ... */
	    _setLink(_header(p),_freeList[make_flat_ssize(s)]);
	    _freeList[make_flat_ssize(s)] = p;	/* ... link to flat list */

	    { register OBJ * l, * r;
	      if (s < big_escape_ssize){
	  	l = _data(p); r = l+s;
	      } else {
	  	/* big cell */
	  	l = _bdata(p); r = l + unpack_word(((BCELL)p)->size);
	      }
	      while (l < r){
		if (_isRef(*l)){
		    if (_decTstRc(_header(*l),1)){
			WORD subs = _size(_header(*l));
		        /* check if we have to reset size */
		        if (subs < s && subs < nexts){
			    nexts = subs;
			}
    			#ifdef _MEMSTAT_
        		    _ms_indir_deallocs[subs]++;
        		#endif
			/* link to appropriate list */
			_setLink(_header(*l),_freeList[subs]);
			_freeList[subs] = (CELL)*l;
		    }
		}
		l++;
	      }
	    }
	}
	s = nexts;
    }
    
    /* also force free of foreign cells here */
    while ((p = _freeList[foreign_escape_ssize])){
	_freeList[foreign_escape_ssize] = _getLink(_header(p));	/* unlink */
	foreign_dispose((OBJ)p);
    }
	
    chunksSinceForce = 0;
}


BCELL _bigAlloc(WORD s){	/* allocate big cell */
    register BCELL p; 
    WORD ws;

    #ifdef _MEMSTAT_
        _ms_allocs[big_escape_ssize]++;
        _ms_hard_allocs[big_escape_ssize]++;
    #endif

    ws = bigWrap(s);

    /* search for matching flat-big, on the fly freeing none-matching
       flat-bigs */
    while ((p = (BCELL)_freeList[make_flat_ssize(big_escape_ssize)])){
	_freeList[make_flat_ssize(big_escape_ssize)] = _getLink(_header(p));
	if (bigWrap(unpack_word(p->size)) == ws){
	    return p;
	}
	free((void*)p);
    }
	
    /* search for matching none-flat-big, on the fly freeing none-matching
       none-flat-bigs */
    while ((p = (BCELL)_freeList[big_escape_ssize])){
	register OBJ *l, *r;
	_freeList[big_escape_ssize] = _getLink(_header(p));
	l = _bdata(p); r = l + unpack_word(p->size);
	while (l < r) { 
	    if (_isRef(*l)){
		if (_decTstRc(_header(*l),1)){
		    register WORD subs = _size(_header(*l));
		    #ifdef _MEMSTAT_
        	    	_ms_indir_deallocs[subs]++;
        	    #endif
		    /* link to appropriate list */
		    _setLink(_header(*l),_freeList[subs]);
		    _freeList[subs] = (CELL)*l;
		}
	    }
	    l++;
	}
	if (bigWrap(unpack_word(p->size)) == ws){
	    return p;
	}
	free((void*)p);
    }
	
    /* allocate from system */
    if (!(p = (BCELL)malloc(sizeof(OBJ)*_totalBigSize(ws*bigWrapSize))))
	HLT("out of memory");
    return p;
}

void _forceAllBigs(){	/* force free of all big cells */
    register BCELL p;

    #ifdef _MEMSTAT_
        _ms_big_forces++;
    #endif
    /* first free all flat bigs */
    while ((p = (BCELL)_freeList[make_flat_ssize(big_escape_ssize)])){
	_freeList[make_flat_ssize(big_escape_ssize)] = _getLink(_header(p));
	free((void*)p);
    }

    /* now free all none-flat bigs */
    while ((p = (BCELL)_freeList[big_escape_ssize])){
	register OBJ *l, *r;
	_freeList[big_escape_ssize] = _getLink(_header(p));
	l = _bdata(p); r = l + unpack_word(p->size);
	while (l < r) { 
	    if (_isRef(*l)){
		if (_decTstRc(_header(*l),1)){
		    register WORD subs = _size(_header(*l));
		    #ifdef _MEMSTAT_
        	    	_ms_indir_deallocs[subs]++;
        	    #endif
		    /* link to appropriate list */
		    _setLink(_header(*l),_freeList[subs]);
		    _freeList[subs] = (CELL)*l;
		}
	    }
	    l++;
	}
	free((void*)p);
    }
}

/* =========================================================================
 * memory statistics
 */
#ifdef _MEMSTAT_

void _endInitStat(){
    WORD i;
    _forceAllNoneFlats(); 
    for (i = 0; i < flat_offset_ssize; i++){
	_ms_const_allocs[i] = 
		_ms_allocs[i] -  
		         (_ms_deallocs[i] +
		         _ms_deallocs[i+flat_offset_ssize] +
			 _ms_indir_deallocs[i] + 
			 _ms_indir_deallocs[i+flat_offset_ssize]);
    }
}
    

void _printStat(){
    long allocs = 0, const_allocs = 0, hard_allocs = 0,
         reallocs = 0, deallocs = 0, indir_deallocs = 0,
         flat_deallocs = 0, flat_indir_deallocs = 0,
         zombies = 0;
    long t1,t2;
    WORD i;

    _forceAllNoneFlats();

    fprintf(stderr,"\n\n");
    fprintf(stderr,
"Sz  total   hard    const   realloc d-free  d-ffree i-free  i-ffree zombies\n");
    fprintf(stderr,
"- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
    for (i = 0;  i < flat_offset_ssize; i++){
         if (_ms_allocs[i] != 0 || _ms_deallocs[i] != 0 || 
				   _ms_deallocs[i+flat_offset_ssize] != 0 ||
				   _ms_indir_deallocs[i] != 0 ||
				   _ms_indir_deallocs[i+flat_offset_ssize] != 0 ||
				   _ms_hard_allocs[i] != 0){

	    allocs += _ms_allocs[i];
	    hard_allocs += _ms_hard_allocs[i];
	    const_allocs += _ms_const_allocs[i];
	    reallocs += _ms_reallocs[i];
	    deallocs += _ms_deallocs[i];
	    flat_deallocs += _ms_deallocs[i+flat_offset_ssize];
	    indir_deallocs += _ms_indir_deallocs[i];
	    flat_indir_deallocs += _ms_indir_deallocs[i+flat_offset_ssize];

	    if (i < big_escape_ssize){
		t1 = _ms_allocs[i] - 
			(_ms_deallocs[i] + 
		         _ms_deallocs[i+flat_offset_ssize] +
			 _ms_indir_deallocs[i] + 
			 _ms_indir_deallocs[i+flat_offset_ssize] +
			 _ms_const_allocs[i]);
	    } else {
		t1 = _ms_hard_allocs[i] - 
			(_ms_deallocs[i] + 
		         _ms_deallocs[i+flat_offset_ssize] +
			 _ms_indir_deallocs[i] + 
			 _ms_indir_deallocs[i+flat_offset_ssize] +
			 _ms_const_allocs[i]);
	    }
	    zombies += t1;

            fprintf(stderr,"%-4d%-8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d\n",
                i,
		_ms_allocs[i],
		_ms_hard_allocs[i],
		_ms_const_allocs[i],
		_ms_reallocs[i],
		_ms_deallocs[i],
		_ms_deallocs[i+flat_offset_ssize],
		_ms_indir_deallocs[i],
		_ms_indir_deallocs[i+flat_offset_ssize],
		t1);
	}
    }
    fprintf(stderr,
"- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
    fprintf(stderr, "TOT %-8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d%-8d\n\n",
		allocs,
		hard_allocs,
		const_allocs,
		reallocs,
		deallocs,
		flat_deallocs,
		indir_deallocs,
		flat_indir_deallocs,
		zombies);

#define percent(base,n) (100 * (n))/(base)

    t1 = deallocs + flat_deallocs + indir_deallocs + flat_indir_deallocs;
    fprintf(stderr,
"%d%% soft alloc, %d%% lazy alloc, %d%% flat free, %d%% indir free\n",
		   percent(allocs,allocs-hard_allocs),
		   percent(allocs,reallocs),
		   percent(t1,flat_deallocs + flat_indir_deallocs),
		   percent(t1,indir_deallocs + flat_indir_deallocs));

    fprintf(stderr,"%d forces, %d * %d words = %dkB grabed\n",
		   _ms_forces,
		   _ms_chunks,
		   nextChunkSize, 
		   (_ms_chunks*nextChunkSize*sizeof(OBJ)) / 1024);


    
}


#endif


/* =========================================================================
 * closures
 */

CODE * _mttabs[MAXRANK];
CODE * _mttabs_l[MAXRANK];


OBJ _cls(char * sym, CODE entry, CODE mttab[], WORD rank){
    CLOSURE c; OBJ res; WORD i;

    alloc_small(sizeof_closure(rank),res); 
    set_sflag(res,closure_sflag);
    
    c = (CLOSURE)res;
    c->entry = pack_pointer(entry); 
    c->mttab = pack_pointer(mttab);
    c->symbolid = make_denotation(sym);
    c->info = pack_word(make_closure_info(rank,0));
    for (i = rank - 1; i > 0; i--)
        FLDCLS(c,i) = NIL;
    return res;
}

OBJ _dpcls(OBJ clos){
    OBJ res; CLOSURE old = (CLOSURE)clos, new;
    WORD info = unpack_word(old->info);
    WORD rank = rank_closure_info(info), argc = argc_closure_info(info), i;

    alloc_small(sizeof_closure(rank),res); 
    set_sflag(res,closure_sflag);
    
    new = (CLOSURE)res;
    new->entry = old->entry; copy_some(new->entry,1);
    new->mttab = old->mttab; 
    new->info = old->info;
    new->symbolid = old->symbolid; copy_denotation(new->symbolid,1);
    for (i = 1; i <= argc; i++){
        FLDCLS(new,i) = FLDCLS(old,i); copy_some(FLDCLS(new,i),1);
    }
    for (; i <= rank-1; i++){
        FLDCLS(new,i) = NIL;
    }
    return res;
}

void _deAllocClos(OBJ clos){
    WORD info = unpack_word(((CLOSURE)clos)->info);
    WORD argc = argc_closure_info(info);
    WORD i;

    for (i = 1; i <= argc; i++){
	free_some(FLDCLS(clos,i), 1);
    }
    free_denotation(((CLOSURE)clos)->symbolid, 1);
    _deAllocFlat(clos);
}

static OBJ lazymethod(OBJ cls, OBJ unit) {
    OBJ r = ((CLOSURE)cls)->entry;
    if (EXCLS(cls)){
	free_denotation( ((CLOSURE)cls)->symbolid, 1 );
	DSCLSF(cls);
    } else {
	COPY(r,1);
	DCCLS(cls,1);
    }
    return r;
}

CODE _lmttab[] = {(CODE)lazymethod};


/* ============================================================ 
stopping time
*/
#if 0
#include <sys/time.h>
static struct timeval start;
static struct timezone timezoneFoo;

static void startTimer(void) {
  gettimeofday(&start, &timezoneFoo);
}

static double stopTimer(void) {
  struct timeval ende;

  gettimeofday(&ende, &timezoneFoo);
  return (double) ende.tv_sec + ende.tv_usec / 1000000.0 -
    (start.tv_sec + start.tv_usec / 1000000.0);
}

#define TIMER(stm) {stm;}
#else
#define TIMER(stm)
#endif


/* =========================================================================
 * dynamic linking 
 */

int ocs_dl_debug;

#define DLDDEBUG(stm) {if (ocs_dl_debug) {stm;}}



/* call entries */

static void * (*dl_resolve)(char *);
static int    (*dl_link)(char *);
static int    (*dl_unlink)(char *);
static char * (*dl_error)(void);

static char * object_map_suffix = ".tso";
static char * object_file_suffix = ".o";
static char * sh_object_file_suffix = ".so";


static void * dl_default_resolve(char *sym){
    return NULL;
}

static int dl_default_link(char *structure){
    return 0;
}

static int dl_default_unlink(char *structure){
    return 0;
}

static char * dl_default_error(){
    return "dynamic linking not supported in this configuration";
}

		     
void * ocs_dl_resolve(char *sym){
    return (*dl_resolve)(sym);
}

int ocs_dl_link(char *structure){
    return (*dl_link)(structure);
}

char *ocs_dl_error(){
    return (*dl_error)();
}

void *ocs_dl_link_and_resolve(char *sym){
    char name[128];
    char structure[128];

    ocs_dl_parse_symbol(sym,
			structure, sizeof(structure)-1,
			name, sizeof(name)-1);

    if (ocs_dl_link(structure)) {
	return ocs_dl_resolve(sym);
    } else 
	return NULL;
}

    


void * ocs_dl_def_method(void *(*resolve)(char *),
                  	 int   (*link)(char *),
			 int   (*unlink)(char *),
			 char *(*error)(void)){
    dl_resolve = resolve;
    dl_link = link;
    dl_unlink = unlink;
    dl_error = error;
    return 0; //fulfill interface, dont want to break it
}

/* parsing symbols */

extern int ocs_dl_parse_symbol(char *sym, 
			       char *structure, int structmaxlen,
			       char *name,   int namemaxlen){
    /* FIXME: convert symbolic names to their OPAL representation? */
    char *cp = sym;
    while (*cp == '_') cp++;
    if (*cp == 'A') {
	int i;
	cp++;
	i = 0;
	while (*cp != '_'){
	    if (i >= structmaxlen || *cp == 0) {
		structure[i] = 0;
		return 0;
	    }
	    structure[i++] = *cp++;
	}
	structure[i] = 0;
	while (*cp == '_') cp++;
	i = 0;
	while (*cp){
	    if (i >= namemaxlen) {
		name[i] = 0;
		return 0;
	    }
	    name[i++] = *cp++;
	}
	name[i] = 0;
	return name[0] == 'A' || name[0] == 'S' || 
	         (name[0] >= '0' &&  name[0] <= '9');
    } else
	return 0;
}

extern int ocs_dl_parse_init_entry(char *sym, 
				   char *structure, int structmaxlen){
#ifdef HAVE_LEADING_UNDERSCORE
    char *init_prefix = "_init_A";
    int prefix_len = 7;
#else
    char* init_prefix = "init_A";
    int prefix_len = 6;
#endif

  /* Fix for verbose nm -u (2004/11/05 bt) */
    /* while (*sym == ' ') sym++; */
    while (*sym) {
      if (*sym == ' ' || *sym == '\t') sym++ ;
      else if (sym[0] == 'U' && sym[1] == ' ') sym += 2 ;
      else break ;
    }

    if (strncmp(sym, init_prefix,  prefix_len) == 0){
	strncpy(structure, sym + prefix_len, structmaxlen-1);
	return 1;
    } else {
	return 0;
    }
}
	
extern int ocs_dl_make_init_entry(char *structure, 
				  char *sym, int symmaxlen){
    strncpy(sym, "init_A", symmaxlen-1);
    strncat(sym, structure, symmaxlen-7);
    return 0;
}


/* locating object files */

typedef struct sOBJMAP {
    struct sOBJMAP * next;
    char * structure;	/* name of structure */
    char * filename;	/* name of object structure is found in */
    void * dl_handle;   /* handle for dynamic loading functions */
} * OBJMAP;

/* literature says prime numbers not too near a power of two are best */
#define OBJMAP_HASHSIZE 199  

/* structure names often have common prefixes; the hash function 
   therefore looks at the whole string */
#define OBJMAP_HASHCODE(stru) ocs_calc_object_hash(stru)

static int ocs_calc_object_hash(char * stru) {
  char *s = stru;
  unsigned int h = 0, f = 1;

  while(*s){
    h = (h + (int) *s * f) % OBJMAP_HASHSIZE;
    s++;
    if (*s) s++;
    f *= 3;
  };
  return h;
}
/* old has method - fast but prone to collisions 
#define OBJMAP_HASHCODE(struct) \
   (((struct[0]) + (struct[1])) % OBJMAP_HASHSIZE)
*/

static OBJMAP object_map[OBJMAP_HASHSIZE];

static int object_map_loaded = 0;

static char *lookup_object_map(char * structure);
static void insert_object_map(char * structure, char * filename);
static void load_object_map(void);
static void load_object_map_dir(char *); 
static void load_object_map_file(char *, char *);
static void load_object_map_direct(char *, char *);
static int check_object(char *dirname, char *structure,
			char *fname, int fnamemaxlen);
static int check_sh_object(char *dirname, char *structure,
			char *fname, int fnamemaxlen);
static int check_object_in_path(char *path, char *structure,
				char *fname, int fnamemaxlen);

extern int ocs_dl_find_object_file (char *structure, 
				    char *fname, int fnamemaxlen){
    OBJMAP p;
    char *path, *buf, *dirname;

    if (!object_map_loaded) 
	load_object_map();

    buf = lookup_object_map(structure);
    if (buf != NULL){
	strncpy(fname, buf, fnamemaxlen);
	return 1;
    }

    /* if we have no mapping stored, we lookup for .o files 
       first in OCS_DL_PATH, then in LD_LIBRARY_PATH, and finally in 
       ./OCS and . 

       No, we dont, but just lookup in OCS_DL_PATH. FIXME: rethink. */


    if (check_object_in_path("OCS_DL_PATH", structure, fname, fnamemaxlen))
	return 1;

    /*
    if (check_object_in_path("LD_LIBRARY_PATH", structure, fname, fnamemaxlen))
	return 1;

    if (check_object("./OCS", structure, fname, fnamemaxlen))
	return 1;

    if (check_object(".", structure, fname, fnamemaxlen))
	return 1;
	*/

    return 0;
}

static char *lookup_object_map(char *structure){
    OBJMAP p;
    for (p = object_map[OBJMAP_HASHCODE(structure)]; p != NULL; p = p->next){
	if (strcmp(p->structure, structure) == 0){
	    return p->filename;
	}
    }
    return NULL;
}

static char *lookup_set_object_map(char *structure, char *fname){
    OBJMAP p;
    for (p = object_map[OBJMAP_HASHCODE(structure)]; p != NULL; p = p->next){
	if (strcmp(p->structure, structure) == 0){
	    free(p->filename);
	    p->filename = fname;
	    return p->filename;
	}
    }
    return NULL;
}

static void *lookup_dl_map(char *structure){
    OBJMAP p;
    for (p = object_map[OBJMAP_HASHCODE(structure)]; p != NULL; p = p->next){
	if (strcmp(p->structure, structure) == 0){
	    return p->dl_handle;
	}
    }
    return NULL;
}

static void *set_dl_map(char *structure, void * handle){
    OBJMAP p;
    for (p = object_map[OBJMAP_HASHCODE(structure)]; p != NULL; p = p->next){
	if (strcmp(p->structure, structure) == 0){
	    p->dl_handle = handle;
	    return handle;
	}
    }
    return NULL;
}

static void insert_object_map(char *structure, char *filename){
    int h = OBJMAP_HASHCODE(structure);
    OBJMAP p = (OBJMAP)malloc_aux(sizeof(struct sOBJMAP));

    DLDDEBUG(fprintf(stderr,"mapping %s to %s\n",
		     structure,
		     filename));
    p->structure = structure;
    p->filename = filename;
    p->dl_handle = NULL;

    p->next = object_map[h];
    object_map[h] = p;
}


static int check_object_in_path(char *var, char *structure,
				char *fname, int fnamemaxlen){
    char *path, *dirname, *buf;
    path = getenv(var);
    if (path != NULL){
	buf = strdup(path);
	dirname = strtok(buf,":");
	while (dirname != NULL){
	    if (check_sh_object(dirname, structure, fname, fnamemaxlen)) 
		return 1;
	    if (check_object(dirname, structure, fname, fnamemaxlen)) 
		return 1;
	    dirname = strtok(NULL,":");
	}
	free(buf);
	return 0;
    } else {
	return 0;
    }
}
    
static int check_object(char *dirname, char *structure,
			char *fname, int fnamemaxlen){
    struct stat statbuf;
    strncpy(fname, dirname, fnamemaxlen-4);
    strcat(fname, "/");
    strncat(fname, structure, fnamemaxlen-4-strlen(dirname));
    strcat(fname, object_file_suffix);
    return stat(fname, &statbuf) == 0;
}

static int check_sh_object(char *dirname, char *structure,
			char *fname, int fnamemaxlen){
    struct stat statbuf;
    strncpy(fname, dirname, fnamemaxlen-4);
    strcat(fname, "/");
    strncat(fname, structure, fnamemaxlen-4-strlen(dirname));
    strcat(fname, sh_object_file_suffix);
    return stat(fname, &statbuf) == 0;
}


#include <sys/time.h>
static void load_object_map(){
    /* load object mapping */
    char *path, *buf, *dirname;
    int i;

    TIMER(startTimer());
    for (i = 0; i < OBJMAP_HASHSIZE; i++){
    	object_map[i] = NULL;
    }
    
    path = getenv("OCS_DL_PATH");
    if (path != NULL){
	buf = strdup(path);
	dirname = strtok(buf,":");

	while (dirname != NULL){
	    DLDDEBUG(fprintf(stderr,"looking up %s\n",dirname));
	    load_object_map_dir(dirname);
	    dirname = strtok(NULL,":");
	}

	free(buf);
    }
    object_map_loaded = 1;
    TIMER(fprintf(stderr, "load_object_map %f sec\n", stopTimer()));	    
}

static void load_object_map_dir(char * dirname){
    DIR * dir = opendir(dirname);
    if (dir != NULL){
	struct dirent * ent;
	while ( (ent = readdir(dir)) != NULL ){
	    char * suffix;
	    suffix = strrchr(ent->d_name,'.');
	    if (suffix != NULL){
		if (strcmp(suffix, object_map_suffix) == 0){
		    load_object_map_file(dirname,ent->d_name);
		} else if (strcmp(suffix, sh_object_file_suffix) == 0){
		    load_object_map_direct(dirname,ent->d_name);
		} else if (strcmp(suffix, object_file_suffix) == 0){
		    load_object_map_direct(dirname,ent->d_name);
		}
	    }
	}
	closedir(dir);
    }
}

static void load_object_map_file(char * dirname, char * fname){
    FILE *file;
    char buf[512];
    
    strncpy(buf, dirname, sizeof(buf)-3);
    strcat(buf, "/");
    strncat(buf, fname, sizeof(buf)-3-strlen(dirname));

    file = fopen(buf,"r");

    if (file != NULL){

        DLDDEBUG(fprintf(stderr,"loading %s\n",buf));
	while ( fgets(buf,sizeof(buf)-1,file) != NULL ){
	    int i, j, k, l;
	    if (buf[0]) {
	        buf[strlen(buf)-1] = 0;
	    }
	    i = strspn(buf, " \t");
	    j = strcspn(buf+i, " \t");
	    k = strspn(buf+i+j, " \t");
	    l = strcspn(buf+i+j+k, " \t");
	    if (j > 0 && l > 0){
		buf[i+j] = 0;
		buf[i+j+k+l] = 0;
		insert_object_map(strdup(buf+i), strdup(buf+i+j+k));
	    }
	}
	fclose(file);
    } 
}

static void load_object_map_direct(char *dirname, char *fname){
    char structure[512];
    char buf[512];
    char *s, *bufdup;
    
    s = strrchr(fname, '.');
    if (s != NULL){
	int i = s-fname;
	i = i > sizeof(structure)-1 ? sizeof(structure)-1 : i;
	strncpy(structure, fname, i);
	structure[i] = 0;
	strcpy(buf, dirname);
	strcat(buf, "/");
	strcat(buf,fname);
	bufdup = strdup(buf);
	if (lookup_set_object_map(structure, bufdup) == NULL){
	    insert_object_map(strdup(structure), bufdup);
	}
    }
}
    

/* closure linking */

int ocs_dl_closure(OBJ Clos){
    CLOSURE clos = (CLOSURE)Clos;
    WORD info = unpack_word(clos->info);
    CODE fun;

    fun = ocs_dl_link_and_resolve((char*)data_denotation(clos->symbolid));
    if (!fun) {
	return 0;
    } else {
	/* setup closure fields */
	clos->entry = pack_pointer(fun);
	{ WORD r = rank_closure_info(info), n = argc_closure_info(info);
	  clos->mttab = pack_pointer(_mttabs[r] + ((2*MAXRANK-n+1)*n)/2);
	  /* FIXME: this ignores unevaluated lazy closures, laziness is lost. 
	     we need an additional sflag for them. */
	}
	return 1;
    }
}

char * link_closure(OBJ Clos){
    if (ocs_dl_closure(Clos)){
	return NULL;
    } else {
	return ocs_dl_error();
    }
}


/* default linking method based on dlopen. */

#ifdef HAVE_DLOPEN

#include <dlfcn.h>
/* FIXME: put include to unixconfig.h */

static void * dlopen_handle_main = NULL;
static int handle_counter = 0;
static char dlopen_error_buf[256] = {0};

static void dl_lazy_open(){
    if (dlopen_handle_main == NULL){
	dlopen_handle_main = dlopen(NULL, RTLD_NOW|RTLD_GLOBAL);
    }
}

static void * dl_dlopen_resolve(char * sym){
    void *res = NULL;
    int i = 0;
    void *h; 
    char stru[128]; char fun[128];
    void (*init)() = NULL;

    dl_lazy_open();
    h = dlopen_handle_main;
    res = dlsym(dlopen_handle_main, sym);
    if (ocs_dl_parse_init_entry(sym, stru, sizeof(stru) - 1)) {
      if (res == NULL) { /* not found in main */
	h = lookup_dl_map(stru); /* lookup handle */
	if (h == NULL) {
	  sprintf(dlopen_error_buf, "no handle for %s ?!", stru);
	  return NULL;
	};
	res = dlsym(h, sym); /* try again */
      };
      init = res; /* init entry already found */
    } else {
      if (ocs_dl_parse_symbol(sym, stru, sizeof(stru) - 1,
			      fun, sizeof(fun) - 1)) {
	if (res == NULL) { /* not found in main */
	  h = lookup_dl_map(stru); /* find dlopen handel */
	  if (h == NULL) {
	    sprintf(dlopen_error_buf, "no handle for %s ?!", stru);
	    return NULL;
	  };
	  res = dlsym(h, sym); /* try again */
	};
	ocs_dl_make_init_entry(stru, fun, sizeof(fun) - 1);
	init = dlsym(h, fun); /* lookup init entry */
      };
    };
    /* Call init entry -- it is possible that the initialization entry of
     * the structure defining this symbol has not yet been called,
     * since it belongs to a shared library. */
    if (init != NULL) {
      (*init)();
    };
    return res;
}

/* return a file, from which the unknown symbols can be read;
   IN: objfile the (shared) object file (name must contain a '.'), 
   OUT: if transientfnam is not NULL, caller should unlink this file
*/
static FILE *dl_dlopen_nmu(char *structure, char *objfile, 
			   char *transientfnam, int *transient) {
  char outbuf[256];
  char cmdbuf[512];
  FILE *symfile;
  char *s;
  struct stat statbuf;

  strncpy(outbuf, objfile, sizeof(outbuf) - 1);
  s = strrchr(outbuf, '.');
  strcpy(s, ".nmu");
  if (0 == stat(outbuf, &statbuf)) {
    symfile = fopen(outbuf, "r");
    if (symfile != NULL) {
      *transient = 0;
      return symfile;
    }
  }

  /* no nmu file found */
  tmpnam(outbuf);
  /* get undefined symbols of this object file */
  strcpy(cmdbuf, "${NMU} ");
  strcat(cmdbuf, objfile);
  strcat(cmdbuf, " > ");
  strcat(cmdbuf, outbuf);
  DLDDEBUG(fprintf(stderr, "retrieving undefined symbols with `%s'\n", 
		   cmdbuf));
  if (system(cmdbuf) != 0 || (symfile = fopen(outbuf, "r")) == NULL){
    sprintf(dlopen_error_buf,
	    "cannot retrieve symbols of `%s'", 
	    charbuf);
    return NULL;
  }
  *transient = 1;
  strcpy(transientfnam, outbuf);
  return symfile;
}

/* recursively link structures referred to by symbols in symfile */
static int dl_link_referred(FILE *symfile) {
  char cmdbuf[512];
  char refstruct[128];

  while (fgets(cmdbuf, sizeof(cmdbuf)-1, symfile)){
    int i = strlen(cmdbuf);
    char *s = cmdbuf;
    while (i > 0 && (cmdbuf[i-1] == '\n' || cmdbuf[i-1] == ' ')){
      cmdbuf[i-1] = 0;
    }
    if (ocs_dl_parse_init_entry(s, refstruct, 
				sizeof(refstruct)-1)){
      if (!ocs_dl_link(refstruct)){
	return 0;
      }
    } else {
      DLDDEBUG(fprintf(stderr," symbol `%s' not an init entry\n",
		       s));
    }
  };
  return 1;
}

/* create shared object file for input object file; sz is sizeof of fname */
static int dl_dlopen_create_so(char *fname, int sz){
  char outbuf[256];
  char cmdbuf[512];

  tmpnam(outbuf);

  strcat(outbuf, ".so");
  strcpy(cmdbuf, "${MKDLOPEN} -o ");
  strcat(cmdbuf, outbuf);
  strcat(cmdbuf, " ");
  strcat(cmdbuf, fname);
  DLDDEBUG(fprintf(stderr, "linking transient shared object with `%s'\n",
		   cmdbuf));
  if (system(cmdbuf) != 0){
    sprintf(dlopen_error_buf,
	    "cannot create transient shared object `%s' for `%s'", 
	    outbuf, fname);
    return 0;
  }
  strncpy(fname, outbuf, sz);
  return 1;
}

static int dl_dlopen_link(char * structure){
    
    char charbuf[512];    
    int transient = 0;
    static double total = 0.0;
    static double totalu = 0.0;
    static double totalL = 0.0;
    static double totaln = 0.0;
    double t, n;
    int library = 0, shared = 0;

    DLDDEBUG(fprintf(stderr, "linking structure %s\n", structure));

    /* check if structure is already linked */
    ocs_dl_make_init_entry(structure, charbuf, sizeof(charbuf)-1);
    if (ocs_dl_resolve(charbuf) != NULL){
	return 1;
    }

    /* find object file */
    if (ocs_dl_find_object_file(structure, charbuf, sizeof(charbuf)-1) == 0){
	sprintf(dlopen_error_buf,
		"cannot locate object file of structure `%s'", structure);
	return 0;
    }

    DLDDEBUG(fprintf(stderr, "object file: %s\n", charbuf));

    library = (strncmp(charbuf, "lib", 3) == 0);
    shared = (strstr(charbuf, sh_object_file_suffix) != NULL);
    /* FIXME: better detection */
    if (!library) {
	char outbuf[256];
	int transNMU;
	FILE * symfile;

	double u;

	TIMER(startTimer());
	/* get undefined symbols of this object file */
	symfile = dl_dlopen_nmu(structure, charbuf, outbuf, &transNMU);
	if (symfile == NULL) return 0;
	TIMER(n = stopTimer());

	/* recursively link referred structures */
	TIMER(startTimer());
	if (!dl_link_referred(symfile)) return 0;
	TIMER(u = stopTimer());
	TIMER(startTimer());
	fclose(symfile);
	if (transNMU) unlink(outbuf);
	if (!shared) {
  	  /* create shared object */
	  if (!(dl_dlopen_create_so(charbuf, sizeof(charbuf)))) return 0;
	  transient = 1;
	};
	handle_counter ++;
	TIMER(t = stopTimer());
	TIMER(total += t);
	TIMER(totalu += u);
	TIMER(totaln += n);
	TIMER(fprintf(stderr, "%13s", structure ));
	TIMER(fprintf(stderr, " #%d nm:%.3f|%.3f so:%.3f|%.3f rec:%.3f|%.3f",
		      handle_counter, n, totaln, t, total, u, totalu));
    }

    TIMER(startTimer());
    /* Now lookup and initialize structure. */
    {
	void *handle;
	char initsym[128];
	char *error;
	void (*init)();
	handle = dlopen(charbuf, RTLD_NOW|RTLD_GLOBAL);
	if (handle == NULL){
	    sprintf(dlopen_error_buf,
		    "cannot open shared object `%s' (%s)", 
		    charbuf, dlerror());
	    if (transient) unlink(charbuf);
	    return 0;
	}
	ocs_dl_make_init_entry(structure, initsym, sizeof(initsym)-1);

	handle = set_dl_map(structure, handle);
	if (handle == NULL) {
	  sprintf(dlopen_error_buf, "cannot store dlopen handle of `%s' ?!",
		  structure);
	  if (transient) unlink(charbuf);
	  return 0;
	}

	dlerror(); /* clear dlerror pointer */
	init = dlsym(handle, initsym);
	if ((error = dlerror()) != NULL){
	    sprintf(dlopen_error_buf,
		    "cannot find structure `%s' in shared object `%s' (%s)", 
		    structure, charbuf, error);
	    if (transient) unlink(charbuf);
	    return 0;
	}
	(*init)();
    }
    if (transient) unlink(charbuf);
    TIMER(t = stopTimer());
    TIMER(totalL += t);
    TIMER(fprintf(stderr, " dl:%.3f|%.3f\n", t, totalL));
    return 1;
}


static int dl_dlopen_unlink(char *structure){
    sprintf(dlopen_error_buf,
	    "unlinking of `%s' not yet implemented", 
	    structure);
    return 0;
}    

static char *dl_dlopen_error(){
    return dlopen_error_buf;
}

#endif 

/* Initializing dynamic linking. */

static void init_dl(){
    ocs_dl_debug = getenv("OCS_DL_DEBUG") != NULL;
#   ifdef HAVE_DLOPEN
    ocs_dl_def_method(dl_dlopen_resolve, 
		      dl_dlopen_link, dl_dlopen_unlink, dl_dlopen_error);
#   else
    ocs_dl_def_method(dl_default_resolve, 
		      dl_default_link, dl_default_unlink, dl_default_error);
#   endif
}




/* =========================================================================
 * handcoding support
 */

extern OBJ _alloc_big(WORD words,WORD ofs){
    OBJ r;
    if((words+1) < big_escape_ssize){        /* account one word for big-header */
        _alloc(words+1,r);
        _mkHeader(_header(r),(words+1) + ofs,1);
    }else{
        r=_bigAlloc(words);
        _mkHeader(_header(r),big_escape_ssize + ofs,1);
    }
    ((BCELL)(r))->size = pack_word(words);
    return r;
}

extern void* malloc_aux(int sz){
    /* ---> to become reimplemented */
    return malloc(sz);
}

extern void free_aux(void* p){
    /* ---> to become reimplemented */
    free(p);
}




/* =========================================================================
 * tracing & debugging
 */

static void (*warn_func)(char *msg);
static void (*halt_func)(char *msg);
static void (*exit_func)(int code);
static void (*trace_enter_func)(char *msg);
static void (*trace_exit_func)(char *msg);
static void (*trace_msg_func)(char *msg);

void (*ocs_warn_def_method(void (*func)(char *)))(char *){
    void (*old)(char *) = warn_func;
    warn_func = func;
    return old;
}

void (*ocs_halt_def_method(void (*func)(char *)))(char *){
    void (*old)(char *) = halt_func;
    halt_func = func;
    return old;
}

void (*ocs_exit_def_method(void (*func)(int)))(int){
    void (*old)(int) = exit_func;
    exit_func = func;
    return old;
}

void ocs_trace_def_method(void (*enter)(char *),
			  void (*exit)(char *),
			  void (*msg)(char *)){
    trace_enter_func = enter;
    trace_exit_func = exit;
    trace_msg_func = msg;
}


void ocs_warn(char *msg){
    (*warn_func)(msg);
}

void ocs_halt(char *msg){
    (*halt_func)(msg);
}

void ocs_exit(int code){
    (*exit_func)(code);
}

/* for upwards compatibility FIXME: remove me */
void _halt(char *msg){
    (*halt_func)(msg);
}

void ocs_trace_enter(char *msg){
    (*trace_enter_func)(msg);
}

void ocs_trace_exit(char *msg){
    (*trace_exit_func)(msg);
}

void ocs_trace_msg(char *msg){
    (*trace_msg_func)(msg);
}


static int level = 0;
static int trace = -1;

static void default_trace_enter(char *m){
   if (trace < 0) trace = getenv("OCS_TRACE") != NULL;
   if (trace) {
       fprintf(stderr,"(%s\n",m);
       level++;
   }
}

static void default_trace_exit(char *m){
    if (trace < 0) trace = getenv("OCS_TRACE") != NULL;
    if (trace) {
	level--;
       fprintf(stderr,")");
    }
}

static void default_trace_msg(char *m){
    if (trace < 0) trace = getenv("OCS_TRACE") != NULL;
    if (trace) {
	level--;
	fprintf(stderr, "%s", m);
    }
}

static void default_halt(char *msg){
    fflush(stdout);
    fputs("\nRUNTIME ERROR: ",stderr); fputs(msg,stderr); fputc('\n',stderr);
    abort();
}

static void default_warn(char *msg){
    fflush(stdout);
    fputs("\nUNDEFINED CONSTANT: ",stderr); fputs(msg,stderr); fputc('\n',stderr);
    fputs("this may lead to unpredictable behaviour\n", stderr);
}

static void default_exit(int code){
    exit(code);
}

static void init_debug(){
    ocs_warn_def_method(default_warn);
    ocs_halt_def_method(default_halt);
    ocs_exit_def_method(default_exit);
    ocs_trace_def_method(default_trace_enter,default_trace_exit, 
			 default_trace_msg);
}


/* =========================================================================
 * boolean type from Ac unit BUILTIN
 */

extern OBJ _ABUILTIN_Afalse_(OBJ x) {
    OBJ r;
    ABUILTIN_Afalse_(x,r);
    return r;
}
extern OBJ _ABUILTIN_Atrue_(OBJ x) {
    OBJ r;
    ABUILTIN_Atrue_(x,r);
    return r;
}
extern OBJ _ABUILTIN_St(OBJ x) {
    OBJ r;
    ABUILTIN_St(x,r);
    return r;
}
extern OBJ _ABUILTIN_Aand(OBJ x1,OBJ x2) {
    OBJ r;
    ABUILTIN_Aand(x1,x2,r);
    return r;
}
extern OBJ _ABUILTIN_Aor(OBJ x1,OBJ x2) {
    OBJ r;
    ABUILTIN_Aor(x1,x2,r);
    return r;
}
extern OBJ _ABUILTIN_Se(OBJ x1,OBJ x2) {
    OBJ r;
    ABUILTIN_Se(x1,x2,r);
    return r;
}
extern OBJ _ABUILTIN_SSe(OBJ x1,OBJ x2) {
    OBJ r;
    ABUILTIN_SSe(x1,x2,r);
    return r;
}

OBJ __ABUILTIN_Atrue_,__ABUILTIN_Afalse_,__ABUILTIN_Aand,
    __ABUILTIN_Aor,__ABUILTIN_St,__ABUILTIN_Se,__ABUILTIN_SSe;

static OBJ _mt_2_0_2(OBJ t,OBJ t1,OBJ t2)
{OBJ r;
 DCCLS(t,1);
 r=(*(OBJ(*)(OBJ,OBJ))ENTRY(t))(t1,t2);
 return r;}

static OBJ _mt_1_0_1(OBJ t,OBJ t1)
{OBJ r;
 DCCLS(t,1);
 r=(*(OBJ(*)(OBJ))ENTRY(t))(t1);
 return r;}
static OBJ _mt_1_0_1_l(OBJ t,OBJ t1)
{OBJ r;
 DCCLS(t,1);
 r=(*(OBJ(*)(OBJ))ENTRY(t))(t1);
 COPY(r,1);LZYCLS(t,r);
 return r;}

extern CODE _mttab_1[], _mttab_2[];

static void init_bool_const(){
 MTH(2,0,2,_mt_2_0_2);LZYMTH(2,0,2,_mt_2_0_2);
 MTH(1,0,1,_mt_1_0_1);LZYMTH(1,0,1,_mt_1_0_1_l);
 /* initializations cannot be done using CLS since the symbol
    for dynamic linking must be the real numeric one, not the 
    symbolic */
 __ABUILTIN_Aand = _cls("_ABUILTIN_9", (CODE)_ABUILTIN_Aand, _mttab_2, 2);
 __ABUILTIN_Aor  = _cls("_ABUILTIN_8", (CODE)_ABUILTIN_Aor, _mttab_2, 2);
 __ABUILTIN_Se   = _cls("_ABUILTIN_10", (CODE)_ABUILTIN_Se, _mttab_2, 2);
 __ABUILTIN_SSe  = _cls("_ABUILTIN_11", (CODE)_ABUILTIN_SSe, _mttab_2, 2);
 __ABUILTIN_Atrue_  = _cls("_ABUILTIN_6", (CODE)_ABUILTIN_Atrue_, _mttab_1, 1);
 __ABUILTIN_Afalse_  = _cls("_ABUILTIN_4", (CODE)_ABUILTIN_Afalse_, _mttab_1, 1);
 __ABUILTIN_St       = _cls("_ABUILTIN_7", (CODE)_ABUILTIN_St, _mttab_1, 1);
}

/* =========================================================================
 * denotation type from Ac unit BUILTIN
 */

char charbuf[CHARBUFSIZE];

extern OBJ alloc_denotation(int leng){
    OBJ r;
    alloc_big_byte_flat(sizeof_big(struct sDENOTATION) + size_data(leng+1),r);
    ((DENOTATION)(r))->leng = pack_word(leng);
    data_denotation(r)[leng] = 0;
    return r;
}

extern OBJ make_denotation(char * s){
    int l = strlen(s);
    OBJ den = alloc_denotation(l);
    memcpy((void*)data_denotation(den),(void*)s,l);
    return den;
}

int get_denotation(OBJ d, char * buf, int size){
    int n,c,l = leng_denotation(d);
    if (size-1 < l){
	/* truncate */
	n = size-1; c = 0;
    } else {
	n = l; c = 1;
    }
    memcpy((void*)buf,(void*)data_denotation(d),n);
    buf[n] = 0;
    free_denotation(d,1);
    return c;
}


OBJ __ABUILTIN_Atl,__ABUILTIN_SlS;

extern OBJ _ABUILTIN_Atl(OBJ d){
    int l = leng_denotation(d);
    if (l > 0){
	OBJ r = alloc_denotation(l-1);
	memcpy((void*)data_denotation(r),(void*)(data_denotation(d)+1),l-1);
	free_denotation(d,1);
	return r;
    } else {
	HLT("tl'DENOTATION: denotation is empty");
	return 0;
    }
}

extern OBJ _ABUILTIN_SlS(OBJ d1,OBJ d2){
    int l1 = leng_denotation(d1), l2 = leng_denotation(d2);
    if (l1 <= l2){
	while (l1 > 0){
	    if (data_denotation(d1)[l1-1] == data_denotation(d2)[l1-1])
		l1--;
	    else break;
	}
    } /* else l1 > 0 */
    free_denotation(d1,1); free_denotation(d2,1);
    return pack_clean_bool(l1 == 0);
}


static void init_denotation_const(){
 __ABUILTIN_Atl = _cls("_ABUILTIN_1", (CODE)_ABUILTIN_Atl, _mttab_1, 1);
 __ABUILTIN_SlS = _cls("_ABUILTIN_2", (CODE)_ABUILTIN_SlS, _mttab_2, 2);
}
    

/* =========================================================================
 * function ABORT from Ac unit BUILTIN
 */

OBJ __ABUILTIN_AABORT;

extern OBJ _ABUILTIN_AABORT(OBJ msg){
    get_denotation(msg,charbuf,CHARBUFSIZE);
    HLT(charbuf);
    return 0; //never reached?
}

static void init_ABORT_const(){
 __ABUILTIN_AABORT = _cls("_ABUILTIN_12", (CODE)_ABUILTIN_AABORT, _mttab_1, 1);
}

/* =========================================================================
 * re-implementation of UNIX functions 
 */

#ifndef HAVE_CLEAN_MEMCP

extern int ocsmemcmp(const void *s1, const void *s2, size_t n){
     int r;
     unsigned char * p1 = (unsigned char *)s1;
     unsigned char * p2 = (unsigned char *)s2;

     while (n > 0){
	 r = *p1 - *p2;
	 if (r != 0) return r;
	 n--; p1++; p2++;
     }
     return 0;
}

#endif /* ! HAVE_CLEAN_MEMCMP */
	 
#ifndef HAVE_STRERROR

extern char * ocsstrerror(int no){
#ifdef HAVE_SYS_ERRLIST
  extern char * sys_errlist[];
  return sys_errlist[no];
#else
  static char buf[20];
  sprintf(buf,"error #%n", no);
  return buf;
#endif
}

#endif /* ! HAVE_STRERROR */

/* This is for CYGWIN only */
#ifdef OCS_CYGWIN
int _bss_end__ ;   
int _bss_start__ ; 
int _data_end__ ;  
int _data_start__ ;
#endif

/* =========================================================================
 * initialization 
 */



extern void init_ABUILTIN(){
 int static visited = 0;
 if (visited != 0) return;
 visited = 1;
#ifdef _GCCGLOBREG_
  _freeList = _freeListMem;
#endif
 max_word = ULONG_MAX / 2 - 1;
 max_sword = LONG_MAX / 2 - 1;
 min_sword = LONG_MIN / 2 + 1;
 bits_per_word = sizeof(WORD)*CHAR_BIT - 1;
 init_debug();
 init_bool_const();
 init_denotation_const();
 init_ABORT_const();
 init_dl();
}


/* make real objects for TRUE and FALSE for dynamic linking */
#undef __ABUILTIN_3
#undef __ABUILTIN_5

OBJ __ABUILTIN_3 = pack_word(0);
OBJ __ABUILTIN_5 = pack_word(1);
