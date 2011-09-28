/* hand-coded implementation part of BinFile */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>

#include "Com.oc.h"
#include "File.oc.h"
#include "UnixFailures.oc.h"

#define DPRINT

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Object descriptors
 */

/* 
   31		    15		   0
   xxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx1	Self containing Value
   ffffffffffffffff sssssssssssss000 	Exclusive cell (f = flags, s = size)
   ffffffffffffffff sssssssssssss010 	Shared    cell (f = flags, s = size)
   llllllllllllllll lllllllllllll100	Reference      (l = label)
*/

#define isVal(d) 	((WORD)(d)&0x1)
#define isExclCell(d)	(((WORD)(d)&0x7)==(WORD)0x0)
#define isSharedCell(d) (((WORD)(d)&0x7)==(WORD)0x2)
#define isRefToCell(d)  (((WORD)(d)&0x7)==(WORD)0x4)

#define mkExclCell(f,s)   (OBJ)( ((WORD)(f) << 16) | ((WORD)(s) << 3) )
#define mkSharedCell(f,s) \
	(OBJ)( ((WORD)(f) << 16) | ((WORD)(s) << 3) | (WORD)0x2)

#define mkRefToCell(l)	(OBJ)(((WORD)(l) << 3) | (WORD)0x4)

#define getSize(d)	( ((WORD)(d) & 0xffff) >> 3 )
#define getFlags(d)	( (WORD)(d) >> 16 )
#define getLabel(d)	( (WORD)(d) >> 3 )


/* Representation of descriptor on file:

   b_n...b_1 x		where b_i = 0..127 x = 128..255
			      long = b_n*(128^n) + ... + b_1*128 + (x-128)
		      
*/

static void write_obj(FILE *f, OBJ ob, OBJ *ep){
    int c; 
    WORD buf[12]; 
    int i = 0; 
    WORD x = (WORD)ob;

    do {
	buf[i++] = x % 128;
	x /= 128;
    } while (x);

    while (i > 1){
	c = buf[--i];
	if (putc(c,f) != c){
	    get_unix_failure(errno,*ep);    
	    return;
	}
    }
    c = buf[0] + 128;
    if (putc(c,f) != c){
	get_unix_failure(errno,*ep);    
    }
}

static OBJ read_obj(FILE *f, OBJ *ep){
    WORD x = 0; 
    int c;
    for (;;) {
	if ((c = getc(f)) == EOF){
	    get_unix_failure(errno,*ep);    
	    return (OBJ)x;	
	}
	if (c >= 128){
	    return (OBJ)(x*128 + (c-128));
	}
	x = x*128 + c;
    } 
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Hashtable for mapping memory locations to labels & vice versa
 */


typedef struct sHASHENTRY {
    struct sCELL header;
    struct sHASHENTRY * next;
    OBJ obj;
    WORD label;
} * HASHENTRY;

HASHENTRY newEntry() {
    OBJ r; 
    alloc_small_flat(sizeof_small(struct sHASHENTRY), r);
    return (HASHENTRY)r;
}

void freeEntry(HASHENTRY e){
    dispose_structured_flat((OBJ)e);
}


#define HASHSIZE 256
typedef struct sHASHTAB {
    struct sBCELL header;
    HASHENTRY tab[HASHSIZE];
} * HASHTAB;

HASHTAB newHashTab() { 
    OBJ r; HASHTAB h; int i;

    alloc_big_flat(sizeof_big(struct sHASHTAB), r);
    h = (HASHTAB)r;
    for (i = 0; i < HASHSIZE; i++) h->tab[i] = NULL;
    return h;
}

void freeHashTab(HASHTAB t) {
    int i; HASHENTRY e,e1;
    for (i = 0; i < HASHSIZE; i++) {
	for (e = t->tab[i]; e != NULL; e = e1) {
	    e1 = e->next; freeEntry(e);
	}
    }
    dispose_structured_flat((OBJ)t);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Dumping
 */


static void dump(FILE *f, HASHTAB t, OBJ ob, int *labct, OBJ *ep) {

    if (is_structured(ob)){
	int sz = _size(_header(ob)), fs = _flags(_header(ob)), i; 
	intptr_t flds;
	OBJ *data;

	if (_tstRc(_header(ob),1)) {
	    write_obj(f,mkExclCell(fs,sz),ep);
	    if (*ep) return;
	} else {
	    HASHENTRY e;

	    /* check if object already dumped */
	    i = (WORD)ob % HASHSIZE;
	    for (e = t->tab[i]; e; e = e->next) {
		if (e->obj == ob) {
		    write_obj(f,mkRefToCell(e->label),ep);
		    return;
		}
	    }

	    /* first time dumping a shared cell */
	    write_obj(f,mkSharedCell(fs,sz),ep);
	    if (*ep) return;

	    /* create new entry in hashtab and assign 
	       label to it, since we might have to visit it again. 
	       */
	    e = newEntry();
	    e->label = (*labct)++;
	    write_obj(f,mkRefToCell(e->label),ep);
	    if (*ep) return;
	    e->obj = ob;
	    e->next = t->tab[i]; t->tab[i] = e;
	}

	/* calculate no of fields */
	if (sz % flat_offset_ssize == big_escape_ssize){
	    flds = unpack_word(((BCELL)ob)->size);
	    write_obj(f,(OBJ)flds,ep);
	    if (*ep) return;
	    data = _bdata(ob);
	} else {
	    flds = sz % flat_offset_ssize;
	    data = _data(ob);
	}

	if (sz >= flat_offset_ssize) {
	    if (fs & (1 << byte_flat_sflag)){
		/* write 2 words and rest as char stream */
		unsigned char * cdata = (unsigned char*)(data+2);
		int cflds = (flds-2) * sizeof(OBJ);
		write_obj(f,data[0],ep); write_obj(f,data[1],ep); 
		for (i = 0; i < cflds && !*ep; i++) {
		    if (putc(cdata[i],f) != cdata[i]){
			get_unix_failure(errno,*ep);
		    }
		}
	    } else {
		/* write fields as coded longwords */
		for (i = 0; i < flds && !*ep; i++) {
		    write_obj(f,data[i],ep);
		}
	    }
	} else {
	    /* recursivly dump fields. */
	    for (i = 0; i < flds && !*ep; i++) {
		dump(f,t,data[i],labct,ep);
	    }
	}
    } else {
	write_obj(f,ob,ep);
    }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Parsing
 *
 */

static OBJ parse(FILE *f, HASHTAB t, OBJ *ep){
    OBJ d;
    int shared;
  
    d = read_obj(f,ep);
    if (*ep) return NIL;

    if (isVal(d)) {
	return d;
    } else if (isRefToCell(d)) {
	WORD lab = getLabel(d);
	int i; HASHENTRY e;
	i = lab % HASHSIZE;
	for (e = t->tab[i]; e; e = e->next) {
	    if (e->label == lab) {
		/* increment RC of refered cell and return it. */
		_incRc(_header(e->obj),1);
		return e->obj;
	    }
	}
	/* Format error. */
	DPRINT(
	    "parse'BinFile: inconsistency: got refToCell but no hashed label");
	copy_some(__ABinFile_AinvalidFormat,1);
	*ep = __ABinFile_AinvalidFormat;
	return NIL;
    } else if ((shared = isSharedCell(d)) || isExclCell(d)) {
	int sz = getSize(d), fs = getFlags(d), i; 
	intptr_t flds;
	OBJ * data; OBJ ob; WORD lab;
	    
	if (shared) {
	    lab = (WORD)read_obj(f,ep);
	    if (*ep) return NIL;
	    if (!isRefToCell(lab)){
		DPRINT(
	"parse'BinFile: inconsistency: expected refToCell after shared");
		copy_some(__ABinFile_AinvalidFormat,1);
		*ep = __ABinFile_AinvalidFormat;
		return NIL;
	    }
	}

	if (sz % flat_offset_ssize == big_escape_ssize){
	    flds = (intptr_t)read_obj(f,ep);
	    if (*ep) return NIL;
	    ob = _bigAlloc(flds);
	    _mkHeader(_header(ob),sz,1);
	    ((BCELL)ob)->size = pack_word(flds);
	    data = _bdata(ob);
	} else {
	    flds = sz % flat_offset_ssize;
	    _alloc(flds,ob);
	    _mkHeader(_header(ob),sz,1);
	    data = _data(ob);
	}
	_flags(_header(ob)) = fs;
	    
	if (shared){
	    HASHENTRY e;
	    lab = getLabel(lab);
	    e = newEntry();
	    e->label = lab; e->obj = ob;
	    i = lab % HASHSIZE;
	    e->next = t->tab[i]; t->tab[i] = e;
	}

	if (sz >= flat_offset_ssize) {
	    if (fs & (1 << byte_flat_sflag)){
		/* read two words and rest as char stream */
		/* (UPDATE STRING FORMAT) */
		unsigned char * cdata = (unsigned char*)(data+2);
		int ch, cflds = (flds-2) * sizeof(OBJ);
		data[0] = read_obj(f,ep); data[1] = read_obj(f,ep); 
		for (i = 0; i < cflds && !*ep; i++) {
		    if ((ch = getc(f)) != EOF){
			cdata[i] = ch;
		    } else
			get_unix_failure(errno,*ep);
		}
	    } else {
		/* read contents of unstructured cell. */
		for (i = 0; i < flds && !*ep; i++) {
		    data[i] = read_obj(f,ep);
		}
	    }
	} else {
	    /* recursivly parse structured cell. */
	    for (i = 0; i < flds && !*ep; i++) {
		data[i] = parse(f,t,ep);
	    }
	}

	/* process closures */
	if (tst_sflag(ob, closure_sflag)){
	    char msgbuf[128];
	    char * msg = link_closure(ob);
	    if (msg){
		strcpy(msgbuf,
		       data_denotation(__ABinFile_AlinkErrorPrefix));
		strcat(msgbuf,
		       data_denotation(((CLOSURE)ob)->symbolid));
		strcat(msgbuf,"': ");
		strcat(msgbuf,msg);
		*ep = declare_failure_answer(msgbuf);
		return NIL;
	    }
	}

	return ob;

    } else {
	DPRINT("parse'BinFile: inconsistency: no legal start");
	copy_some(__ABinFile_AinvalidFormat,1);
	*ep = __ABinFile_AinvalidFormat;
	return NIL;
    }
}
	    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * OPAL entrys
 */

OBJ _ABinFile_AwriteData(OBJ file,OBJ data,OBJ unit)
{
  FILE * f = unpack_file(file); 
  OBJ err  = (OBJ)0;
  HASHTAB t;
  int labct = 0;

  /* minimize sharing at this place */
  _forceAllNoneFlats();

  t = newHashTab();
  dump(f,t,data,&labct,&err);
  freeHashTab(t);
  free_some(data,1);

  if (err){
      return err;
  } else {
      return_okay_nil;
  }
}

OBJ _ABinFile_AreadData(OBJ file,OBJ unit)
{
  FILE * f = unpack_file(file); 
  OBJ err  = (OBJ)0, r;
  HASHTAB t;

  /* prepare for allocating a lot */
  _forceAllNoneFlats();

  t = newHashTab();
  r = parse(f,t,&err);
  freeHashTab(t);

  if (err){
      /* free of partial read object ????? */
      return err;
  } else {
      return_okay(r);
  }
}

static init_const_ABinFile()
{
 init_ACom();
 init_AFile();
 init_AUnixFailures();
}
