/*  LAST EDIT: Tue Feb  6 19:02:27 1996 by Christian Maeder (andromache!maeder)  */
/* hand-coded implementation part of Dbm */
/* coding scheme version acc-2.1 */

#include  "unixconfig.h"

#include "gdbm.h"

#include "Nat.h"
#include "Com.h"
#include "Seq.h"
#include "UnixFailures.h"

#include "Dyn.h"

#define blockSize 512
#define fileMode (0400|0200 | 040 | 04)

#define retryInterval 5		/* seconds to sleep for next retry */
#define retryMsg      2		/* number of retrys message is printed */



#define return_gdbm_failure(code){\
    char * __gdbm_msg = mystrdup(gdbm_strerror(code)); \
    OBJ __gdbm_res = _ACom_Afail(make_denotation(__gdbm_msg)); \
    free(__gdbm_msg); \
    return __gdbm_res; \
}
	/* the dup is just included because make_denotation's 
    	   argument isn't declared as a constant and gdbm_strerr
    	   yields such one ... to avoid these nasty warnings.
        */

static char * mystrdup (const char * s){
    char * res = (char *)malloc(strlen(s) + 1);
    strcpy(res, s);
    return res;
}

#define pack_dbf(x) pack_pointer(x)
#define unpack_dbf(x) unpack_pointer(x)


extern OBJ _ADbm_ACreate(OBJ Name,OBJ Mode,OBJ Cache,OBJ Unit) /* Create */ {
    GDBM_FILE dbf;
    int acc;

    get_denotation(Name, charbuf, CHARBUFSIZE);

    acc = Mode == __ADbm_AasyncMode ? GDBM_WRCREAT | GDBM_FAST : GDBM_WRCREAT;

    errno = gdbm_errno = 0;
    dbf = gdbm_open(charbuf, blockSize, acc, fileMode, NULL);

    if (dbf == NULL){
	if (gdbm_errno != 0){
	    return_gdbm_failure(gdbm_errno);
	} else {
	    return_unix_failure(errno);
	}
    } else {

	int cache = unpack_nat(Cache);
	if (cache != 0){
	    gdbm_setopt(dbf, GDBM_CACHESIZE, &cache, sizeof(int));
	}

	return_okay(pack_dbf(dbf));
    }
}

extern OBJ _ADbm_AOpen(OBJ Name,OBJ Msg,OBJ Acc,OBJ Mode,OBJ Cache,OBJ Unit) {
    GDBM_FILE dbf;
    int acc, retry;
    char msgbuf[256];

    get_denotation(Name, charbuf, CHARBUFSIZE);
    get_denotation(Msg,  msgbuf, sizeof(msgbuf));

    if (Acc == __ADbm_AwriterAccess){
	acc = GDBM_WRITER; retry = 1;
    } else 
    if (Acc == __ADbm_AtryWriterAccess){
	acc = GDBM_WRITER; retry = 0;
    } else
    if (Acc == __ADbm_AreaderAccess){
	acc = GDBM_READER; retry = 1;
    } else {
	acc = GDBM_READER; retry = 0;
    }
    if (Mode == __ADbm_AasyncMode){
	acc |= GDBM_FAST;
    }

    for (;;){

	errno = gdbm_errno = 0;
	dbf = gdbm_open(charbuf, blockSize, acc, fileMode, NULL);

	if (dbf == NULL) {

	    if (retry && (gdbm_errno == GDBM_CANT_BE_READER ||
	        	  gdbm_errno == GDBM_CANT_BE_WRITER)) {

	        if ((retry-1) % retryMsg == 0  &&  msgbuf[0]){
	  	    fputs(msgbuf, stderr); fputc('\n', stderr);
	        }
	        retry++;

	        sleep(retryInterval);

	    } else 

	    if (gdbm_errno != 0){
		return_gdbm_failure(gdbm_errno);
	    } else {
		return_unix_failure(errno);
	    }

	} else {

	    int cache = unpack_nat(Cache);
	    if (cache != 0){
		gdbm_setopt(dbf, GDBM_CACHESIZE, &cache, sizeof(int));
	    }

	    return_okay(pack_dbf(dbf));
	}
    }
}


extern OBJ _ADbm_AClose(OBJ Dbf, OBJ Unit) /* Close */ {
    GDBM_FILE dbf = unpack_dbf(Dbf);

    errno = gdbm_errno = 0;
    gdbm_close(dbf);

    if (gdbm_errno != 0){
	return_gdbm_failure(gdbm_errno);
    } else 
    if (errno != 0) {
	return_unix_failure(errno);
    } else {
	return_okay_nil;
    }
}


extern OBJ _ADbm_AUpdate(OBJ Dbf,OBJ Key,OBJ Data,OBJ Unit) /* Update */ {
    GDBM_FILE dbf = unpack_dbf(Dbf);
    datum key, data;
    int ret;

    key.dptr = data_denotation(Key); key.dsize = leng_denotation(Key);
    data.dptr = base_dyn(Data); data.dsize = size_dyn(Data);

    errno = gdbm_errno = 0;
    ret = gdbm_store(dbf, key, data, GDBM_REPLACE);
    free_denotation(Key, 1); free_dyn(Data, 1);

    if (ret == 0) {
	return_okay_nil;
    } else
    if (gdbm_errno != 0) {
	return_gdbm_failure(gdbm_errno);
    } else {
	return_unix_failure(errno);
    }
}


extern OBJ _ADbm_ASelect(OBJ Dbf,OBJ Key,OBJ Unit) /* Select */ {
    GDBM_FILE dbf = unpack_dbf(Dbf);
    datum key, data;

    key.dptr = data_denotation(Key); key.dsize = leng_denotation(Key);

    errno = gdbm_errno = 0;
    data = gdbm_fetch(dbf, key);
    free_denotation(Key, 1); 

    if (data.dptr != NULL) {
	OBJ Res;
	alloc_big_flat(size_data(data.dsize), Res);
	memcpy(base_dyn(Res), data.dptr, data.dsize);
	free(data.dptr);
	return_okay(Res);
    } else
    if (gdbm_errno != 0) {
	return_gdbm_failure(gdbm_errno);
    } else {
	return_unix_failure(errno);
    }
}


extern OBJ _ADbm_AExists(OBJ Dbf,OBJ Key,OBJ Unit) /* Exists */ {
    GDBM_FILE dbf = unpack_dbf(Dbf);
    datum key, data;
    int ret;

    key.dptr = data_denotation(Key); key.dsize = leng_denotation(Key);

    errno = gdbm_errno = 0;
    ret = gdbm_exists(dbf, key);
    free_denotation(Key, 1); 

    return_okay(pack_bool(ret));
}


extern OBJ _ADbm_ADelete(OBJ Dbf,OBJ Key,OBJ Unit) /* Delete */ {
    GDBM_FILE dbf = unpack_dbf(Dbf);
    datum key, data;
    int ret;

    key.dptr = data_denotation(Key); key.dsize = leng_denotation(Key);

    errno = gdbm_errno = 0;
    ret = gdbm_delete(dbf, key);
    free_denotation(Key, 1); 

    if (ret == 0) {
	return_okay_nil;
    } else
    if (gdbm_errno != 0) {
	return_gdbm_failure(gdbm_errno);
    } else {
	return_unix_failure(errno);
    }
}



extern OBJ _ADbm_ADom(OBJ Dbf, OBJ Unit) /* Dom */ {
    GDBM_FILE dbf = unpack_dbf(Dbf);
    datum key;
    OBJ Res = __ASeq_Slg, Key;

    errno = gdbm_errno = 0;
    key = gdbm_firstkey(dbf);

    while (key.dptr != NULL){

	Key = alloc_denotation(key.dsize);
	memcpy(data_denotation(Key), key.dptr, key.dsize);
	Res = _ASeq_Sii(Key, Res);
	free(key.dptr);
	key = gdbm_nextkey(dbf, key);
    }

    return_okay(Res);
}


extern OBJ _ADbm_AReorganize(OBJ Dbf,OBJ Unit) /* Reorganize */ {
    GDBM_FILE dbf = unpack_dbf(Dbf);
    int ret;

    errno = gdbm_errno = 0;
    ret = gdbm_reorganize(dbf);

    if (ret == 0) {
	return_okay_nil;
    } else
    if (gdbm_errno != 0) {
	return_gdbm_failure(gdbm_errno);
    } else {
	return_unix_failure(errno);
    }
}



extern OBJ _ADbm_ASync(OBJ Dbf,OBJ Unit) /* Sync */ {
    GDBM_FILE dbf = unpack_dbf(Dbf);
    int ret;

    errno = gdbm_errno = 0;
    gdbm_sync(dbf);

    if (gdbm_errno != 0) {
	return_gdbm_failure(gdbm_errno);
    } else 
    if (errno != 0){
	return_unix_failure(errno);
    } else {
	return_okay_nil;
    }
}


static init_const_ADbm(){
    init_ANat();
    init_ASeq();
    init_ADyn();
    init_ACom();
    init_AUnixFailures();

    {   char * msg;   /* just because of the constant problem ... see 
    		         return_gdbm_failure. */
	msg = mystrdup(gdbm_strerror(GDBM_CANT_BE_READER));
	__ADbm_AcantBeReader = make_denotation(msg); free(msg);


	msg = mystrdup(gdbm_strerror(GDBM_CANT_BE_WRITER));
	__ADbm_AcantBeWriter = make_denotation(msg); free(msg);
    }
}
