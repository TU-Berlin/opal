/* hand-coded implementation part of File */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:20 $ ($Revision: 1.1.1.1 $)
*/

#include <unixconfig.h>

#include "Nat.h"
#include "Int.h"
#include "Char.h"
#include "String.h"
#include "Com.h"
#include "UnixFailures.h"

#define READMONOLITHICSIZE	1024

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

/* -----------------------------------------------------------------------
 * opening / closing / positioning files
 */


extern OBJ _AFile_Axopen(OBJ name,OBJ kind,OBJ unit) {
    FILE * f; char kindbuf[10];
    if (!get_denotation(name,charbuf,CHARBUFSIZE)){
	return_fail(__AUnixFailures_AnameTooLong);
    }
    get_denotation(kind,kindbuf,sizeof(kindbuf));
    if ((f = fopen(charbuf,kindbuf)) != NULL){
        return_okay(pack_file(f));
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_Axreopen(OBJ name,OBJ kind,OBJ file,OBJ unit) {
    FILE * f = unpack_file(file); char kindbuf[10];
    if (!get_denotation(name,charbuf,CHARBUFSIZE)){
	return_fail(__AUnixFailures_AnameTooLong);
    }
    get_denotation(kind,kindbuf,sizeof(kindbuf));
    if ((f = freopen(charbuf,kindbuf,f)) != NULL){
	return_okay_nil;
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_Axflush(OBJ file,OBJ unit) {
    FILE *f = unpack_file(file);
    if (fflush(f) != EOF){
	return_okay_nil;
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_Axclose(OBJ file,OBJ unit) {
    FILE *f = unpack_file(file);
    if (fclose(f) != EOF){
	return_okay_nil;
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_Axerror_(OBJ file,OBJ unit){
    FILE *f = unpack_file(file);
    return_okay(pack_bool(ferror(f)));
}

extern OBJ _AFile_Axeof_(OBJ file,OBJ unit){
    FILE *f = unpack_file(file); int c;
    if ( (c = fgetc(f)) != EOF ) {
	ungetc(c,f);
        return_okay(pack_clean_bool(0));
    } else {
	return_okay(pack_clean_bool(1));
    }
}

extern OBJ _AFile_Axtell(OBJ file,OBJ unit) {
    FILE *f = unpack_file(file); long int p;
    if ( (p = ftell(f)) != -1L ){
	if (p <= unpack_nat(__ANat_Amax)){
	    return_okay(pack_nat(p));
	} else {
	    return_fail(__AUnixFailures_AfileTooLarge);
	}
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_Axsize(OBJ file,OBJ unit) {
    FILE *f = unpack_file(file); long int p,s;
    if ( (p = ftell(f)) != -1L ){
	if (fseek(f,0,SEEK_END) != -1L){
	    s = ftell(f);
	    fseek(f,p,SEEK_SET);
	    return_okay(pack_nat(s));
	} else {
            return_unix_failure(errno);
	}
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_Axseek(OBJ file,OBJ offs,OBJ mode,OBJ unit) {
    FILE *f = unpack_file(file); INT o = unpack_int(offs); int m;
    if (mode == __AFile_AfromCurrent) m = SEEK_CUR; else
    if (mode == __AFile_AfromEnd) m = SEEK_END; else m = SEEK_SET;
    if ( fseek(f,o,m) != -1L ){
        return_okay_nil;
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_Axrewind(OBJ file,OBJ unit) {
    FILE *f = unpack_file(file); 
    errno = 0;
    rewind(f);
    if (errno == 0){
        return_okay_nil;
    } else {
        return_unix_failure(errno);
    }
}

/* -----------------------------------------------------------------------
 * reading / writing files
 */

extern OBJ _AFile_AxunreadChar(OBJ file,OBJ ch, OBJ unit) {
    FILE *f = unpack_file(file); int c = unpack_char(ch);
    if ( ungetc(c,f) != EOF ){
        return_okay_nil;
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_AxreadChar(OBJ file,OBJ unit) {
    FILE *f = unpack_file(file); int c;
    if ( (c = fgetc(f)) != EOF ){
        return_okay(pack_char(c));
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_AxreadString(OBJ file,OBJ no,OBJ unit) {
    FILE *f = unpack_file(file); long int n = unpack_nat(no),p; 
    
    if ( n <= READMONOLITHICSIZE 
		&& (p = ftell(f)) != -1L && fseek(f,0,SEEK_END) != -1L ){
	/* able to determinate size of file */
        long int s,act; OBJ den;
	s = ftell(f); fseek(f,p,SEEK_SET);
	n = s < n ? s : n;
	if (n > 0){
	    den = alloc_denotation(n);
	    act = fread(data_denotation(den),sizeof(char),n,f);
	    if (act < 0){
	        free_denotation(den,1);
                return_unix_failure(errno);
	    } else 
	    if (act != n){
	        /* for some reason, not all bytes */
	        free_denotation(den,1);
	        return_fail(__AUnixFailures_AioError);
	    } else {
	        return_okay(_AString_AasString(den));
	    }
	} else {
	    return_okay(__AString_Slg);
	}
    } else {
	/* have to read in chuncks */
	long int request, act; 
	OBJ str = __AString_Slg,den; OBJ * strp = &str;
        do {
            request = n > CHARBUFSIZE ? CHARBUFSIZE : n;
            act = fread(charbuf,sizeof(char),request,f);
            if (act < 0){
                free_some(str,1);
                return_unix_failure(errno);
            } else
	    if (act > 0){
	        den = alloc_denotation(act);
            	memcpy((void*)data_denotation(den),(void*)charbuf,act);
	        AString_AasString(den,*strp);
	    	addr_rest_string(*strp,strp)
            	n -= act;
	    } else
		break;
        } while (act == request && n > 0);
        return_okay(str);
    }
}

OBJ _AFile_AxreadWhile(OBJ file, OBJ predicate, OBJ unit){
    OBJ d, s = __AString_Slg, *sp = &s; 
    int c; FILE *f = unpack_file(file); long i = 0;
    while ( (c = fgetc(f)) != EOF ){
        OBJ b;
        copy_closure(predicate,1);
        b = EVAL1(predicate,pack_char(c));
        if (!unpack_bool(b)){
            ungetc(c,f);
            break;
        }
        if (i >= CHARBUFSIZE){
	    d = alloc_denotation(CHARBUFSIZE);
       	    memcpy((void*)data_denotation(d),(void*)charbuf,CHARBUFSIZE);
	    AString_AasString(d,*sp);
	    addr_rest_string(*sp,sp);
            i = 0;
        }
        charbuf[i++] = c;
    }
    free_closure(predicate,1);
    if (i > 0){
        d = alloc_denotation(i);
        memcpy((void*)data_denotation(d),(void*)charbuf,i);
        AString_AasString(d,*sp);
    }
    return_okay(s);
}



extern OBJ _AFile_AxreadLine(OBJ file,OBJ unit) {
    FILE *f = unpack_file(file);
    if ( fgets(charbuf,CHARBUFSIZE,f) != NULL ){
        int n = strlen(charbuf); OBJ s;
        if (n > 0 && charbuf[n-1] == '\n'){
            n--;
        }
	if (n > 0){
	    s = alloc_denotation(n);
            memcpy((void*)data_denotation(s),(void*)charbuf,n);
            return_okay(_AString_AasString(s));
	} else {
	    return_okay(__AString_Slg);
	}
    } else {
        return_unix_failure(errno);
    }
}

extern OBJ _AFile_AxwriteChar(OBJ file,OBJ ch,OBJ unit) {
    FILE * f = unpack_file(file); int c = unpack_char(ch);
    if ( putc(c,f) != EOF ){
	return_okay_nil;
    } else {
        return_unix_failure(errno);
    }
}

static int write_string(FILE * f,OBJ str) {
    while (!is_empty_string(str)){
	OBJ data,rest; NAT start,leng;
	unpack_chunck_string(str,start,data,rest);
	leng = leng_denotation(data) - start;
        if ( fwrite(data_denotation(data) + start,sizeof(char),leng,f) != leng){
	    free_denotation(data,1);
	    free_some(rest,1);
            return 0;
	} 
	free_denotation(data,1);
	str = rest;
    }
    return 1;
}

extern OBJ _AFile_AxwriteString(OBJ file,OBJ str,OBJ unit) {
    FILE * f = unpack_file(file); 
    if ( write_string(f,str) ){
	return_okay_nil;
    } else {
        return_unix_failure(errno);
    }
}
	
extern OBJ _AFile_AxwriteLine(OBJ file,OBJ str,OBJ unit) {
    FILE * f = unpack_file(file); 
    if ( write_string(f,str) && putc('\n',f) != EOF ){
	return_okay_nil;
    } else {
        return_unix_failure(errno);
    }
}


/* -----------------------------------------------------------------------
 * initialization
 */

static init_const_AFile()
{
 init_ANat();
 init_AInt();
 init_AChar();
 init_AString();
 init_ACom();
 init_AUnixFailures();
 __AFile_AstdIn = pack_file(stdin);
 __AFile_AstdOut = pack_file(stdout);
 __AFile_AstdErr = pack_file(stderr);
}
