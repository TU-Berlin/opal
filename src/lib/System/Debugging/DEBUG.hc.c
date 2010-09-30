/* hand-coded implementation part of DEBUG */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/
#include <unixconfig.h>

#include "String.h"
#include "Nat.h"

extern int start_argc;
extern char ** start_argv;

extern OBJ _ADEBUG_AFIELD_(OBJ field) /* FIELD? */ {
    int i;
    get_denotation(field,charbuf,sizeof(charbuf));
    for (i = 1; i < start_argc; i++){
        if (strcmp(start_argv[i],charbuf) == 0)
            return pack_clean_bool(1);
    }
    return pack_clean_bool(0);
}

extern OBJ _ADEBUG_Aprint(OBJ x,OBJ str) /* print */ {
    fprintf(stderr,"DEBUG PRINT:\n");
    while (!is_empty_string(str)) {
	OBJ data,rest; NAT start,leng;
        unpack_chunck_string(str,start,data,rest);
        leng = leng_denotation(data) - start;
        fwrite(data_denotation(data) + start,sizeof(char),leng,stderr);
        free_denotation(data,1);
	str = rest;
    }
    fputc('\n',stderr);
    return x;
}

static init_const_ADEBUG()
{}
