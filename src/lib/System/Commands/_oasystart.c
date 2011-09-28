
/* subject: generic startup file for OPAL standalone programs
 * author:  wg 7-92, 5-95
 * version: $_Header$
 */

/* expects

	command	#defined to the main command
	init	#defined to the main commands structures init entry

*/

#include "BUILTIN.h"
#include "Com.oc.h"
#include <stdlib.h>

extern int (*ocs_top_exec)(OBJ);/* how to execute the top-level command;
			           initialized depending on the used
			           librarys. */

extern OBJ command;
extern void init();


extern int start_argc;
extern char ** start_argv;
extern char ** start_env;


#ifdef _MEMSTAT_
extern void _endInitStat();
extern void _printStat();
#endif

#ifdef _ENVIRON_
extern char ** environ;
main(int argc, char** argv){
#else
main(int argc, char** argv, char** environ){
#endif

    OBJ ans;

    start_argc = argc; start_argv = argv;
    start_env = environ;

    init_ABUILTIN();	
    init();

    #ifdef _PRINTMEMSTAT_
       _endInitStat();
       #ifdef _sun4_
       { extern int on_exit();
	 on_exit(_printStat,(void*)0);
       }
       #else
       atexit(_printStat);
       #endif
    #endif

    copy_some(command, 1);
    return (*ocs_top_exec)(command);

    ocs_exit(0);
}
