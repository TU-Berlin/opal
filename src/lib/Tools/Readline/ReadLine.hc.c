/* hand-coded implementation part of ReadLine */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "readline/readline.h"

#include "Nat.h"
#include "Seq.h"
#include "Com.h"

extern char **start_argv;	/* defined in _ostart.c */


#define evaluate_command _ACom_AexecCom


static char ** no_completion(), ** opal_completion(),
	     * null_generator(), * opal_generator();

static OBJ EndOfInput;
static OBJ Completer, Completions;


extern OBJ _AReadLine_AReadLine(OBJ Prompt,OBJ Unit) /* ReadLine */ {

    char prompt[40];
    char * line;

    rl_readline_name = start_argv[0];

    rl_attempted_completion_function = (CPPFunction *)no_completion;

    get_denotation(Prompt, prompt, sizeof(prompt));
    line = readline(prompt);

    if (line) {
	OBJ Res = make_denotation(line);
	if (*line) add_history(line);
	free(line);
	return_okay(Res);
    } else {
	return_fail(EndOfInput);
    }
}

extern OBJ _AReadLine_AReadLineCompleter(OBJ Prompt,OBJ Compl,OBJ Unit) {

    char prompt[40];
    char * line;

    rl_readline_name = start_argv[0];

    rl_attempted_completion_function = (CPPFunction *)opal_completion;
    Completer = Compl;

    get_denotation(Prompt, prompt, sizeof(prompt));
    line = readline(prompt);

    free_closure(Completer,1);

    if (line) {
	OBJ Res = make_denotation(line);
	if (*line) add_history(line);
	free(line);
	return_okay(Res);
    } else {
	return_fail(EndOfInput);
    }
}

static char ** no_completion(char * word, int start, int end){
    return completion_matches(word, null_generator);
}

static char ** opal_completion(char * word, int start, int end){
    char ** matches;
    
    OBJ Line = make_denotation(rl_line_buffer);
    OBJ Word = make_denotation(word);
    OBJ Com, Ans;

    /* compute completion command */
    copy_closure(Completer, 1);
    Com = EVAL4(Completer, Word, Line, pack_nat(start), pack_nat(end));

    /* evaluate completion command. */
    Ans = evaluate_command(Com);

    copy_some(Ans, 1);
    if (unpack_bool(_ACom_Afail_(Ans))){
    
	free_some(Ans, 1);

	/* let the default file name completer take over */
	return NULL;

    } else {

	Completions = _ACom_Adata(Ans);

	return completion_matches(word, opal_generator);
    }
}

static char * opal_generator(char *text, int state){
    OBJ Den; char * name;

    if (!state && Completions == __ASeq_Slg){
	/* first attempt and empty. seems that readLine switches
	   automatically to file name completion if we dont give it
	   at least what we get ... */
	name = (char *)malloc(strlen(text)+1);
	strcpy(name, text);
	return name;
    }

    if (Completions != __ASeq_Slg){

	copy_some(Completions,1);
	Den = _ASeq_Aft(Completions);
	Completions = _ASeq_Art(Completions);

	name = (char *)malloc(leng_denotation(Den)+1);
	
	get_denotation(Den, name, leng_denotation(Den)+1);
	return name;

    } else {
	return NULL;
    }
}

static char * null_generator(char * text, int state){
    char * name;
    if (!state){
	name = (char *)malloc(strlen(text)+1);
	strcpy(name, text);
	return name;
    }
    return NULL;
}


static init_const_AReadLine()
{
  init_ANat();
  init_ASeq();
  init_ACom();
  EndOfInput = _ACom_Afail(make_denotation("end of input in readline"));
}
