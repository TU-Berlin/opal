/* Filtering ocs output */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]){
  char line[512];
  while (fgets(line, sizeof(line), stdin) != NULL){
    if (strstr(line,"GlobalRules") != NULL &&
           strstr(line,"o such file") != NULL){
      /* ignore */
    } else 
    if (strstr(line,"othing to be done for") != NULL){
      /* print abstract target */
      fputs("Nothing to be done for `", stdout);
      fputs(argv[1], stdout);
      fputs("'.\n", stdout);
      fflush(stdout);
    } else {
      fputs(line, stdout);
      fflush(stdout);
    }
  }
  exit(0);
}
