/* Filtering ocs output */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-17 11:01:14 $ ($Revision: 1.2 $)
*/
#include <string.h>
#include <stdio.h>

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
