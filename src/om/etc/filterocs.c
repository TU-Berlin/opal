/* Filtering ocs output */
/* $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/om/etc/filterocs.c,v 1.1.1.1 1998-06-16 15:59:54 wg Exp $ */

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
      fputs("Noting to be done for `", stdout);
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
