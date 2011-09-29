/* generate method table declarations in separate C files 
   s.t. they can be linked on demand.
*/

/* $Id$
*/

#include <string.h>
#include <stdio.h>

#include "BUILTIN.h"	/* get MAXRANK  -- thats the reason we use C ... */

void genfile(int rank, char *fn){
  FILE * file; 
  if ( (file = fopen(fn,"w")) == NULL ){
      perror("genmttab");
      exit(1);
  }
  fprintf(file,"\n#include \"BUILTIN.h\"\n");
  
  /* evaluation table */
#ifdef NeXT
  /* if we dont give an initializer the linker will not find it (????) */
  fprintf(file, "CODE _mttab_%d[(MAXRANK*(MAXRANK+1))/2] = {0};\n", rank);
  fprintf(file, "CODE _mttab_%d_l[(MAXRANK*(MAXRANK+1))/2] = {0};\n", rank);
#else
  fprintf(file, "CODE _mttab_%d[(MAXRANK*(MAXRANK+1))/2];\n", rank);
  fprintf(file, "CODE _mttab_%d_l[(MAXRANK*(MAXRANK+1))/2];\n", rank);
#endif

  fclose(file);
}

main(){
  int r; 
  for (r = 1; r <= MAXRANK; r++){
      char fn[256];
      sprintf(fn, "_mttab_%d.c", r);
      genfile(r, fn);
  }
  exit(0);
}

