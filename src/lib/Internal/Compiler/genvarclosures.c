/* generate foreign closure tables and functions
*/

/* $Id$
*/

#include <string.h>
#include <stdio.h>

#include "BUILTIN.h"	/* get MAXRANK  */

int main(int argc, char *argv[]){
  FILE * file; 
  int a,i,j;

  if ( (file = fopen("_varclosures.c","w")) == NULL ){
      perror("gencalls");
      exit(1);
  }
  fprintf(file,"#include \"BUILTIN.h\"\n\n");

  /* the variadic method tables */
  fprintf(file,"static OBJ passv[MAXRANK];\n");
  for (a = 1; a <= NUMVARMETHODTABLES; a++){
      fprintf(file,
       "static OBJ (*var_method_vec%d)(OBJ Param, int argc, OBJ argv[]);\n",
	      a);
      for (i = 1; i <= MAXRANK; i++){
	  fprintf(file, "static OBJ var_method_%d_%d(OBJ Param",a,i);
	  for (j = 1; j <= i; j++){
	      fprintf(file,",OBJ Arg%d", j);
	  }
	  fprintf(file,"){\n");
	  for (j = 1; j <= i; j++){
	      fprintf(file,"  passv[%d] = Arg%d;\n", j-1, j);
	  }
	  fprintf(file,
		  "  return (*var_method_vec%d)(Param, %d, passv);\n", 
		  a, i);
	  fprintf(file,"}\n");
      }
      fprintf(file, "static CODE var_method_table%d[] = {\n", a);
      for (i = 1; i <= MAXRANK; i++){
	  fprintf(file,"  (CODE) var_method_%d_%d,\n",a,i);
      }
      fprintf(file,"};\n\n");
  }
  fprintf(file,"static int free_table = 0;\n");
  fprintf(file, 
"CODE *ocs_alloc_var_method_table(OBJ (*method)(OBJ, int argc, OBJ argv[])){\n"
"  if (free_table >= NUMVARMETHODTABLES)\n"
"      HLT(\"<RUNTIME>: no more variadic method tables\");\n"
"  free_table++;\n"
"  switch(free_table){\n");
  for (a = 1; a <= NUMVARMETHODTABLES; a++){
      fprintf(file,
"    case %d: var_method_vec%d = method;\n"
"             return var_method_table%d;\n",
	      a,a,a);
  }
  fprintf(file,
"  }\n"
"}\n");


  /* the variadic closure evaluator */
  for (i = 1; i <= MAXRANK; i++){
      fprintf(file, 
	      "static OBJ var_eval_%d(OBJ Clos, OBJ argv[]){\n", i);
      fprintf(file,
	      "  return (* (OBJ (*)(OBJ");
      for (j = 1; j <= i; j++){
	  fprintf(file,",\n    OBJ");
      }
      fprintf(file, "))METHOD(Clos,%d))(Clos",i);
      for (j = 1; j <= i; j++){
	  fprintf(file,",\n    argv[%d]", j-1);
      }
      fprintf(file,");\n}\n");
  }
  fprintf(file,
	  "static OBJ (*var_eval_table[])(OBJ Clos, OBJ argv[]) = {\n");
  for (i = 1; i <= MAXRANK; i++){
      fprintf(file, "  var_eval_%d,\n", i);
  }
  fprintf(file,"};\n");
  fprintf(file,
	  "OBJ ocs_var_eval(OBJ Clos, int argc, OBJ argv[]){\n"
	  "  return (*var_eval_table[argc-1])(Clos, argv);\n"
	  "}");

  fclose(file);
  exit(0);
}

