%{
  #include <string.h>

  char lastide[128] = {0,};
  int  nestcomment = 0;
  int  linecomment = 0;
  int  nestinst    = 0;
  int  denotation  = 0;

  void checklexem();
%}
LETGIT [a-zA-Z0-9]+
GRAPH [\!#$%&*+-/;<=>?@^`{|}~.:]+
%%
({GRAPH}|{LETGIT})(_({GRAPH}|{LETGIT})?)* checklexem();
[(),]			   lastide[0] = 0;  
[[]			   { if (!denotation && !nestcomment && !linecomment)
				nestinst++; }
]			   { if (!denotation && !nestcomment && !linecomment)
				nestinst--; }
\n			   linecomment = 0;
["]["]			   lastide[0] = 0;
["]			   { if (!nestcomment && !linecomment) 
			       denotation = denotation? 0 : 1; lastide[0] = 0;}
.			   ;
%%
  void checklexem(){
      if (denotation){
	;
      } else
      if (nestcomment){
        if (!strcmp(yytext,"*/")) 
	  nestcomment--;
        else
        if (!strcmp(yytext,"/*"))
	  nestcomment++;
      } else  
      if (!linecomment) {
        if (!strcmp(yytext,"--"))
	  linecomment = 1;
        else
        if (!strcmp(yytext,"/*"))
	  nestcomment = 1;
        else
        if (!nestinst){
	  if (!strcmp(yytext,"COMPLETELY") || !strcmp(yytext,"ONLY")){
	    if (lastide[0]){
	      fputs(lastide,stdout); fputc('\n',stdout);
	      lastide[0] = 0;
	    }
	  } else {
	    int i;
	    for (i = 0; i < 127 && yytext[i]; i++) lastide[i] = yytext[i];
	    lastide[i] = 0;
	  }
	}
      }
  }
