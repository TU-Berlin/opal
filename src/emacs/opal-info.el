;;; information about an Opal source

(require 'opal-browser)
(provide 'opal-info)

;; set up mouse

(defun opal-info-keymap ()
;  (define-key opal-mode-map [(control button3)] 'opal-info-mouse)
  (define-key opal-mode-map [(f1)] 'opal-info-key)
)

;; information on keywords
(setq opal-info-match-list '( 
  
;; AS
("^AS$" .
"DEFINITIONS Pattern

Var AS Pattern

AS may be used on the left-hand side of a pattern-based 
definition. Introduces Var as variable of the actually
matched pattern on the right-hand side of the equation."
)
;; COMPLETELY / IMPORT / ONLY
("^\\(COMPLETELY\\|IMPORT\\|ONLY\\)$" .
"Syntax: IMPORT Structure1 COMPLETELY
                          Structure2 ONLY name1 name2 ...
        ...

Import all or only the named items from the given
structures. Several imports of the same structure
are merged. A name need not be imported before
its application.")
;; DATA / TYPE
("^\\(DATA\\|TYPE\\)$" .
"DECLARATION AND DEFINITION OF TYPES

TYPE typename == c1(sel11: t11, sel12: t12, ...)
                         c2(sel21: t21, sel22: t22, ...)
			 ...

DATA typename == c1(sel11: t11, sel12: t12, ...)
                         c2(sel21: t21, sel22: t22, ...)
			 ...

TYPE declares a free type, DATA defines a free type.
DATA may only be used in the IMPLEMENTATION part.
The type declared by TYPE must be implemented by a DATA
definition.

The declaration/definition additionally contains
- constructors ci: ti1 ** ti2 ** ... -> typename;
- discriminators ci? : typename -> bool
- selectors selij: typename -> tij.
")
;; DEF
("^DEF$" .
"DEFINITION OF FUNCTIONS

DEF c == Rhs

- or -

DEF f(Pattern1) == Rhs1
DEF f(Pattern2) == Rhs2
      ...

Define function f, which must be declared in the same structure.
The type of the rhs must match the type on the left-hand side
of the equation. If present, patterns consist of variables, 
constructors and underline. Variables are available in the 
right-hand side expression. Patterns may overlap (in which 
case the compiler resolves the ambiguity arbitrarily) and need 
not be exhaustive.

Functions may only be defined in the IMPLEMENTATION part.")


;; FUN
("^FUN$" .
 "DECLARATION OF FUNCTIONS

FUN Name1 Name2 ... : Type

Declare functions Name# to be of type Type.")

;; IF/THEN/ELSE/FI
  ("^\\(IF\\|THEN\\|ELSE\\|FI\\)$" .
"EXPRESSIONS: Conditional

IF Cond1 THEN Expr1
IF Cond2 THEN Expr2
...
[ELSE Expr-Else]
FI

Cond# are boolean expressions. If none of the 
conditions are true, Expr-Else is evaluated;
if no ELSE-clause is given, the Conditional
is undefined.
")

;; IMPLEMENTATION / SIGNATURE
("^\\(IMPLEMENTATION\\|SIGNATURE\\)$" .
"STRUCTURE PARTS

SIGNATURE StructureName
IMPLEMENTATION StructureName

Except for comments, each Opal file must declare at the very 
beginning the structure part and structure name. The SIGNATURE 
is the interface part of the structure and contains 
declarations; the IMPLEMENTATION contains the implementations 
and may introduce hidden objects.

StructureName must match the name of the file (StructureName.sign 
or StructureName.impl). It is strongly recommended to use only
alphanumeric characters.

The StructureName may be followed by a comma separated list
of parameter names in square brackets.")

;; LET/IN/WHERE
("^\\(LET\\|IN\\|WHERE\\)$" .
 "EXPRESSIONS:  Block

LET Var1 == Expr1
       Var2 == Expr2
    ... 
IN Expression

- or -

Expression WHERE Var1 == Expr1
                         Var2 == Expr2
                  ...

Declare local variables Var#, which may be used with Expression.
Expr# is evaluated before Expression (unless the optimizer
detects that Var# is not referred to in Expression). Equations
need not be ordered, but may not be cyclical.

If Expr# is a tuple type, the left-hand side of the equation
 must also be a tuple:
(Var#1, Var#2, ...) == Expr#
A variable which is not used in Expression, may be replaced by `_'.

LET binds tighter, than WHERE, so
LET ... IN Expression WHERE ... is parsed as 
(LET ... IN Expression) WHERE ..."
)

;; OTHERWISE
("^OTHERWISE$" .
"EXPRESSION: Conditionals

IF Cond1 THEN Expr1
OTHERWISE
IF Cond2 THEN Expr2
...
[ELSE Expr-Else]
FI

The usage of OTHERWISE forces the conditions before the 
OTHERWISE to be evaluated first.")

;; SORT
("^SORT$" .
"SORT DECLARATION

SORT name1 name2 ...

Declares name# as sort names. The sorts must be implemented,
or be parameters.

FULL NAMES

name:SORT

`:SORT' restricts the interpretation of `name',
e.g. in an IMPORT list.")
;; ==
("^==$" .
 "FUNCTION DEFINITION

DEF lhs == rhs

EXPRESSION: Block

LET Var1 == Expr1
    Var2 == Expr2
    ...
IN
Expression

`==' serves to separate left-hand side and right-hand side 
of an equation; either a definitional equation or an 
equation in a LET- or a WHERE-block.")

;; ** ->
("^\\(\\*\\*\\|->\\)$" .
"TYPE CONSTRUCTION

t1 ** t2 ** ... ** tn
t1 -> t2

Basic types are introduced by SORT or TYPE declarations
or DATA definitions. These types may be combined by
`**' to form the product and `->' to form the function type.
Note that product binds tighter than function, and the arrow
associates to the right, so be sure to use brackets
for function types as arguments.
")
;; :
("^\\(:\\|[\\|]\\|`\\)$" .
"OPAL NAMES

full-name -> name origin instance kind

origin -> ' structure
instance -> [ param1, param2, ..., paramn ]
kind -> : SORT 
      | : functionality

Full names consist of name, origin, instance and kind. 
Origin, instance and kind may be omitted. In this case,
the compiler deduces these parts.

Note that ` (backquote) , (comma) and square brackets 
are delimiters and can not be part of an identifier, 
whereas : (colon) may be part of an identifier.
")
;; \\ .
("^\\(\\\\\\\\\\|\\.\\)$" .
 "EXPRESSION: Abstraction

\\\\ Var1, Var2, ..., Varn . Expression

`\\\\' is the Opal representation of lambda.
Abstracts the Expression over variables Var1, ...
The variable list may be empty, which serves to
make the expression a lazy expression.

A single dot serves to separate the variable list
from the expression. Only the first dot after \\\\
is treated as a keyword, every other dot is a
single identifier.
") 
;; /$ $/
("^\\(/\\$\\|\\$/\\)$" .
"PRAGMAS

/$ Item1 Item2 ... $/

Item# is either a text or a list of Opal names 
in square brackets. Pragmas serve to control
the translation of Opal structures. Apart from
the pragmas mentioned in the documentation,
the precise meaning is subject to change without 
notice.
")
;; ( , )
("^\\((\\|,\\|)\\)$" .
"EXPRESSION: Tuple

( Expr1 , Expr2 , ..., ExprN )

Construct the tuple from Expr#

EXPRESSION: Application (prefix)

f ( Arg1, Arg2, ..., ArgN )
  
Apply function f to given arguments. Argument list may be emtpy.

TYPE CONSTRUCTION

( type )
()

First form overrules bindings. Second form serves to 
define lazy functions.
")
;; ANDIF ORIF
("^\\(ANDIF\\|ORIF\\)$" .
"EXPRESSION: Condition

IF Cond1 ANDIF Cond2 ANDIF ... ANDIF CondN THEN ...
IF Cond1 ORIF Cond2 ORIF ... ORIF CondN THEN ...

Cond# must be boolean expressions, which are evaluated 
from left to right until one expressions yields false 
or true resp. This short-circuit effect is implemented
by syntactic transformation. 

These short-circuit conditions should only be used
for semantical reasons. The optimizer will 
optimize the evaluation of and'PREDEF_BOOL and or'PREDEF_BOOL.
")
;; /* */ --
("^\\(/\\*\\|\\*/\\|--\\)$" .
"COMMENTS

/* comment */
/* %documentation */
-- line comment
-- %line documentation

Opal knows nested comments `/* ... */' and line comments.
Comments may be nested. Line comments have precedence about
nested comments.

If the first character of a comment is a `%' (percent) 
character, the documentation tool DOSFOP will recognize
these comments as documentation. (See the manual.)
")
;; EXTERNAL INTERNAL PROPERTIES
("^\\(EXTERNAL\\|INTERNAL\\|PROPERTIES\\)$" .
"OPAL PROPERTY LANGUAGE

These keywords used to introduce new structure parts
dedicated to specification. They are still reserved
but currently unused.
")
;; LAW
("^LAW$" .
"OPAL PROPERTY LANGUAGE

LAW [ Name == ] Formula

Introduces a Formula which is declared to be valid
within the structure. The law need not be given a 
name, but this is strongly discouraged.
")
;; ALL EX
("^\\(ALL\\|EX\\)$" .
"OPAL PROPERTY LANGUAGE

ALL Var1 Var2 ... VarN . Formula
EX Var1 Var2 ... VarN . Formula

Universal and existential quantification over formula. 
The first dot after the quantifier is recognized 
as keyword.
")
;; AND NOT OR ==> <=>
("^\\(AND\\|NOT\\|OR\\|==>\\|<=>\\)$" .
"OPAL PROPERTY LANGUAGE

NOT Formula
Formula1 AND Formula2
Formula1 OR Formula2
Formula1 ==> Formula2
Formula1 <=> Formula2

Logical connectives for formulae, given in 
decreasing order of precedence.
")
;; DFD === <<=
("^\\(DFD\\|===\\|<<=\\)$" .
"OPAL PROPERTY LANGUAGE

DFD Expression
Expression1 === Expression2
Expression1 <<= Expression2
Expression

Basic formulas:
- valid, iff Expression is defined
- valid, iff both expressions are equal
- valid, iff Expression1 is less defined 
     than Expression2
- abbreviation for Expression === true
")


) ; close list
) ; close setq

(defconst opal-info-NA "Sorry, no information available") 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-info-mouse (event)   
  (interactive "e")
  (mouse-set-point event)
  (opal-info-show-info (opal-current-ide))
)

(defun opal-info-key ()  
  (interactive "")
  (opal-info-show-info (opal-current-ide))
)

(defun opal-info-nil () )
  
(defun opal-info-show-info (item)
  "pop up a dialog box with information about the specified item"
  (interactive "sInfo on:")

  (let (txt)
    (setq txt (opal-info-try-keyword item))
    (if (not txt)
	(if (browser-test-interOpal)
	    (setq txt (opal-info-get-browser item))
	  (setq txt opal-info-NA)
	  )
      ) 
  (setq txt (concat "OPAL Information about `" item "'\n\n" txt))
  (popup-dialog-box (list txt ["OK" 'opal-info-nil t]))
  )
  )

(defun opal-info-try-keyword (item)
  "return nil, if no information for item is found or a string to display"
  (let (a m)
    (setq a opal-info-match-list)
    (setq m nil)
    (while a
      (if (string-match (car (car a)) item)
	  (progn
	    (setq m (cdr (car a)))
	    (setq a nil)
	    )
	(setq a (cdr a))
	)
      )
    m
    )
  )
		      
      
;; get information from browser
;; copied from opal-browser.el
(defun opal-info-get-browser (item)
  "get information from browser about item"

  (set-browser-position-and-name)
  (let (b fn cont cmp start end)
    (setq fn (buffer-file-name))
    (setq b (get-buffer-create "*browse*"))
    (save-excursion
      (set-buffer b)
      (toggle-read-only 0)
      (delete-region (point-min) (point-max))
      (call-process browser-exe nil b nil
		    "Command" "11"
		    "Level" browse-level
		    "Filename" fn
		    "Name" item
		    "Begin" browser-pos-begin-line browser-pos-begin-col
		    "End" browser-pos-end-line browser-pos-end-col
		    "Option" "v1"
		    "Option" "nameNotPos")
      (goto-char (point-min))
      (while (search-forward-regexp "^BROWSER.*$" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	)      
      (goto-char (point-min)) 
      (delete-non-matching-lines ":")
      (goto-char (point-min))
      (while (search-forward-regexp "%%\\(global\\|local\\).*$" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	)
      (sort-lines nil (point-min)(point-max))
      (goto-char (point-min))
      (setq cont t)
      (while cont
	(beginning-of-line)
	(setq start (point))
	(end-of-line)
	(setq end (point))
	(setq cmp (buffer-substring start end))
	(if (= 0 (forward-line 1))
	    (progn
	      (delete-matching-lines (regexp-quote cmp))
	      )
	  (setq cont nil)
	  )
	)
      
      (buffer-substring nil nil b)
      )
    )
  )
