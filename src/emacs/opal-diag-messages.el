;; ----------------------------------------------------------------------

(provide `opal-diag-messages)


;; the data for the extended diagnostic information

(setq opal-diag-extended-doku-alist 
	'(
;;; HINTS ;;;
("assumed right associativity of \\(.*\\)\\('.*\\)" .
          "There is an expression containing several occurrences of \\1\\2 
as infixes. These infixes are parsed in a right associative way, i.e. 
`a \\1 b \\1 c' is parsed as `a \\1 (b \\1 c)'. 

This usually is what is wanted. Two of the few exceptions are the arithmetic 
operators `-' and `/'. If you do not want right associative parsing, you must 
provide parentheses.")

("local name \\(.*\\) is not used" .
	   "This hint may be caused by a typing error, better check the correct 
spelling of `\\1'. 

If you actually do not want to use the variable `\\1', better rename 
it to `_' and the compiler will not complain again.")

("unused operation \\(.*\\)" .
            "The operation \\1 was declared at the indicated place but never 
used or exported for further use. 

This can happen in a DATA declaration, if you did not export the corresponding 
free type. Otherwise you can remove the declaration and the definition of 
\\1 without affecting the usefulness of this structure.

Note that you cannot use this function in the oasys evaluator, because the optimizer will remove it. The simplest way to keep the optimizer from removing it is to export this function.")

("unused sort \\(.*\\)" .
             "The sort \\1 was declared at the indicated place but never used nor exported.")

("the following names are imported (Gottfried's rule): " .
             "The name in the import list is ambiguous. All of the following functions match this name and are imported into the current structure.

Normally, ambiguity causes an error, but in this case all matching functions are selected. This may cause ambiguities in expressions you were not aware of.

Gottfried Egger is a former member of the Opal Group who suggested importing all matching names instead reporting an error.")

("unused pattern variable \\(.*\\)" .
	   "The variable `\\1' on the left-side pattern of the definition is never referenced in the expression on the right side. If this is deliberate, you should change the pattern variable to `_' and the compiler will not generate this hint again. Otherwise, you most probably misspelled the name on the right hand side (and probably some error messages follow this hint).")

;;; WARNINGS ;;;
("ambiguous patterns at \\(.*\\) for \\(.*\\)" .
          "The equations at \\2 all apply to the pattern \\1. The compiler will chose one of them arbitrarily. This does not matter if the right hand side is equal in these cases. If you cannot figure out, why the pattern \\1 applies to the equations at \\2, check that all constructors are actually imported - if they are not, the constructor names will be variable names!")

("imported file '\\(.*\\)/\\(.*\\)\\.extp\\.inter' not found: only 'signature export' imported" .
          "Imports in property parts import from the corresponding property part. The abstract syntax of the external property part of structure \\2 was not found, therefore only items from the signature part are actually imported.

You most probably forgot to compile the property parts of \\2.")

("\\(.*\\)\\('.*\\)\\(:SORT\\) is not implemented" .
           "The indicated sort \\1 is declared but is not implemented. This will result in a runtime error if a function is called to construct an element of this sort."
)

("\\(.*[a-zA-Z0-9]\\)\\('.*\\):\\(.*\\) is not implemented" .
           "The indicated function \\1: \\3 is declared but is not implemented. This will result in a runtime error if the function is called."
)

("\\(.*\\)\\('.*\\):\\(.*\\) is not implemented" .
           "The indicated function \\1 : \\3 is declared but is not implemented. This will result in a runtime error if the function is called."
)

("unknown escape `.\\(.\\)' replaced by `\\(.\\)'" .
            "There is a fixed set of escape sequences to denote special characters. `\\\\\\1' is not one of those seqences.

Allowed sequences are:

\\\\a (alert), \\\\b (backspace), \\\\f (formfeed), \\\\n (newline), \\\\r (carriage return), \\\\t (tabulator), \\\\v (vertical tabulator), \\\\\\\\ (literal backslash), \\\\? (question mark), \\\\' (single quote), \\\\\" (double quote)
\\\\x followed by an arbitrary number of hexadecimal digits, denotes the character at that code position, \\\\ followed by one, two or three octal digits is an alternative to denote a certain character.

\\\\u followed by one, two, three or four hexadecimal digits (unicode escape), likewise denotes the character at that code position. Note that unicode escapes are processed outside of denotation constants, too, and that this expansion takes place before the expansion of the other characters. ")

("pattern variable \\(.*\\) hides constant \\(.*\\)'\\(.*\\):\\(.*\\)" .
            "The pattern variable \\1 has the same name and type as a global constant from structure \\3. Within the function definition, \\1 will designate the pattern variable and not the global constant.

Best practice is to rename the pattern variable in order not to confuse the use of \\1 as pattern variable and as global constant in this structure.")

;;; ERRORS ;;;
("ambiguous identification: cannot select from" .
           "The compiler cannot decide which of the functions or typings mentioned should be applied here. You must annotate the function or the variable. This message often comes several times, once for every subterm. In this case, you should annotate the innermost subterm and try again.")

("ambiguous identification: functionality of local name \\(.*\\):\\([^]]*\\)\\(\\[.*\\) ambiguous" .
           "The proper instantiation of the local variable \\1 cannot be deduced in this context. The 'var(xx)' entries in \\2\\3 show which parts cannot be deduced; usually functions must be supplied. 

Either import \\2 instantiated or supply the instantiation at the application.")

("ambiguous identification: functionality of local name \\(.*\\):\\(.*\\) ambiguous" .
           "The proper functionality of the local variable \\1 cannot be deduced in this context. The 'var(xx)' entries in \\2 show which parts cannot be deduced.")

("ambiguous Identification of \\(.*\\): instantiation missing" .
           "The instantiation of \\1 cannot be deduced from the context. Context consists of of imports and declarations only.")

("ambiguous Identification of instantiation" .
           "The instantiation cannot be deduced from the context. Context consists of of imports and declarations only. (?)")

("ambiguous Identification of \\(.*\\)" .
           "The instantiation of \\1 cannot be deduced from the context. Context consists of of imports and declarations only. (?)")

("ambiguous identification: uninstantiated global name \\(.*\\)'\\([^]]*\\)\\(\\[.*\\)\\(:.*\\)" .
           "The proper instantiation of imported name \\1 cannot be deduced in this context. The 'var(xx)' entries in \\2\\3 show which parts cannot be deduced; usually functions must be supplied. Either import \\2 instantiated or supply the instantiation at the application.")

("ambiguous infix application:\\(.*\\)" .
           "The infix application cannot be resolved, because there are several type correct possibilities. The error message contains a scheme of the offending expression, where dots stand for subexpressions and the functions whose infix application cannot be resolved are marked. Add parentheses and try again.")

("Application of _ not allowed here" .
           "The use of a section (?) is not allowed here. Check if all functions applied in a section are imported.")

("\\(`.*'\\) : codes above \\\\u00FF not supported.*" .
           "Though unicode escapes allow the notation of 65536 characters, the current OPAL scanner only supports the first 256.")

("compound object: not allowed for operations (and local names)" .
           "You may not declare a function or a local variable to be of a product type. 

If this error occurs in a function declaration, the easiest explanation is that you typed '*' instead of '**', but you may also have forgotten the codomain of the functionality, and check the correct instantiation of the sorts in the domain, too. 

If you really need to have a compound object here, use pair, triple or quadruple from the Bibliotheca Opalica.")

("direct cyclic initialization: \\(.*\\)" .
           "The constant \\1 is defined in terms of itself, which is not allowed. This error is detected in a very late stage of the compilation, and the position mey therefore be wrong.

All constants are evaluated once, when the program is started. Therefore this error occurs even if the constant is not applied. You may use the empty tuple () in the domain of the functionality to delay evaluation.")

("duplicate implementation: \\(.*\\)\\('.*\\)\\(:.*\\) already implemented at \\(.*\\)" .
           "The function \\1\\3 is implemented by a DATA item at \\4 and cannot be further implemented.")

("duplicate or overloaded local name \\(.*\\) (also added at \\(<[0-9]+,[0-9]+>\\))" .
           "Local names cannot be overloaded. The name \\1 is already declared as a local variable at \\2. Check for a typing error, if it is not an error, you have to rename one occurrence of \\1.")

("Expected was >> \\(.*\\) << as Structure Name" .
    "The name of a structure and the base name of the file where it is located must be the same.")

("Expected was \\(`.*'\\) instead of \\(`.*'\\)" .
           "The compiler expected \\1 and found instead \\2. Perhaps you forgot \\1, perhaps \\2 is at the wrong place.")

("Expected was `\\(.*\\)'" .
           "The compiler did not find a \\1 token before the next item. Perhaps you forgot it, but probably you need to look closer.")

("Found unexpected Ide\"\\(.*\\)\"".
           "The identifier `\\1' is not allowed in this place. 

Check the preceding text carefully for typing errors and missing keywords. Some expressions may not be used without brackets as infix arguments.")

("Found unexpected \\(.*\\)".
           "The token `\\1' is not allowed in this place. 

Check the preceding text carefully for typing errors and missing keywords. Some expressions may not be used without brackets as infix arguments.")

("illegal uninstantiated import of '\\(.*\\)'" .
            "The structure \\1 may not imported unparameterized in this structure part. You must supply the parameters in the IMPORT item.")

("import of \\(.*\\) needs import of \\(.* and .*\\)" .
           "The object \\1 somehow uses the the objects \\2, most often \\1 denotes a function, in the functionality of which the types \\2 are used. If you can't figure out why \\2 are needed, remember that all objects with the same name are imported, types and functions (constructors, selectors, discriminators) alike. 

If you want to restrict the import to a particular object, add the kind to the name (\\1:SORT or \\1:functionality).")

("import of \\(.*\\)'\\(.*\\):\\(.*\\) needs import of \\(.*\\)'\\(.*\\)" .
           "The object \\1 somehow uses the the object \\4, most often \\1 denotes a function, in the functionality of which the type \\4 is used. If you can't figure out why \\4 is needed, remember that all objects with the same name are imported, types and functions (constructors, selectors, discriminators) alike. 

If you want to restrict the import to a particular object, add the kind to the name (\\1:SORT or \\1:functionality). Otherwise add a line IMPORT \\5 ONLY \\4.")

("imported structure '\\(.*\\)' not in command line" .
           "This error can happen if you use the OPAL compiler's front end directly, or if you added an IMPORT \\1 item and saved the file after invoking ocs. In this case simply start ocs again.")

("improperly named function definition target or parameter \\(.*\\)" .
           "There is no declaration of a function \\1. 

Check for typing errors and commented declarations.")

("improperly instantiated import" .
           "The instantiation given is not correct. Check the parameters, if they all exist and are of the correct type.")

("\\(`\.*'\\) is a non existent character!" .
           "The available characters range from \\\\x0 - \\\\xFF (or \\\\0 - \\\\377 in octal notation). \\1 lies outside that range.

Note that the \\\\x escape takes _all_ following hexadecimal digits. Octal notation takes up one to three octal digits.")

("incompatible else" . 
        "The types of the `THEN' expressions (given as `guards:') and the `ELSE' expression (given as `else:') are different.")

("local name '\\(.*\\)' is used with conflicting types: '\\(.*\\)'&'\\(.*\\)'" .
           "The compiler can't decide which type the local name `\\1' should have. You probably forgot some conversion. If you cannot find the error, type the variable explicitly, either `\\1:\\2' or `\\1:\\3', so the compiler can point you to the error in the next pass.")

("Missing Infix Operator" .
           "This error results most often from a mixture of infix and postfix applications which cannot be resolved. Another reason is the omission of an infix operator. Check the expression, supply parentheses for postfix expressions.")

("Missing Operand" .
	   "Check for wrong usage of mixfix and postfix function symbols. You perhaps forgot a comma between two expressions.")

("Missing Operator" .
	   "Check for wrong usage of mixfix and postfix function symbols on the left-hand side of a definition equation. You perhaps forgot a comma between two expressions.")

("no matching free constructor for \\(.*\\)" .
           "You used \\1 in a pattern on the left-hand side of a definition. \\1 is not known to be a free constructor of the corresponding sort. Check whether you imported \\1.")

("no matching name for \\(.*\\)" .
           "The compiler knows nothing about a function (or a variable) `\\1'.

If it should be a function, you might have forgotten to import this function. If it should be a variable, you may have mistyped its name. If you did specify origin and/or instantiation check both for errors. [in instantiation ?]")

("no matching operation for \\(.*\\)" .
           "The compiler knows nothing about a function (or a variable) `\\1'.

If it should be a function, you might have forgotten to import this function. If it should be a variable, you may have mistyped its name. If you did specify origin and/or instantiation check both for errors.")

("no possible bracketing for infix found" .
           "You either forgot to import the function used as infix, or there is an error in one of the expressions involved. If you cannot figure out the error, you will have to supply brackets.\n\nYou may omit brackets in expressions like `a o b & c % d' if one of the following conditions holds: \n\n[1] The functions o & % are all the same (so the expression above looks like `a + b + c + d'). In this case, the compiler will assume right-associativity and interpret the expression as `a + (b + (c + d))'. \n\n[2] There is only one bracketing which respects the functionalities of the functions and the expressions.")

("`/\\*` not closed" .
            "The comment started at the indicated position was not closed by a corresponding `*/`. Note that '.*/' or '!*/' are valid identifiers in Opal and do not close a comment.

If you want to comment just one line, use '--' which marks everything up to the end of the line as comment.")

("`\"` not closed before end of line" .
            "Check the indicated line carefully for a forgotten \". You cannot enclose newline literally in OPAL denotations (use \\\\n for this purpose).")

("recursive let/where: direct recursive equation" .
            "The variable on the left hand side of the equation also appears on the right hand side. This produces an recursive equation which is not allowed.")

("recursive let/where: indirect recursive equation, involving equations at \\(.*\\)" .
            "The equations at \\1 form a cyclic definition of the variable(s) ob the left hand side of the current equation. This produces an indirect recursive equation which is not allowed.")

("undefined application of \\(.*\\) in only list" .
           "There is no item named \\1 in the interface. Check for typing errors, also for mistyped 'ONLY' and 'COMPLETELY' keywords. The error may also be caused by a mistyped explicit functionality.")

("undefined identification" .
	   "The function name is known to the compiler, but none of the possibilities is applicable here. This error message is followed by a (sometimes long) list of the possibilities the compiler considered, i.e. one line which contains a functionality, followed by a line which contains the actual functionalities of the arguments. Not evaluated sorts are shown as 'var(xx)', where xx is a natural number.

Check the functionalities, if the desired functionality is not among them, you may have forgotten to import the function you want to apply here.

If `delivered' is a prefix of `demanded', you may have forgotten some parameters of the type mentioned in `demanded'.")

("undefined Identification of \\(.*\\)" .
	   "If \\1 is a function, The function name `\\1' is known to the compiler, but none of the possibilities is applicable here. If \\1 is a sort, the compiler knows nothing about a sort \\1.

In case of a function, this error message is followed by a (probably long) list of the possibilities the compiler considered, i.e. one line which contains a functionality, followed by a line which contains the actual functionalities of the arguments. Not evaluated sorts are shown as 'var(xx)', where xx is a natural number.

Check the functionalities, if the desired functionality is not among them, you may have forgotten to import the function you want to apply here. If function and argument seem unifiable, check the (non-printed) origins!

In case of a sort, check whether you imported the correct instantiation.")
;; sorte nicht bekannt
("unexpected type of expression$
^    demanded:  \\(.*\\)$
^    delivered: \\(.*\\)$" .
           "In this context, only the type `\\1' is possible, but the expression turned out to have actually type `\\2'.")

("unexpected type of expression" .
           "In this context, only the type `demanded' is possible, but the expression turned out to have actually type `delivered'.")

("uninstantiated import of '\\(.*\\)'" .
           "Within SIGNATURE parts, uninstantiated imports are not allowed. You must explicitly give the instantiation of the import of structure \\1.")

("wrong number of parameters in instantiated import of '\\(.*\\)'" .
           "The structure \\1 has a different number of parameters than given here.")

("wrongly typed application" .
           "In a main error this means that there is exactly one function known with this name, but the arguments supplied have the wrong functionality. In a suberror this is one of the considered possibilities. You find below the functionality of the function and of its arguments. 

Check if you imported the desired function, keep in mind that there may be different functions with the same name and different origin. Another possibility is that you inadvertently named a function parameter like the desired function.")

("wrongly typed equation" .
           "The left and the right side of the equation have different types. You find below the types of the left- and the right-hand side of the equation.

If the left-hand side is explicitly typed, there is no possibility that the right-hand side is of that type. If the left-hand side is a tuple, the right-hand side is not of a tuple type or of a tuple type of different length.")

("wrongly typed implementation" .
           "The types of the left-hand side and the right-hand side of the equation do not match. You find below the types which the compiler computed for the left-hand side and the right-hand side of the equation. If you think that the expression is correctly typed, you should make sure that the function declaration matches your intention.")

(".*" . 
	   "Sorry, there is no help available for this diagnostic.")
	)
)

(put 'opal-diag-extended-doku-alist 'variable-documentation
     "alist of regexps matching diagnostics and associated information.")
