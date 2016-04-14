## Documentation not attached to an SJulia Head with apprules

@sjdoc All "
All is a symbol used in options.
"

@sjdoc Regex "
r\"expr\" creates a regular expression (PCRE). This is a Julia DataType.
"

@sjdoc macros "
If SJulia encounters a macro call in input, it first Julia-evaluates all the
arguments then Julia-evaluates the macro and inserts it into the SJulia expression
tree. For instance, big numbers and regular expressions are constructed this way.
"

@sjdoc ans "
'ans' holds the result of the most recent evaluation.
"

@sjdoc Sequence "
Sequence represents a sequence of arguments to be spliced into function arguments.
"

@sjexamp(Sequence,
         ("f(a,b,Sequence(c,d),e,g)", "f(a,b,c,d,e,g)"))


@sjdoc bf "
 bf\"str\" converts string str to a BigFloat
"

@sjdoc BF "
 bf\"str\" converts string str to a BigFloat
"

@sjdoc bi "
 bf\"str\" converts string str to a BigInt
"

@sjdoc BI "
 bf\"str\" converts string str to a BigInt
"

