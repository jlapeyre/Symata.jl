## Documentation not attached to an Symata Head with apprules

@sjdoc All """
   All

a symbol used in options.
"""

@sjdoc Regex """
    r\"expr\"

create a regular expression (PCRE). This is a Julia `DataType`.
"""

@sjdoc macros """
macros

If Symata encounters a macro call in input, it first Julia-evaluates all the
arguments then Julia-evaluates the macro and inserts it into the Symata expression
tree. For instance, big numbers and regular expressions are constructed this way.
"""

@sjdoc ans """
    ans

holds the result of the most recent evaluation.
"""

@sjdoc Sequence """
    Sequence

represents a sequence of arguments to be spliced into function arguments.
"""

@sjexamp(Sequence,
         ("f(a,b,Sequence(c,d),e,g)", "f(a,b,c,d,e,g)"))


@sjdoc bf """
    bf"str"

convert string `str` to a `BigFloat`.
"""

@sjdoc BF """
 BF(str)

converts `str` to type `BigFloat`.
"""

@sjdoc bi """
    bi"str"

convert string `str` to type `BigInt`.
"""

@sjdoc BI "
    bI(n)

converts `n` type `BigInt`.
"

@sjdoc Unicode_Input """
Unicode characters may be input at the Symata REPL just as they are in Julia. Eg. \\alpha [TAB] will enter the greek letter α.
"""

@sjseealso_group(ReturnSymPy,ToSymPy,ToSymata,ShowSymPyDocs)

