## Documentation not attached to an SJulia Head with apprules

@sjdoc All "
All is a symbol used in options.
"

@sjdoc I "
I is the imaginary unit
"

@sjdoc E "
E is the base of the natural logarithm
"

@sjdoc Pi "
Pi is the trigonometric constant π.
"

@sjdoc Regex "
r\"expr\" creates a regular expression (PCRE). This is a Julia DataType.
"

@sjdoc macros "
If SJulia encounters a macro call in input, it first Julia-evaluates all the
arguments then Julia-evaluates the macro and inserts it into the SJulia expression
tree. For instance, big numbers and regular expressions are constructed this way.
"

@sjdoc Complex "
Complex(a,b) returns a complex number when a and b are Reals. This is done when the
expression is parsed, so it is much faster than 'a + I*b'.
"

@sjdoc Rational "
Rational numbers are entered with 'a//b', for Integers a and b.  This is done when the
expression is parsed, so it is much faster than 'a/b'. The form
'Rational(a,b)' is not yet implemented.
"
