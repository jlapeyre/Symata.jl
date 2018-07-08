"""
    wrapout(x)

Wrap Symata output for printing. Most methods are defined
in output.jl.
"""
wrapout(x) = x

for f in ( :print, :println, :string, :warn, :error, :throw)
   @eval function $(Symbol("sym" * string(f)))(args...)
        $(f)(map(wrapout,args)...)
    end
end



@doc """
   symprintln(expr)

prints `expr` as a Symata expression. For instance, symbols
are printed without the leading `:`. Rational numbers are printed
like `x/y` rather than `x//y`.
""" symprintln
