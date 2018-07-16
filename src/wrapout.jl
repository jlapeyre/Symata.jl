"""
    wrapout(x)

Wrap Symata output for printing. Most methods are defined
in output.jl.
"""
wrapout(x) = x

## TODO: disable symwarn, so we are forced to use @symwarn below
for f in ( :print, :println, :string, :warn, :error, :throw)
   @eval function $(Symbol("sym" * string(f)))(args...)
        $(f)(map(wrapout,args)...)
    end
end

"""
    @symwarn(arg)

Performs macro call `@warn` on `wrapout(arg)` so that
Julia displays objects of type `Mxpr` correctly.
"""
macro symwarn(arg)
    ex = Meta.parse("@warn(dummy)")
    ex.args[3] = wrapout(arg)
    return ex
end

@doc """
   symprintln(expr)

prints `expr` as a Symata expression. For instance, symbols
are printed without the leading `:`. Rational numbers are printed
like `x/y` rather than `x//y`.
""" symprintln
