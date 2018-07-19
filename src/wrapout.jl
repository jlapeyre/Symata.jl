"""
    wrapout(x)

Wrap Symata output for printing. Most methods are defined
in Output.jl.
"""
wrapout(x) = x

## TODO: disable symwarn, so we are forced to use @symwarn below
for f in ( :print, :println, :string, :warn, :error, :throw)
   @eval function $(Symbol("sym" * string(f)))(args...)
        $(f)(map(wrapout,args)...)
    end
end

## This might take some work. We need to do wrapout on the evaluated arguments,
## which gets into the guts of @warn.
## FIXME. Probably need to write our own version of log_msg :(
"""
    @symwarn(arg)

Performs macro call `@warn` on `wrapout(arg)` so that
Julia displays objects of type `Mxpr` correctly.
"""
macro symwarn(args...)
#    exprs =  map(x -> :(wrapout($(esc(x)))), args)
    exprs =  map(x -> :(wrapout($(esc(x)))), args)
    :(@warn($(exprs...)))
#    :(@warn(wrapout($(esc(arg)))))
#    :(@warn($(wrapout(esc(wrapout(args))))))
#    :(@warn($(map(wrapout, args)...)))
#    :(@warn(map(x -> esc(wrapout(x), $args))))
end

@doc """
   symprintln(expr)

prints `expr` as a Symata expression. For instance, symbols
are printed without the leading `:`. Rational numbers are printed
like `x/y` rather than `x//y`.
""" symprintln
