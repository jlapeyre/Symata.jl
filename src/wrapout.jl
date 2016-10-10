"""
    wrapout(x)

Wrap Symata output for printing. Most methods are defined
in output.jl.
"""
wrapout(x) = x

for f in ( :print, :println, :string, :warn, :error )
   @eval function $(Symbol("sym" * string(f)))(args...)
        $(f)(map(wrapout,args)...)
    end
end
