## Copied from base. There is probably a trick to avoid this.
function print_with_function_to_string(printfunc::Function, xs...; env=nothing)
    # specialized for performance reasons
    s = IOBuffer(Array{UInt8}(Base.tostr_sizehint(xs[1])), true, true)
    # specialized version of truncate(s,0)
    s.size = 0
    s.ptr = 1
    if env !== nothing
        env_io = IOContext(s, env)
        foreach( x -> printfunc(env_io, x), xs)
    else
        foreach( x -> printfunc(s, x), xs)
    end
    String(resize!(s.data, s.size))
end

"""
    @runtime_include filename initfunc

defines a function with name `initfunc` to load code in `filename` (in the src
directory) at runtime and subsequently check if it is loaded.  `initfunc()` will
return true if the code is already loaded.

A similar macro should be written to "use" a module at runtime.
"""
macro runtime_include(fname, initfunc)
    fpath = gensym()
    quote
        const $fpath = joinpath(dirname(@__FILE__), $(esc(fname)))
        let polys_inited = false
            global $(esc(initfunc))
            function ($(esc(initfunc)))()
                if ! polys_inited
                    try
                        info("loading ", $fpath)
                        if ! isfile($fpath)
                            warn("unable to find '", $fpath, "'")
                        end
                        @eval include($fpath)
                        polys_inited = true
                        return true
                    catch e
                        warn(e)
                        warn("Unable to load '", $fpath, "'")
                        return false
                    end
                end
                true
            end
        end
    end
end


## In the example below, we gave _poly_path the scope of the let.
## But, this requires escaping, as seen below. I didn't have the patience to get this
## working inside a macro. So we use a global constant (gensym) above.
## But, it should be possible to reproduce the example below

# let polys_inited = false, _poly_path = joinpath(dirname(@__FILE__), "PolynomialSequences.jl")
#     global _init_polys
#     function _init_polys()
#     if ! polys_inited
#         try
#             info("loading polynomial functions")
#             if ! isfile(_poly_path)
#                 warn("unable to find '$_poly_path'")
#             end
#             @eval include($(_poly_path)) # The functions are in the Symata module.
#             polys_inited = true
#             return true
#         catch e
#             warn(e)
#             warn("Unable to load 'PolynomialSequences'.")
#             return false
#         end
#     end
#     true
#     end
# end
