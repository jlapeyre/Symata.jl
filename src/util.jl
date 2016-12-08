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
