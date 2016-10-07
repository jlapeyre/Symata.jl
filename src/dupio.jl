import Base: show_backtrace, warn_color

function stprint(io::IO, x)
    lock(io)
    try
        stshow(io, x)
    finally
        unlock(io)
    end
    return nothing
end

function stprint(io::IO, xs...)
    lock(io)
    try
        for x in xs
            stprint(io, x)
        end
    finally
        unlock(io)
    end
    return nothing
end

stprintln(io::IO, xs...) = stprint(io, xs..., '\n')
stprint(xs...)   = stprint(STDOUT::IO, xs...)
stprintln(xs...) = stprintln(STDOUT::IO, xs...)
stprint(io::IO, c::Char) = (write(io, c); nothing)
stprint(io::IO, s::AbstractString) = (write(io, s); nothing)


function stprint_to_string(xs...; env=nothing)
    # specialized for performance reasons
    s = IOBuffer(Array{UInt8}(Base.tostr_sizehint(xs[1])), true, true)
    # specialized version of truncate(s,0)
    s.size = 0
    s.ptr = 1
    if env !== nothing
        env_io = IOContext(s, env)
        for x in xs
            stprint(env_io, x)
        end
    else
        for x in xs
            stprint(s, x)
        end
    end
    String(resize!(s.data, s.size))
end

ststring_with_env(env, xs...) = stprint_to_string(xs...; env=env)
ststring(xs...) = stprint_to_string(xs...)

## printing with color ##

function stwith_output_color(f::Function, color::Symbol, io::IO, args...)
    buf = IOBuffer()
    Base.have_color && stprint(buf, get(text_colors, color, color_normal))
    try f(buf, args...)
    finally
        Base.have_color && stprint(buf, color_normal)
        stprint(io, takebuf_string(buf))
    end
end

stprint_with_color(color::Symbol, io::IO, msg::AbstractString...) =
    stwith_output_color(stprint, color, io, msg...)
stprint_with_color(color::Symbol, msg::AbstractString...) =
    stprint_with_color(color, STDOUT, msg...)
stprintln_with_color(color::Symbol, io::IO, msg::AbstractString...) =
    stwith_output_color(stprintln, color, io, msg...)
stprintln_with_color(color::Symbol, msg::AbstractString...) =
    stprintln_with_color(color, STDOUT, msg...)

function stwarn(io::IO, msg...;
              prefix="WARNING: ", once=false, key=nothing, bt=nothing,
              filename=nothing, lineno::Int=0)
    str = chomp(ststring(msg...))
    if once
        if key === nothing
            key = str
        end
        (key in Base.have_warned) && return
        push!(Base.have_warned, key)
    end
    stprint_with_color(warn_color(), io, prefix, str)
    if bt !== nothing
        show_backtrace(io, bt)
    end
    if filename !== nothing
        stprint(io, "\nwhile loading $filename, in expression starting on line $lineno")
    end
    stprintln(io)
    return
end
stwarn(msg...; kw...) = stwarn(STDERR, msg...; kw...)

stwarn(io::IO, err::Exception; prefix="ERROR: ", kw...) =
    stwarn(io, sprint(buf->showerror(buf, err)), prefix=prefix; kw...)

stwarn(err::Exception; prefix="ERROR: ", kw...) =
    stwarn(STDERR, err, prefix=prefix; kw...)

