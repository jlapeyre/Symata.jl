# formatting expression

### Argument specification

struct ArgSpec
    argidx::Int
    hasfilter::Bool
    filter::Function

    function ArgSpec(idx::Int, hasfil::Bool, filter::Function)
        idx != 0 || error("Argument index cannot be zero.")
        new(idx, hasfil, filter)
    end
end

getarg(args, sp::ArgSpec) =
    (a = args[sp.argidx]; sp.hasfilter ? sp.filter(a) : a)

# pos > 0: must not have iarg in expression (use pos+1), return (entry, pos + 1)
# pos < 0: must have iarg in expression, return (entry, -1)
# pos = 0: no positional argument before, can be either, return (entry, 1) or (entry, -1)
function make_argspec(s::AbstractString, pos::Int)
    # for argument position
    iarg::Int = -1
    hasfil::Bool = false
    ff::Function = Base.identity

    if !isempty(s)
        ifil = searchindex(s, "|>")
        if ifil == 0
            iarg = parse(Int,s)
        else
            iarg = ifil > 1 ? parse(Int,s[1:ifil-1]) : -1
            hasfil = true
            ff = eval(Symbol(s[ifil+2:end]))
        end
    end

    if pos > 0
        iarg < 0 || error("entry with and without argument index must not coexist.")
        iarg = (pos += 1)
    elseif pos < 0
        iarg > 0 || error("entry with and without argument index must not coexist.")
    else # pos == 0
        if iarg < 0
            iarg = pos = 1
        else
            pos = -1
        end
    end

    return (ArgSpec(iarg, hasfil, ff), pos)
end


### Format entry

struct FormatEntry
    argspec::ArgSpec
    spec::FormatSpec
end

function make_formatentry(s::AbstractString, pos::Int)
    @assert s[1] == '{' && s[end] == '}'
    sc = s[2:end-1]
    icolon = search(sc, ':')
    if icolon == 0  # no colon
        (argspec, pos) = make_argspec(sc, pos)
        spec = FormatSpec('s')
    else
        (argspec, pos) = make_argspec(sc[1:icolon-1], pos)
        spec = FormatSpec(sc[icolon+1:end])
    end
    return (FormatEntry(argspec, spec), pos)
end


### Format expression

mutable struct FormatExpr
    prefix::String
    suffix::String
    entries::Vector{FormatEntry}
    inter::Vector{String}
end

_raise_unmatched_lbrace() = error("Unmatched { in format expression.")

function find_next_entry_open(s::AbstractString, si::Int)
    slen = length(s)
    p = search(s, '{', si)
    p < slen || _raise_unmatched_lbrace()
    while p > 0 && s[p+1] == '{'  # escape `{{`
        p = search(s, '{', p+2)
        p < slen || _raise_unmatched_lbrace()
    end
    # println("open at $p")
    pre = p > 0 ? s[si:p-1] : s[si:end]
    if !isempty(pre)
        pre = replace(pre, "{{" => '{')
        pre = replace(pre, "}}" => '}')
    end
    return (p, convert(String, pre))
end

function find_next_entry_close(s::AbstractString, si::Int)
    slen = length(s)
    p = search(s, '}', si)
    p > 0 || _raise_unmatched_lbrace()
    # println("close at $p")
    return p
end

function FormatExpr(s::AbstractString)
    slen = length(s)

    # init
    prefix = convert(String, "")
    suffix = convert(String, "")
    entries = FormatEntry[]
    inter = String[]

    # scan
    (p, prefix) = find_next_entry_open(s, 1)
    if p > 0
        q = find_next_entry_close(s, p+1)
        (e, pos) = make_formatentry(s[p:q], 0)
        push!(entries, e)
        (p, pre) = find_next_entry_open(s, q+1)
        while p > 0
            push!(inter, pre)
            q = find_next_entry_close(s, p+1)
            (e, pos) = make_formatentry(s[p:q], pos)
            push!(entries, e)
            (p, pre) = find_next_entry_open(s, q+1)
        end
        suffix = pre
    end
    FormatExpr(prefix, suffix, entries, inter)
end

function printfmt(io::IO, fe::FormatExpr, args...)
    if !isempty(fe.prefix)
        write(io, fe.prefix)
    end
    ents = fe.entries
    ne = length(ents)
    if ne > 0
        e = ents[1]
        printfmt(io, e.spec, getarg(args, e.argspec))
        for i = 2:ne
            write(io, fe.inter[i-1])
            e = ents[i]
            printfmt(io, e.spec, getarg(args, e.argspec))
        end
    end
    if !isempty(fe.suffix)
        write(io, fe.suffix)
    end
end

printfmt(io::IO, fe::AbstractString, args...) = printfmt(io, FormatExpr(fe), args...)
printfmt(fe::Union{AbstractString,FormatExpr}, args...) = printfmt(STDOUT, fe, args...)

printfmtln(io::IO, fe::Union{AbstractString,FormatExpr}, args...) = (printfmt(io, fe, args...); println(io))
printfmtln(fe::Union{AbstractString,FormatExpr}, args...) = printfmtln(STDOUT, fe, args...)

format(fe::Union{AbstractString,FormatExpr}, args...) =
    sprint(printfmt, fe, args...)
