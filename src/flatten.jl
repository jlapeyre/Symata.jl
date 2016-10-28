## Flatten, flatten nested operations that are associative

# eg   (a + b + (c + d)) --> (a + b + c + d)

# FlatT because Flat is already a symbol
#typealias FlatT Union{Mxpr{:Plus},Mxpr{:Times},Mxpr{:And},Mxpr{:Or}, Mxpr{:LCM}, Mxpr{:GCD} }
# We do not rely only on the Flat attribute. We use FlatT in the hope that Julia compiles
# efficient code for each type in the Union.

# Might be faster to interleave the terms because they may need less
# ordering then.  As it is, this is very fast for flattening two long
# sums.  We tried this:
# m1 = Apply(Plus,Table(x^i,[i,1000]));
# m2 = Apply(Plus,Table(x^i,[i,1000])); then
# m3 = mxpr(:Plus,symval(:m1),symval(:m2)) flatten!(m3) --> 6e-5s
# canonexpr!(m3) (ie ordering) --> 0.011s running canonexpr! again
# (i.e. it is already sorted and combined) takes 0.00925 s, that is
# only a little bit faster.  So maybe optimizing here is not worth
# anything at the moment.  At Symata cli, m3 = m1 + m2 --> 0.04 s.
# calling setfixed() after canonexpr cuts this time to 0.01s But, this
# cannot be done, in general.  Maxima generates the two sums much more
# slowly but adds them much more quickly.

# Flatten one level only
function flatten!{T}(mx::T)
    needsflat::Bool = false
    for x in margs(mx)
        if isa(x,T)
            needsflat = true
            break
        end
    end
    needsflat == false && return mx
    na = newargs()
    for x in margs(mx)
        if isa(x,T)
            for y in margs(x)
                push!(na,y)
            end
        else
            push!(na,x)
        end
    end
    return mxpr(mhead(mx),na)
end

# Flatten one level only if the argument has the Flat attribute
maybeflatten!(mx::FlatT) = flatten!(mx)
maybeflatten!(x) = x


# Flatten to all levels
function flatten_recursive!{T}(mx::T)
    needsflat::Bool = false
    for x in margs(mx)
        if isa(x,T)
            needsflat = true
            break
        end
    end
    needsflat == false && return mx
    na = newargs()
    for x in margs(mx)
        if isa(x,T)
            for y in margs(flatten_recursive!(x))
                push!(na,y)
            end
        else
            push!(na,x)
        end
    end
    return mxpr(mhead(mx),na)
end

type FlattenData
    level::Int
    maxlevel::Int
    head::Symbol  # We may want type Any, for heads that are not Symbols
end

# Flatten from level 1 to level n
function flatten_recursive!{T}(mx::Mxpr{T}, n::Integer)
    d = FlattenData(1,n,T)
    _flatten_recursive!(mx,d)
end

# Flatten expressions with head headtype from level 1 to level n
function flatten_recursive!(mx::Mxpr, n::Integer, headtype::Symbol)
    d = FlattenData(1,n,headtype)
    _flatten_recursive!(mx,d)
end

function _flatten_recursive!(mx::Mxpr, d::FlattenData)
    needsflat::Bool = false
    for x in margs(mx)
        if is_Mxpr(x,d.head)
            needsflat = true
            break
        end
    end
    needsflat == false && return mx
    na = newargs()
    for x in margs(mx)
        if is_Mxpr(x,d.head)
            if d.level >= d.maxlevel
                for y in margs(x)
                    push!(na,y)
                end
            else
                d.level += 1
                for y in margs(_flatten_recursive!(x,d))
                    push!(na,y)
                end
                d.level -=1
            end
        else
            push!(na,x)
        end
    end
    return mxpr(mhead(mx),na)
end

# TODO: implement the "transpose" case
@mkapprule Flatten :nodefault => true

@sjdoc Flatten """
    Flatten(expr)

remove braces from `Lists` at all levels of `expr`.

    Flatten(expr,n)

flatten only down to level `n`.

    Flatten(expr,n,h)

flatten only expressions with head `h`.
"""

@doap Flatten(x::Mxpr) = flatten_recursive!(x)
@doap Flatten(x) = x

@doap function Flatten(x::Mxpr, n::Integer)
    n == 0 && return x
    flatten_recursive!(x,n)
end

@doap function Flatten(x::Mxpr, n, headtype::Symbol)
    n == 0 && return x
    n = (n == Infinity ? typemax(Int) : n)
    flatten_recursive!(x,n, headtype)
end

@doap function Flatten(x::Mxpr, i::Mxpr{:DirectedInfinity})
    i[1] != 1 && return x
    flatten_recursive!(x)
end
