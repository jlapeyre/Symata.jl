### Length

@sjdoc Length """
    Length(expr)

print the length of `expr`.

For Symata expressions, the length is the number or arguments. For scalar Julia
types, the length is zero. For `Array`'s and `Dict`'s the length is the same as
Julia `length`.
"""
@mkapprule Length :nargs => 1
@doap Length(x) = symlength(x)

symlength(mx::Mxpr) = length(mx)
symlength(s::String) = 1
symlength(s::Symbol) = 0
symlength(s::Number) = 0
function symlength(x)
    try
        length(x)
    catch
        0
    end
end

### LeafCount

## This is not the same as Mma.
## 1/2 + I  -> Complex[Rationa[1,2],1]  : LeafCount is 5
## We have Complex{:Rational}(Rational(1,2),Rational(1,1)), LeafCount is 7

"""
    leaf_count(x)

return the number of nodes in `x`. This views `Mxpr`'s as trees and all other objects as nodes.
This is `1` if `x` is a node and the number of nodes in the tree if `x` is an `Mxpr`.
An empty `Mxpr` is an empty tree, i.e. a node.

`LeafCount` is Mma's term. But better might be `NodeCount`,
because it counts all nodes in the tree, not only terminal nodes.
"""
leaf_count(x) = 1
leaf_count(x::Complex) = 3
leaf_count{T<:Rational}(x::Complex{T}) = 7
leaf_count(x::Rational) = 3
leaf_count(mx::Mxpr) = sum(leaf_count,margs(mx)) + 1  #   1 for the Head

### ByteCount

## Try to count bytes allocated for everything in an object.
## This relies in part on Julia's ability to do this with sizeof.
## But the result is not quite what we want, so we define jssizeof.
## For instance, sizeof a BigInt always returns 16. The amount of data allocated
## is it is probably actually  8 * x.size or x.alloc.
## We also guess what to do for symbol, and we don't yet handle BigFloat.

jssizeof(x) = sizeof(x)
# I think they are 64 bit chunks
jssizeof{T<:BigInt}(x::T) = 8 * x.alloc
jssizeof(a::Symbol) = Int(ccall(:strlen, Int32, (Ptr{UInt8},), a))
byte_count(x) = jssizeof(x)
function byte_count(mx::Mxpr)
    count::Int = sum(byte_count, margs(mx))
    count += jssizeof(mx)
    return count
end

### Depth

## This is the maximum depth of the tree.
## We do not descend into Complex and Rational.
## Different than tree for LeafCount.
depth(x) = 1
function depth(mx::Mxpr)
    d::Int = 1
    for i in 1:length(mx)
        if is_Mxpr(mx[i])
            nd = depth(mx[i])
            if nd > d
                d = nd
            end
        end
    end
    return d + 1
end


### LeafCount

@sjdoc LeafCount """
    LeafCount(expr)

gives the number of indivisible (`Part` can't be taken) elements in `expr`.

This amounts to counting all the `Heads` and all of the arguments that are not of type `Mxpr`,
that is compound expressions.

A more accurate name is `NodeCount`.
"""
@mkapprule LeafCount :nargs => 1
@doap LeafCount(x) = leaf_count(x)

#### ByteCount

@sjdoc ByteCount """
    ByteCount(expr)

gives number of bytes in `expr`.
"""
@mkapprule ByteCount :nargs => 1
@doap ByteCount(x) = byte_count(x)


#### Depth

@sjdoc Depth """
    Depth(expr)

gives the maximum number of indices required to specify
any part of `expr`, plus `1`.
"""
@mkapprule Depth :nargs => 1
@doap Depth(x) = depth(x)

### Dimensions

@sjdoc Dimensions """
    Dimensions(expr)

returns a list of the dimensions of `expr`.
"""

@mkapprule Dimensions :nargs => 1:2
@doap Dimensions(x) = 0

type DimensionsData
    dims::Array{Int,1}
    level::Int
    head
    done::Bool
end

@doap function Dimensions(x::Mxpr)
    data = DimensionsData(Array{Int}(0), 1, mhead(x), false)
    dimensions(x,data)
    MList(data.dims)
end

dimensions(x,data) = 0

function dimensions(x::Mxpr,data)
    length(x) ==  0 && return 0
    if mhead(x[1]) !=  data.head
        data.done = true
    else
        thelength = symlength(x[1])
        for i in 2:length(x)
            if symlength(x[i]) != thelength || mhead(x[i]) != data.head
                data.done = true
                break
            end
        end
    end
    if length(data.dims) < data.level
        push!(data.dims, symlength(x))
    end
    data.done && return
    data.level +=1
    for i in 1:symlength(x)
        dimensions(x[i],data)
        data.done && return
    end
    data.level -=1
end

## Maybe Allocated belongs with Time and Timing. They are all in different places.

### Allocated

@sjdoc Allocated """
    Allocated(expr)

evaluate `expr` and return a list of the memory allocated
and the result of the evaluation.
"""

function apprules(mxt::Mxpr{:Allocated})
    local mx
    a = @allocated begin
        reset_meval_count()
        mx = doeval(mxt[1])
        setsymval(:ans,mx)  ## why here ?
    end
    mxpr(:List,a,mx)
end
