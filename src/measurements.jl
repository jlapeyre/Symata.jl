###  LeafCount, ByteCount, Depth

## LeafCount

# View Mxpr's as trees and all other objects as nodes.
# leafcount(x) returns the number of nodes in x:
# that is 1 if x is a node and
# the number of nodes in the tree if it is an Mxpr.
# An empty Mxpr is an empty tree, i.e. a node.
#
# LeafCount is Mma's term. But better might be NodeCount,
# because it counts all nodes in the tree, not only terminal nodes.
# This is not the same as Mma.
# 1/2 + I  -> Complex[Rationa[1,2],1]  : LeafCount is 5
# We have Complex{:Rational}(Rational(1,2),Rational(1,1)), LeafCount is 7
leaf_count(x) = 1
leaf_count(x::Complex) = 3
leaf_count{T<:Rational}(x::Complex{T}) = 7
leaf_count(x::Rational) = 3
function leaf_count(mx::Mxpr)
    count::Int = 1 #  1 for the Head
    for i in 1:length(mx)
        count += leaf_count(mx[i])
    end
    return count
end

## ByteCount

# Try to count bytes allocated for everything in an object.
# This relies in part on Julia's ability to do this with sizeof.
# But the result is not quite what we want, so we define jssizeof.
# For instance, sizeof a BigInt always returns 16. The amount of data allocated
# is it is probably actually  8 * x.size or x.alloc.
# We also guess what to do for symbol, and we don't yet handle BigFloat.

jssizeof(x) = sizeof(x)
# I think they are 64 bit chunks
jssizeof{T<:BigInt}(x::T) = 8 * x.alloc
jssizeof(a::Symbol) = Int(ccall(:strlen, Int32, (Ptr{UInt8},), a))
byte_count(x) = jssizeof(x)
function byte_count(mx::Mxpr)
    count::Int = jssizeof(mx)
    args = margs(mx)
    for i in 1:length(mx)
        count += byte_count(args[i])
    end
    return count
end

## Depth

# This is the maximum depth of the tree.
# We do not descend into Complex and Rational.
# Different than tree for LeafCount.
depth(x) = 1
function depth(mx::Mxpr)
    d::Int = 1
    for i in 1:length(mx)
        if is_Mxpr(mx[i])
            nd = depth(mx[i])
            if nd > d d
                d = nd
            end
        else
            nothing
        end
    end
    return d + 1
end

################

#### LeafCount

@sjdoc LeafCount """
    LeafCount(expr)

gives the number of indivisible (`Part` can't be taken) elements in `expr`.

This amounts to counting all the `Heads` and all of the arguments that are not of type `Mxpr`,
that is compound expressions.

A more accurate name is `NodeCount`.
"""


apprules(mx::Mxpr{:LeafCount}) = leaf_count(mx[1])

#### ByteCount

@sjdoc ByteCount """
    ByteCount(expr)

gives number of bytes in `expr`.
"""

apprules(mx::Mxpr{:ByteCount}) = byte_count(mx[1])

#### Depth

@sjdoc Depth """
    Depth(expr)

gives the maximum number of indices required to specify
any part of `expr`, plus `1`.
"""

apprules(mx::Mxpr{:Depth}) = depth(mx[1])

#### Dimensions

# @mkapprule Dimensions :nargs => 1:2

# @doap Dimensions(x) = 0

# type DimensionsData
#     dims::Array{Int,1}
#     level::Int
# end

# dimensions(x,data) = 0

# function dimensions(x::Mxpr,data)
#     length(x) ==  0 && return 0
#     thehead = mhead(x[1])
#     thelength = length(x[1])
#     data.level += 1
#     failflag = false
#     for i in 2:length(x)
#         if length(x[i]) != thelength || mhead(x[i]) != thehead
#             failflag = true
#             break
#             dimensions
#         end
#     end
#     if failflag
#         push!(data.dims, 0)
#     else
#         push!(data.dims, length(x))
#     end
#     data.level -=1
# end

# @doap function Dimensions(x::Mxpr)
#     data = DimensionsData(Array(Int,0), 0)
#     dimensions(x,data)
#     mxpr(:List, data.dims...)
# end
