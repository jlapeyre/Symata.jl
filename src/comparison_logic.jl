#### And

function apprules(mx::Mxpr{:And})
    args = margs(mx)
    length(args) == 0 && return true
    nargs = newargs()
    for arg in args
        if isa(arg,Bool)
            arg == true && continue
            arg == false && return false
        end
        push!(nargs, arg)
    end
    length(nargs) == 1 && return nargs[1]
    mxpr(:And,nargs)
end

#### Or

function apprules(mx::Mxpr{:Or})
    args = margs(mx)
    length(args) == 0 && return false
    nargs = newargs()
    for arg in args
        if isa(arg,Bool)
            arg == true && return true
            arg == false && continue
        end
        push!(nargs, arg)
    end
    length(nargs) == 1 && return nargs[1]
    mxpr(:Or,nargs)
end

#### Not

@sjdoc Not "
Not(expr) returns False if expr is True, and True if it is False. Not reduces some very simple logical expressions and otherwise
remains unevaluated. Not(expr) may also be entered '! expr'.
"

@mkapprule Not :nargs => 1

do_Not(mx::Mxpr{:Not}, ex::Bool) = ex == true ? false : true

do_Not{T<:Number}(mx::Mxpr{:Not}, ex::T) = mx
do_Not{T}(mx::Mxpr{:Not}, ex::T) = mx

const comparison_negations  = Dict(
                               :<   =>  :>=,
                               :>   =>  :<=,
                               :<=  =>  :>,
                               :>=  =>  :<,
                               :(==)  =>  :!=,
                               :!=  =>  :(==)
                               )

function do_Not(mx::Mxpr{:Not},  ex::Mxpr{:Comparison})
    if length(ex) == 3
        return mxpr(:Comparison, ex[1], comparison_negations[ex[2]], ex[3])
    end
    return mx
end


#### Comparison

@sjdoc Comparison "
Comparison(expr1,c1,expr2,c2,expr3,...) performs or represents a
chain of comparisons. Comparison expressions are usually input and
displayed using infix notation.
"
@sjexamp(Comparison,
         ("Clear(a,b,c)",""),
         ("a == a","true"),
         ("a == b","false"),
         ("a < b <= c","a < b <= c"),
         ("(a=1,b=2,c=2)","2"),
         ("a < b <= c","true"))
# We do this the Julia- and mma4max way, not the Mma way.

function apprules(mx::Mxpr{:Comparison})
    do_Comparison(mx,margs(mx)...)
#    do_Comparison(mx)
end

# Mma does this a == a != b  --->  a == a && a != b,  and  a == a  -->  True
# Note: Mma 10, at least does this: a == a != b  ---> a != b, in disagreement with the above

# FIXME: Don't convert all chained comparisons to conjunctions
# But, Mma  does this a < b < c,  ie. does not alwasy return conjunctions.
# This always returns conjunctions if more than on comparison remains
# after removing true comparisons.
function do_Comparison(mx::Mxpr{:Comparison},args...)
    len = length(args)
    nargs = newargs()
    for i in 2:2:len
        a = args[i-1]
        cmp = args[i]
        b = args[i+1]
        res = _do_Comparison(a,cmp,b)
        if isa(res, Bool)
            res == false && return res
            push!(nargs,res)
        else
            push!(nargs,(a,cmp,b))
        end
    end
    nargs1 = newargs()
    for i in 1:length(nargs)
        a = nargs[i]
        if a != true
            push!(nargs1, mxpr(:Comparison, a...))
        end
    end
    length(nargs1) == 1 && return nargs1[1]
    mxpr(:And, nargs1)
end

# This does:  1 < 2 < b  -->  1 < 2 < b
function old_do_Comparison(mx::Mxpr{:Comparison},args...)
    len = length(args)
    for i in 2:2:len
        a = args[i-1]
        cmp = args[i]
        b = args[i+1]
        res = _do_Comparison(a,cmp,b)
        res == false && return res
        res != true && return mx
    end
    return true
end

function do_Comparison{T<:Number,V<:Number}(mx::Mxpr{:Comparison},a::T,comp::SJSym,b::V)
    _do_Comparison(a,comp,b)
end

function _do_Comparison{T<:Number, V<:Number}(a::T, comp::SJSym, b::V)
    if comp == :<    # Test For loop shows this is much faster than evaling Expr
        return a < b
    elseif comp == :>
        return a > b
    elseif comp == :(==)
        return a == b
    elseif comp == :(>=)
        return a >= b
    elseif comp == :(<=)
        return a <= b
    elseif comp == :(!=)
        return a != b
    elseif comp == :(===)
        return a === b
    end
    eval(Expr(:comparison,a,comp,b)) # This will be slow.
end

## FIXME Uh this is just copied from above. This is required to disambiguate
# from the catchall below
function _do_Comparison{T<:Number}(a::T, comp::SJSym, b::T)
    if comp == :<    # Test For loop shows this is much faster than evaling Expr
        return a < b
    elseif comp == :>
        return a > b
    elseif comp == :(==)
        return a == b
    elseif comp == :(>=)
        return a >= b
    elseif comp == :(<=)
        return a <= b
    elseif comp == :(!=)
        return a != b
    elseif comp == :(===)
        return a === b
    end
    eval(Expr(:comparison,a,comp,b)) # This will be slow.
end

# This catches some cases
function _do_Comparison{T<: Number}(mx::Mxpr{:DirectedInfinity}, comp::SJSym, n::T)
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return nothing
end

function _do_Comparison{T<: Number}(n::T, comp::SJSym, mx::Mxpr{:DirectedInfinity})
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return nothing
end

# FIXME. duplicated code. Maybe SJulia needs its own Boolean type, one that is not <: Number
function _do_Comparison(mx::Mxpr{:DirectedInfinity}, comp::SJSym, n::Bool)
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return nothing
end

function _do_Comparison(n::Bool, comp::SJSym, mx::Mxpr{:DirectedInfinity})
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return nothing
end


# a == a  --> True, etc.  for unbound a
function _do_Comparison{T<:Union{Mxpr,SJSym,AbstractString,DataType}}(a::T,comp::SJSym,b::T)
    if comp == :(==)
        res = a == b
        res && return res
    elseif comp == :(!=)
        res = a == b
        res && return false
    elseif comp == :(>=)  # Julia says  :a <= :b because symbols are ordred lexicographically
        res = a == b      # We don't want this behavior
        res && return res
    elseif comp == :(<=)
        res = a == b
        res && return res
    elseif comp == :(===)
        return a === b
    end
    return nothing
end

# TODO: Try to find why the Unions don't work and condense these methods
function _do_Comparison(a::Mxpr,comp::SJSym,b::Mxpr)
    if comp == :(==)
        res = a == b
        res && return res
    elseif comp == :(!=)
        res = a == b
        res && return false
    elseif comp == :(===)
        return a === b
    end
    return nothing
end

function _do_Comparison(a::Mxpr,comp::SJSym,b::SJSym)
    if comp == :(==)
        res = a == b
        res && return res
    elseif comp == :(!=)
        res = a == b
        res && return false
    elseif comp == :(===)
        return a === b
    end
    return nothing
end

_do_Comparison{T<:SJReal}(a::SJSym, comp::SJSym, b::T) = nothing
_do_Comparison{T<:Union{Mxpr,AbstractString,DataType}}(a::T, comp::SJSym, b::SJSym) = nothing
_do_Comparison{T<:SJReal}(a::T, comp::SJSym, b::Mxpr) = nothing
_do_Comparison{T<:SJReal}(a::Mxpr, comp::SJSym, b::T) = nothing

function _do_Comparison{T<:Number}(a::T, comp::SJSym, b::Bool)
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return false
end

function _do_Comparison(a::Bool, comp::SJSym, b::Bool)
    comp == :(==) && return a == b
    comp == :(!=) && return a != b
    comp == :(===) && return a == b
    return false
end

function _do_Comparison(a, comp::SJSym, b::Bool)
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return false
end

_do_Comparison(a::Qsym, comp::Symbol, b::Bool) = nothing
_do_Comparison{T<:Number}(a::Qsym, comp::SJSym, b::T) = nothing

function _do_Comparison{T<:Number}(a, comp::SJSym, b::T)
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return false
end

# Note the asymmetry between this and previous method.
# This one, at least, is correct. and catches 2 < b
function _do_Comparison{T<:Number}(a::T, comp::SJSym, b::SJSym)
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return nothing
end

# used this to search for bug
# _do_Comparison{T<:Number, V<:Mxpr}(mx::V, comp::SJSym, n::T) = false

function  _do_Comparison{T<:Number, V<:Mxpr}(mx::V, comp, n::T)
    if typeof(comp) != SJSym
        error("_do_Comparison: Comparing with $comp, of type ", typeof(comp))
    else
        error("_do_Comparison: (assert error) Got symbol $comp, when expecting non-symbol")
    end
end

# function _do_Comparison(a::Bool, comp::SJSym, b::Bool)
#     comp == :(==) && return a == b
#     comp == :(!=) && return a != b
#     comp == :(===) && return a == b
#     return false  # I guess this is good
# end

# This is meant to be a catchall for any object.
# But, we should use try catch because == may not be defined.
# Currently, this catches qsym
# function _do_Comparison{T}(a::T, comp::SJSym, b::T)
#     comp == :(==) && return a == b ? true : nothing
#     comp == :(!=) && return a != b ? nothing : true
#     comp == :(===) && return a == b
#     return nothing
# end

# function _do_Comparison{T}(a::T, comp::SJSym, b::T)
#     comp == :(==) && return a == b ? true : nothing
#     comp == :(!=) && return a != b ? nothing : true
#     comp == :(===) && return a == b
#     return nothing
# end


# FIXME. We need >=, <= like this in several places
# Break them out into a function
# NB. Mma leaves  a < a unevaluated.
# This is probably good because a may be of a type for which there is no order
function _do_Comparison{T<:Qsym}(a::T, comp::SJSym, b::T)
    (comp == :(==) || comp == :(>=) || comp == :(<=))  && return a == b ? true : nothing
    comp == :(!=) && return a == b ?  false : nothing
    comp == :(===) && return a == b
#    (comp == :(<) || comp == :(>)) && return a == b ? false : nothing
    return nothing
end

#_do_Comparison(a::Qsym, comp::Symbol, b::Bool) = nothing

# function _do_Comparison{T}(a::Qsym, comp::SJSym, b::T)
#     return nothing
# end

## These allow converting values returned by sympy, although we could do it differntly
apprules(mx::Mxpr{:<}) = mxpr(:Comparison,mx[1],:< ,mx[2])
