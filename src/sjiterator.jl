## These are SJulia Mma-like iterators. They have nothing to
# do with Julia iterators.
# There are at least two kinds of "standard" iterators that
# require different Julia types.
# We have AbstractSJIter and AbstractSJIterA.
# "Do" is a prototype using the former, and "Range using the latter.

## Standard iterator used, for instance, by Do

abstract AbstractSJIter

type SJIter1{T<:Real} <: AbstractSJIter
    imax::T
end

type SJIter2{T<:Real} <: AbstractSJIter
    i::Symbol
    imax::T
end

type SJIter3{T,V} <: AbstractSJIter
    i::Symbol
    imin::T
    imax::V
    num_iters::Int
end

type SJIter4{T,V,W} <: AbstractSJIter
    i::Symbol
    imin::T
    imax::V
    di::W
    num_iters::Int
end

type SJIterList <: AbstractSJIter
    i::Symbol
    list::Array{Any,1}
end

# We can make this a bona fide exception
itererror(mx) = error(mx, " does not have the form of an iterator.")

make_sjiter(x) = itererror(x)

function make_sjiter(mx::Mxpr{:List})
    make_sjiter(mx,margs(mx))
end

function make_sjiter{T}(mx,args::Array{T,1})
#    args = margs(mx)
    len = length(args)
    len < 1 && itererror(mx)
    if len == 1
        imax = doeval(args[1])
        if is_type_less(imax,Real)
            return SJIter1(imax)
        else
            itererror(mx)
        end
    elseif ! is_type(args[1],Symbol)
        return itererror(mx)
    else
        if  len ==  2
            if is_Mxpr(args[2],:List)
                return SJIterList(args[1],margs(args[2]))
            else
                imax = doeval(args[2])
                if is_type_less(imax,Real)
                    return SJIter2(args[1],doeval(args[2]))
                else
                    itererror(mx)
                end
            end
        elseif len == 3        # We need to overload ops to make these easier to write
            # do we need deepcopy here ?
            num_iters = doeval(mxpr(:Plus, args[3], -1 * args[2]))
            if is_type_less(num_iters, Number) # args3 - args2
                return SJIter3(args[1],doeval(args[2]),doeval(args[3]),floor(Int,num_iters)) # use splat ?
            else
                itererror(mx)
            end
        elseif len == 4
            nargs = recursive_copy(args) # needed if we do the computation below
            (i,imin,imax,di) = (nargs[1],nargs[2],nargs[3],nargs[4])
            #  Mma does not simplify this: x + y + -2*(x + y)
            #  Mma does simplify this: x + y + -1*(x + y)            
            tst = mxpr(:Times, mxpr(:Plus,imax, mxpr(:Minus,imin)), mxpr(:Power,di,-1))
#            tst = mxpr(:Times, mxpr(:Plus,imax, mxpr(:Expand ,mxpr(:Minus,imin))), mxpr(:Power,di,-1))            
#            tst = extomx(:( ($(args[3]) - $(args[2])) / $(args[4]))) # This works, but is slow. need Expand, too.
#            tst = @exnoeval(:( ((args[3]) - (args[2])) / (args[4]))) # this is wrong
#            println(mxpr(:FullForm,tst))
            num_iters = doeval(tst)
            if is_type_less(num_iters, Number)
                nargs[2] = doeval(args[2])
                nargs[3] = doeval(args[3])
                nargs[4] = doeval(args[4])
                return SJIter4(nargs...,floor(Int,num_iters)+1)
            else
                itererror(mx)
            end
        else
            itererror(mx)
        end
    end
end

## Iterators used by Range

abstract AbstractSJIterA

type SJIterA1{T<:Real} <: AbstractSJIterA
    imax::T
    num_iters::Int    
end

type SJIterA2{T,V} <: AbstractSJIterA
    imin::T
    imax::V
    num_iters::Int
end

type SJIterA3{T,V,W} <: AbstractSJIterA
    imin::T
    imax::V
    di::W
    num_iters::Int
end

#itererrora(a::Array) = error(mxpr(:List,a), " does not have the form of a (single variable) iterator.")
itererrora(a::Array,s::AbstractString) = error(mxpr(:List,a), " does not have the form of a (single variable) iterator. ",s)
#itererrora(mx::Mxpr) = error(mx, " does not have the form of a (single variable) iterator.")
itererrora(mx::Mxpr,s::AbstractString) = error(mx, " does not have the form of a (single variable) iterator. ",s)

make_sjitera(x) = itererrora(x)

function make_sjitera(mx::Mxpr{:List})
    make_sjiter(margs(mx))
end

function make_sjitera{T}(args::Array{T,1})
    len = length(args)
    len < 1 && itererrora(args)
    if len == 1
        imax = doeval(args[1])
        if is_type_less(imax,Real)
            return SJIterA1(imax,floor(Int,imax))
        else
            itererrora(args,"imax is not a Real number.")
        end
    elseif len ==  2
        nargs = recursive_copy(args)
        num_iters = doeval(mxpr(:Plus, nargs[2], -1 * nargs[1]))
        if is_type_less(num_iters, Real) # args2 - args1
            return SJIterA2(args[1],args[2],floor(Int,num_iters)+1)
        else
            itererrora(args, "(imax-imin) is not a Real number.")
        end
    elseif len == 3
        nargs = recursive_copy(args) # needed if we do the computation below
        (imin,imax,di) = (nargs[1],nargs[2],nargs[3])
        tst = mxpr(:Times, mxpr(:Plus,imax, mxpr(:Minus,imin)), mxpr(:Power,di,-1))
        num_iters = doeval(tst)
        if is_type_less(num_iters, Real)
            return SJIterA3(args...,floor(Int,num_iters)+1)
        else
            itererrora(args,"(imax-imin)/di = $num_iters is not a Real number, but has type $(typeof(num_iters))")
        end
    else
        itererrora(args,"Wrong number of arguments: $len")
    end
end

