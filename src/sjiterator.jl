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
    loopmax::Int
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
    args = margs(mx)
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
            elseif is_type_less(args[2],Real)
                return SJIter2(args[1],args[2])
            else
                itererror(mx)
            end
        elseif len == 3        # We need to overload ops to make these easier to write
            # do we need deepcopy here ?
            loopmax = doeval(mxpr(:Plus, args[3], -1 * args[2]))
            if is_type_less(loopmax, Number) # args3 - args2
                return SJIter3(args[1],args[2],args[3],floor(Int,loopmax)) # use splat ?
            else
                itererror(mx)
            end
        elseif len == 4
            nargs = deepcopy(args) # needed if we do the computation below
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
                return SJIter4(args...,floor(Int,num_iters)+1)
            else
                itererror(mx)
            end
        else
            itererror(mx)
        end
    end
end
