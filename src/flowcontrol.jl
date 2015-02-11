# Not super-fast.

@sjdoc For "
For(start,test,incr,body) is a for loop. Eg. For(i=1,i<=3, i = i + 1 , Println(i))
"

function apprules(mx::Mxpr{:For})
    (start,test,incr)= (mx[1],mx[2],mx[3])
    body = nothing
    if isdefined(margs(mx),4)
        body = mx[4]
    end
    doeval(start)
    while doeval(test)
        doeval(body)
        doeval(incr)
    end
end

@sjdoc If "
If(test,tbranch,fbranch) evaluates test and if the result is true, evaluates tbranch, otherwise fbranch
"

function apprules(mx::Mxpr{:If})
    (test,tbranch)= (mx[1],mx[2])
    fbranch = false
    if length(mx) == 3
        fbranch = mx[3]
    end
    tres = doeval(test) == true
    tres ? doeval(tbranch) : doeval(fbranch)
end

@sjdoc While "
While(test,body) evaluates test then body in a loop until test does not return true.
"

function apprules(mx::Mxpr{:While})
    (test,body)= (mx[1],mx[2])
    while doeval(test) == true
        doeval(body)
    end
end
        

abstract AbstractSJIter

type SJIter1{T<:Real} <: AbstractSJIter
    imax::T
end

type SJIter2{T<:Real} <: AbstractSJIter
    i::Symbol
    imax::T
end

type SJIter3 <: AbstractSJIter
    i::Symbol
    imin::Any
    imax::Any
end

type SJIter4 <: AbstractSJIter
    i::Symbol
    imin::Any
    imax::Any
    di::Any
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
    if len == 1
        if is_type_less(args[1],Real)
            return SJIter1(args[1])
        else
            itererror(mx)
        end
    elseif ! is_type(args[1],Symbol)
        return itererror(mx)
    else
        if  len ==  2
            if is_Mxpr(args[2],:List)
                return SJIterList(args[1],args[2])
            elseif is_type_less(args[2],Real)
                return SJIter2(args[1],args[2])
            else
                itererror(mx)
            end
        elseif len == 3        # We need to overload ops to make these easier to write
            if is_type_less(doeval(mxpr(:Plus, args[3], -1 * args[2])), Number) # args3 - args2
                return SJIter3(args[1],args[2],args[3]) # use splat ?
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
            res = doeval(tst)
#            println(res)
            if is_type_less(res, Number)
                return SJIter4(args...)
            else
                itererror(mx)
            end
        else
            itererror(mx)
        end
    end
end

function apprules(mx::Mxpr{:Do})
    expr = mx[1]
    iter = make_sjiter(mx[2])
    do_doloop(expr,iter) # need to hand more iters as well
end

function do_doloop(expr,iter::SJIter1)
    
end

function do_doloop2(expr,incr)
    incr = one(incr)
end

#    isym = gensym(string(iter.imax))
#    ex = replsym(deepcopy(expr),iter.imax,isym)
