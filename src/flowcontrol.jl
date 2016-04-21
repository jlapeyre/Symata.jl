# For, While, Do, If, CompoundExpression

macro checkbreak()
    return esc(:(
    if FLOWFLAGS[:Break]
        FLOWFLAGS[:Break] = false
        break
    end))
end

# Localize variables.
# For lexically scoped variables. Replace symbol os with ns in ex
# We should follow what we did in table and set the value in the function,
# rather than localizing the variable and letting meval do the setting.
function replsym(ex,os,ns)
    if is_Mxpr(ex)
        args = margs(ex)
        @inbounds for i in 1:length(args)
            args[i] = replsym(args[i],os,ns)
        end
    end
    if ex == os
        return ns
    else
        return ex
    end
end


#### For

@sjdoc For "
For(start,test,incr,body) is a for loop. Eg. For(i=1,i<=3, Increment(i) , Println(i))
Using Increment(i) is currently much faster than i = i + 1. There is no special syntax yet for
Increment.
"

# This is pretty fast: For(i=1,i<1000, Increment(i))
# Note using 10^3 is much slower. Mma3.0 also is slower with 10^3
function apprules(mx::Mxpr{:For})
    (start,test,incr)= (mx[1],mx[2],mx[3])
    body = nothing
    if isdefined(margs(mx),4)
        body = mx[4]
    end
    doeval(start)
    if body != nothing
        FLOWFLAGS[:Break] = false
        while doeval(test)
            doeval(body)
            @checkbreak
            # if FLOWFLAGS[:Break]
            #     FLOWFLAGS[:Break] = false
            #     break
            # end
            doeval(incr)
        end
    else # This is not at all faster than doeval(nothing)
        while doeval(test)
            doeval(incr)
        end
    end
end

#### If

# TODO Fix third branch. Dispatch on number or args, etc.

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

#### While

@sjdoc While "
While(test,body) evaluates test then body in a loop until test does not return true.
"

function apprules(mx::Mxpr{:While})
    (test,body)= (mx[1],mx[2])
    FLOWFLAGS[:Break] = false
    while doeval(test) == true
        doeval(body)
        @checkbreak
    end
end

#### Break

@sjdoc Break "
Break() exits the nearest enclosing For, While, or Do loop.
"

@mkapprule Break

function do_Break(mx::Mxpr{:Break})
    FLOWFLAGS[:Break] = true
    nothing
end

#### Do

@sjdoc Do "
Do(expr,[imax]) evaluates expr imax times.
Do(expr,[i,imax]) evaluates expr imax times with i localized taking values from 1 through
  imax in increments of 1.
Do(expr,[i,imin,imax]) evaluates expr with i taking values from imin to imax with increment 1.
  imin and imax may be symbolic.
Do(expr,[i,imin,imax,di]) evaluates expr with i taking values from imin to imax with increment di.
  imin, imax, and di may be symbolic.
Do(expr,[i,[i1,i2,...]) evaluates expr with i taking values from a list.
"

function apprules(mx::Mxpr{:Do})
    expr = mx[1]
    iter = make_sjiter(mx[2])
    do_doloop(expr,iter)
end

function do_doloop(expr,iter::SJIter1)
    do_doloop_kern(expr,iter.imax)
end

# TODO: prbly don't need to use kernel
function do_doloop_kern(expr,imax)
    start = one(imax)
    FLOWFLAGS[:Break] = false
    for i in start:imax
        doeval(expr)
        @checkbreak
    end
end

function do_doloop(expr,iter::SJIter2)
    isym = get_localized_symbol(iter.i)
    ex = replsym(deepcopy(expr),iter.i,isym)
    FLOWFLAGS[:Break] = false
    for i in 1:iter.imax  # mma makes i an Int no matter the type of iter.imax
        setsymval(isym,i)
        doeval(ex)
        @checkbreak
    end
    removesym(isym)
end

function do_doloop{T<:Real,V<:Real}(expr,iter::SJIter3{T,V})
    isym = get_localized_symbol(iter.i)
    ex = replsym(deepcopy(expr),iter.i,isym)
    FLOWFLAGS[:Break] = false
    for i in iter.imin:iter.imax  # mma makes i type of one of these
        setsymval(isym,i)
        doeval(ex)
        @checkbreak
    end
    removesym(isym)
end

# fields of iter may be symbolic
function do_doloop(expr,iter::SJIter3)
    isym = get_localized_symbol(iter.i)
    ex = replsym(deepcopy(expr),iter.i,isym)
    setsymval(isym,iter.imin)
    FLOWFLAGS[:Break] = false
    for i in 1:(iter.num_iters)
        doeval(ex)
        @checkbreak
        setsymval(isym,doeval(mxpr(:Plus,isym,1)))
    end
    removesym(isym)
end

function do_doloop{T<:Real, V<:Real, W<:Real}(expr, iter::SJIter4{T,V,W})
    isym = get_localized_symbol(iter.i)
    ex = replsym(deepcopy(expr),iter.i,isym)
    FLOWFLAGS[:Break] = false
    for i in (iter.imin):(iter.di):(iter.imax)
        setsymval(isym,i)
        doeval(ex)
        @checkbreak
    end
    removesym(isym)
end

# fields of iter may be symbolic
function do_doloop(expr,iter::SJIter4)
    isym = get_localized_symbol(iter.i)
    ex = replsym(deepcopy(expr),iter.i,isym)
    setsymval(isym,iter.imin)
    FLOWFLAGS[:Break] = false
    for i in 1:(iter.num_iters)
        doeval(ex)
        @checkbreak
        setsymval(isym,doeval(mxpr(:Plus,isym,iter.di)))
    end
    removesym(isym)
end

function do_doloop(expr,iter::SJIterList)
    isym = get_localized_symbol(iter.i)
    ex = replsym(deepcopy(expr),iter.i,isym)
    FLOWFLAGS[:Break] = false
    for i in 1:(length(iter.list))
        setsymval(isym,iter.list[i])
        doeval(ex)
        @checkbreak
    end
    removesym(isym)
end

#### CompoundExpression

@sjdoc CompoundExpression "
CompoundExpression(expr1,expr2,...) or (expr1,expr2,...) evaluates each expression in turn and
returns the result of only the final evaluation.
"
function apprules(mx::Mxpr{:CompoundExpression})
    local res
        @inbounds for i in 1:length(mx)
            res = doeval(mx[i])
            FLOWFLAGS[:Break] && break
        end
    res
end
