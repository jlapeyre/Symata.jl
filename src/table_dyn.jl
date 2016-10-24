## Table with dynamic scope for iteration variable.
## using a[2] for iteration variable still does not work.
##
## We need to reimplement tricks like writing the value
## of the iteration variable into a copy of the expression (or restoring it).
## In this case, we still have to localize the variable with dynamic scope so
## it has the expected (possible) side-effects.
##
## Also, this is copied from older code. There are probably no inefficiencies introduced.
## But, it is more complicated than it needs to be

abstract TableEval

immutable TableSetEval{T,V} <: TableEval
    expr::T
    sym::V
end

immutable TableNoSetEval{T} <: TableEval
    expr::T
end

@mkapprule Table

@sjdoc Table """
    Table(expr,[imax])

return a `List` of `imax` copies of `expr`.

    Table(expr,[i,imax])

returns a `List` of `expr` evaluated `imax` times with `i` set successively to `1` through `imax`.

    Table(expr,iter)

`iter` can be any standard iterator.

    Table(expr,iter1,iter2,...)

is equivalent to `Table(Table(expr,iter2),iter1)...`

This example calls an anonymous Julia function
```
f = :( g(x) = x^2 )
Table( f(i), [i,10])
```
"""

set_attribute(:Table, :HoldAll)

@doap function Table(inexpr, iters...)
    if length(iters) > 1
        riters = reverse(iters)
        mxout = mxpr(:Table,inexpr,riters[1])
        for i in 2:length(riters)
            mxout = mxpr(:Table,mxout,riters[i])
        end
        return mxout
    end
    expr = is_Mxpr(inexpr,:Jxpr) ? eval(inexpr) : inexpr
    it = iters[1]
    sjiter = make_sjiter(it)
    dosaveval::Bool = true
    if typeof(sjiter) <: SJIter1
        dosaveval = false
    end
    local saveval
    if dosaveval                  
        saveval = symval(it[1])   
    end
    local t = NullMxpr
    try          # at the moment it doesn't matter much because we can't interrupt anyway..., could be errors though
        t = table(expr, sjiter)
    catch
        symerror("error in Table")  # or interrupt
    finally
        if dosaveval    
            setsymval(it[1], saveval)
        end
    end
    t
end

function table(expr, sjiter)
    mxpr(:List, intable(expr, sjiter))
end

function intable(expr, iter::SJIter1)
    tableloop1(expr, iter.imax)
end

function intable(expr, iter::SJIter2)
    (sym, nexpr) = (iter.i,expr)
    tableloop2(sym, nexpr, iter.imax)
end

function intable(expr, iter::SJIterList)
    (sym, nexpr) = (iter.i,expr)
    tableloop_list(sym, nexpr, iter.list)
end

function evaltable(model::TableSetEval, val)
    setandeval(model.expr, model.sym, val)
end

function evaltable(model::TableNoSetEval)
    nosetandeval(model.expr)
end

function setandeval(expr,sym,val)
    setsymval(sym,val)
    unsetfixed(expr)
    r = doeval(expr)
    setfixed(r) # trying to gain a little efficiency
    setcanon(r) # usually fails in some more or less edge case
    r
end

function nosetandeval(expr)
    unsetfixed(expr)
    r = doeval(expr)
    setfixed(r) # trying to gain a little efficiency
    setcanon(r) # usually fails in some more or less edge case
    r
end

function intable{T<:Real,V<:Real}(expr0,iter::SJIter3{T,V})
    (sym, expr) = (iter.i,expr0)
    imax = doeval(iter.imax) # maybe this should be done earlier. When iter is created ?
    imin = doeval(iter.imin)
    args = newargs(imax-imin+1)
    if iter.i == expr
        @inbounds for i in imin:imax
            args[i-imin+1] = i
        end
    else
        model = TableSetEval(expr,sym)
        _table3(model,imin,imax,args)
    end
    return args
end

function _table3(model,imin,imax,args)
    for i in imin:imax
        args[i-imin+1] = evaltable(model,i)
    end
end

function intable{T<:Real, V<:Real, W<:Real}(expr0, iter::SJIter4{T,V,W})
    (sym, expr) = (iter.i,expr0)
    imax = iter.imax  # why do we not evaluate here
    imin = iter.imin
    di = iter.di
    args = newargs(iter.num_iters)
    j::Int = 0
    if iter.i == expr
        @inbounds for i in imin:di:imax
            j += 1
            args[j] = i
        end
    else
        model = TableSetEval(expr,sym)
        @inbounds for i in imin:di:imax
            j += 1
            args[j] = evaltable(model,i)
        end
    end
    return args
end

# Symbolic value and increment
function intable{T, V, W}(expr0, iter::SJIter4{T,V,W})
    (sym, expr) = (iter.i,expr0)
    imax = iter.imax
    imin = iter.imin
    di = iter.di
    num_iters = iter.num_iters
    args = newargs(iter.num_iters)
    model = TableSetEval(expr,sym)
    val = deepcopy(imin)
    @inbounds for i in 1:num_iters
        args[i] = evaltable(model,val)
        val = doeval(mxpr(:Plus, val, di))
    end
    args
end

function tableloop1(expr, imax)
    args = newargs(imax)
    model = TableNoSetEval(expr)
    @inbounds  for i=1:imax
        args[i] = evaltable(model)
    end
    args
end

function tableloop2(sym, expr, imax)
    args = newargs(imax)
    model = TableSetEval(expr,sym)
    @inbounds  for i=1:imax
        args[i] = evaltable(model,i)
    end
    args
end

function table3{T<:Real,V<:Real}(expr,iter::SJIter3{T,V})
    imax = doeval(iter.imax) # maybe this should be done earlier. When iter is created ?
    imin = doeval(iter.imin)
    args = newargs(imax-imin+1)
    if iter.i == expr
        @inbounds for i in imin:imax
            args[i-imin+1] = i
        end
    else
        model = TableSetEval(expr,sym)
        @inbounds for i in imin:imax
            args[i-imin+1] =  evaltable(model,i)
        end
    end
    return args
end

function tableloop_list(sym, expr, list)
    len = length(list)
    args = newargs(len)
    model = TableSetEval(expr,sym)
    for i in 1:len
        args[i] = evaltable(model,list[i])
    end
    args
end
