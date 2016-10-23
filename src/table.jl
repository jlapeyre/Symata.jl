@mkapprule NTable

## This is our versions of Table with the loop variable having lexical scope.
## It is disabled because function names would clobber the same ones used in
## the dynamic scoping version in table_dyn.jl
##
## Table sets a symbol sym to a sequence of values, evaluating expr
## each time and building a list of the results.
##
## We replace sym everywhere it appears in expr by a gensym so that we don't
## clobber other bindings of sym.
##
## If there are few replacements to make, another approach appears to be faster (by about a factor of 2):
## Instead of substituting a unique symbol, we record the positions in expr at which sym appears.
## Then, at each iteration, we subsitute the value directly into (a copy of) expr and evaluate.
##
## But, if expr is large and sym appears many times, using the gensym approach is much faster.
##
## We should add some logic to choose between the strategies. 
##
## To start, we use types representing strategies <: TableEval. We first implement strategies
## that use setsymval. Next we will implment those that substitute the value directly.
## Then we need a criterion for dispatch. Probably just the number of subsitutions is enough
## for zeroeth order. But, the setsymval strategy is best if we only have one strategy. It
## seems to be at worst about half as fast as direct substitution, wheras the latter can get
## arbitrarily bad if there are many substitutions.
##

abstract TableEval

immutable TableSetEval{T,V} <: TableEval
    expr::T
    sym::V
end

immutable TableNoSetEval{T} <: TableEval
    expr::T
end

@doap function NTable(inexpr, iters...)
    if length(iters) > 1
        riters = reverse(iters)
        mxout = mxpr(:Table,inexpr,riters[1])
        for i in 2:length(riters)
            mxout = mxpr(:Table,mxout,riters[i])
        end
        return mxout
    end
    expr = is_Mxpr(inexpr,:Jxpr) ? eval(inexpr) : deepcopy(inexpr)
    table(expr, iters)
end

function table(expr, iters)
    length(iters) == 1 && return mxpr(:List, intable(expr, make_sjiter(iters[1])))
end

function intable(expr, iter::SJIter1)
    tableloop1(expr, iter.imax)
end

function intable(expr, iter::SJIter2)
    (sym, nexpr) = localize_variable(iter.i,expr)
    tableloop2(sym, nexpr, iter.imax)
end

function intable(expr, iter::SJIterList)
    (sym, nexpr) = localize_variable(iter.i,expr)
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
    (sym, expr) = localize_variable(iter.i,expr0)
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
    (sym, expr) = localize_variable(iter.i,expr0)
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
    (sym, expr) = localize_variable(iter.i,expr0)
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
