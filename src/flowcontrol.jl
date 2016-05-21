# For, While, Do, If, CompoundExpression

# FIXME.  We should use dynamically scoped local variables for Do.
# Block is automatically used to localize values of iterators in iteration constructs such as Do, Sum, and Table
# Hmmm. looks like I noted this a long time ago in the doc for Do.


# FIXME. We need to delete local variables before breaking or returning.
macro checkbreak(arg)
    return esc(:( begin
      if is_throw()
        return($arg)
    end
    if is_break()
        clear_break()
        break
    end end))
end

macro unsetbreak()
   esc(:(clear_break()))
end

# Get Return(x), we return Return(x), rather than x.
macro checkthrowreturn(mx)
    esc(:(is_Mxpr($mx,:Return) && return $mx))
end

# Get Return(x), we return x, or Null
macro checkreturn(mx)
    esc(:(is_Mxpr($mx,:Return) && return length($mx) == 0 ? Null : $mx[1]))
end

macro checkthrowcontinue(mx)
    esc(:(is_Mxpr($mx,:Continue) && return $mx))
end

# Probably need to check for
macro checkcontinue(mx,incr)
    esc( :( if is_Mxpr($mx,:Continue) begin
                                       doeval(incr)
                                       continue
                                    end
            end
            ))
end

macro checkcontinue0(mx)
    esc( :( if is_Mxpr($mx,:Continue) continue end ))
end

# NOTE. See comment at top. We should use dynamcic scoping here.
# Localize variables.
# For lexically scoped variables. Replace symbol os with ns in ex
# We should follow what we did in table and set the value in the function,
# rather than localizing the variable and letting meval do the setting.
function substitute_symbol(ex,os,ns)
    if is_Mxpr(ex)
        args = margs(ex)
        @inbounds for i in 1:length(args)
            args[i] = substitute_symbol(args[i],os,ns)
        end
    end
    if ex == os
        return ns
    else
        return ex
    end
end




#### For

@mkapprule For :nargs => 3:4

@sjdoc For "
For(start,test,incr,body) is a for loop. Eg. For(i=1,i<=3, Increment(i) , Println(i))
Using Increment(i) is currently much faster than i = i + 1. (but what about i += 1 ?)
There is no special syntax yet for Increment.

The variable i is not local to the For loop.
"

# This is pretty fast: For(i=1,i<1000, Increment(i))
# Note using 10^3 is much slower. Mma3.0 also is slower with 10^3

function do_For(mx::Mxpr{:For}, start, test, incr)
    @unsetbreak
    doeval(start)
    while
        (res = doeval(test))
        @checkbreak res
        res = doeval(incr)
        @checkbreak res
    end
    Null
end

function do_For(mx::Mxpr{:For}, start, test, incr, body)
    @unsetbreak
    doeval(start)
    while (res = doeval(test))
        @checkbreak res
        res = doeval(body)
        @checkbreak res
        @checkthrowreturn(res)
        @checkcontinue(res,incr)
        doeval(incr)
    end
    Null
end

#### If

@mkapprule If
@sjdoc If "
If(test,tbranch,fbranch) evaluates test and if the result is true, evaluates tbranch, otherwise fbranch.
If(test,tbranch) returns Null if test does not evaluate to True.
If(test,tbranch,fbranch,ubranch) evaluates ubranch if the truth value of test cannot be determined.
"

do_If(mxpr::Mxpr{:If}, test, tbranch) =  doeval(test) == true ? doeval(tbranch) : Null

function do_If(mxpr::Mxpr{:If}, test, tbranch, fbranch)
    tres = doeval(test)
    tres == true ? doeval(tbranch) : tres == false ? doeval(fbranch) : Null
end

function do_If(mxpr::Mxpr{:If}, test, tbranch, fbranch, ubranch)
    tres = doeval(test)
    tres == true ? doeval(tbranch) : tres == false ? doeval(fbranch) : doeval(ubranch)
end

#### While

@sjdoc While "
While(test,body) evaluates test then body in a loop until test does not return true.
"

@mkapprule While :nargs => 1:2

# TODO: Check for return and continue here too
function do_While(mx::Mxpr{:While}, test)
    @unsetbreak
    while (res = doeval(test)) == true
        @checkbreak res
    end
    Null
end

function do_While(mx::Mxpr{:While}, test, body)
    @unsetbreak
    while doeval(test) == true
        res = doeval(body)
        @checkbreak res
        @checkthrowreturn(res)
        @checkcontinue0(res)
    end
    Null
end

#### Break

@sjdoc Break "
Break() exits the nearest enclosing For, While, or Do loop.
"

@mkapprule Break

function do_Break(mx::Mxpr{:Break})
    set_break()
    Null
end

#### Return

@sjdoc Return "
Return(x) returns x from the enclosing block.
Return() returns Null.
Calling Return(x) from within For, While, CompoundExpression returns Return(x).
Calling Return(x) from within Do and Module returns x.
"

#### Continue

@sjdoc Continue "
Continue() begins the next iteration of the enclosing loop without evaluating any remaining
expressions in the body.
"

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

Mma says that Do effectively uses Block to localize variables. This probably means i has dynamic
scope. In SJulia, we give it lexical scope, as in Module.
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
    @unsetbreak
    for i in start:imax
        res = doeval(expr)
        @checkbreak res
        @checkreturn(res)
        @checkcontinue0(res)
    end
    Null
end

function do_doloop(expr,iter::SJIter2)
    isym = get_localized_symbol(iter.i)
    ex = substitute_symbol(deepcopy(expr),iter.i,isym)
    @unsetbreak
    for i in 1:iter.imax  # mma makes i an Int no matter the type of iter.imax
        setsymval(isym,i)
        res = doeval(ex)
        @checkbreak res
        @checkreturn(res)
        @checkcontinue0(res)
    end
    delete_sym(isym)
    Null
end

function do_doloop{T<:Real,V<:Real}(expr,iter::SJIter3{T,V})
    isym = get_localized_symbol(iter.i)
    ex = substitute_symbol(deepcopy(expr),iter.i,isym)
    @unsetbreak
    for i in iter.imin:iter.imax  # mma makes i type of one of these
        setsymval(isym,i)
        res = doeval(ex)
        @checkbreak res
        @checkreturn(res)
        @checkcontinue0(res)
    end
    delete_sym(isym)
    Null
end

# fields of iter may be symbolic
function do_doloop(expr,iter::SJIter3)
    isym = get_localized_symbol(iter.i)
    ex = substitute_symbol(deepcopy(expr),iter.i,isym)
    setsymval(isym,iter.imin)
    @unsetbreak
    for i in 1:(iter.num_iters)
        res = doeval(ex)
        @checkbreak res
        @checkreturn(res)
        @checkcontinue0(res)  # This will not increment isym
        setsymval(isym,doeval(mxpr(:Plus,isym,1)))
    end
    delete_sym(isym)
    Null
end

function do_doloop{T<:Real, V<:Real, W<:Real}(expr, iter::SJIter4{T,V,W})
    isym = get_localized_symbol(iter.i)
    ex = substitute_symbol(deepcopy(expr),iter.i,isym)
    @unsetbreak
    for i in (iter.imin):(iter.di):(iter.imax)
        setsymval(isym,i)
        res = doeval(ex)
        @checkbreak res
        @checkreturn(res)
        @checkcontinue0(res)
    end
    delete_sym(isym)
    Null
end

# fields of iter may be symbolic
function do_doloop(expr,iter::SJIter4)
    isym = get_localized_symbol(iter.i)
    ex = substitute_symbol(deepcopy(expr),iter.i,isym)
    setsymval(isym,iter.imin)
    @unsetbreak
    for i in 1:(iter.num_iters)
        res = doeval(ex)
        @checkbreak res
        @checkreturn(res)
        @checkcontinue0(res)
        setsymval(isym,doeval(mxpr(:Plus,isym,iter.di)))
    end
    delete_sym(isym)
    Null
end

function do_doloop(expr,iter::SJIterList)
    isym = get_localized_symbol(iter.i)
    ex = substitute_symbol(deepcopy(expr),iter.i,isym)
    @unsetbreak
    for i in 1:(length(iter.list))
        setsymval(isym,iter.list[i])
        res = doeval(ex)
        @checkbreak res
        @checkreturn(res)
        @checkcontinue0(res)
    end
    delete_sym(isym)
    Null
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
            is_throw() && return res
            is_break() && break
            @checkthrowreturn(res)
            @checkthrowcontinue(res)
        end
    res
end


#### Warn

@mkapprule Warn :nargs => 1

do_Warn(mx::Mxpr{:Warn},msg::AbstractString) = warn(msg)

#### Throw

@mkapprule Throw :nargs => 1:2

@doap function Throw(x)
    set_throw()
    return mx
end

#### Catch

# TODO: implement tagged throw, catch
@mkapprule Catch :nargs => 1:2

@doap function Catch(x::Mxpr{:Throw})
    clear_throw()
    length(x) > 0 ? x[1] : mx
end

@doap function Catch(x)
    res = doeval(x)
    if is_throw()
        if is_Mxpr(res,:Throw)
            clear_throw()
            return length(res) == 0 ? Null : res[1]
        end
        warn("Catch: throw set but expr is $res")
        return res
    end
    clear_throw()    
    return res
end

