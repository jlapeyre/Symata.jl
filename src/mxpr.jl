const MXDEBUGLEVEL = -1 # debug level, larger means more verbose. -1 is off

## FIX
#  @sj :(:(:(a + 1))) --> :(mplus(a,1)) , too many evaluations.

## FIX
## SJulia does not process expressions in files. So we need
# the auto evaluation of symbols here anyway!

## FIX
# SJulia segfaults when it encounters an unbound symbol representing
# a macro call.

## FIX
# @sj Sqrt(a) , and @sj Sqrt(3), etc. are broken
# in both sjulia and julia and in different ways.

## Less urgent
## FIX
# Printing of BigFloats

# Test if we have the altered interpreter.c, i.e. SJulia
const HAVE_SJULIA = try
    ccall((:jl_is_meval_hook, "libjulia.so"), Bool, ())
    true
catch
    false
end

if HAVE_SJULIA
    include("sjulia.jl")  # Code that only works with SJulia
    macro if_no_sjulia(e)
    end
    macro if_sjulia(e)
        :($(esc(e)))
    end    
else        
    macro if_no_sjulia(e)
        :($(esc(e)))
    end
    macro if_sjulia(e)
    end
end

# This is not yet applied in code.
# It's ugly, but maybe useful.
const SJFloat = Float64
const SJInt = Int
const SJRational = Rational{SJInt}
const _SJOne = 1
const _SJZero = 0
SJOne(x) = _SJOne
SJOne() = _SJOne
SJZero(x) = _SJZero
SJZero() = _SJZero
SJInt(x::Number) = convert(SJInt,x)
SJFloat(x::Number) = convert(SJFloat,x)
SJRational(x::Number) = convert(SJRational,x)

##############################################
##  Mxpr type for symbolic math expression   #
##############################################

# This package is for creating and manipulating expressions. Using
# Julia Expr would be convenient, but seems not possible. Eg. we need
# a dirty bit to know eg if an expression is in canonical form. It's
# not clear how to organize the data. So: Do not access fields the
# directly!

# Mxpr.args is not the same as the equivalent Julia args.  If it were,
# it would allow faster construction of Julia Expr for evaluation.
# Instead, for Expr with head field :call, we use the function name,
# i.e. args[1] as the head of Mxpr.

# We share the Julia symbol table, rather than managing our own symbol
# table.

# get ambiguous method warnings when I use Symbolic
typealias Symbolic Union(Mxpr,Symbol)
# For fast method dispatch. Users can define Orderless Mxpr, too.
# But, we don't handle that yet.
typealias Orderless Union(Mxpr{:mmul},Mxpr{:mplus})

mxmkargs() = Array(Any,0)

mxpr(h,a,jh,d) = Mxpr{h}(h,a,jh,d)
# make an empty Mxpr, unused ?
mxpr(s::Symbol) = Mxpr{s}(s,mxmkargs(),:nothing,false)


## Following is OK in principle, but we stopped using the only instance.
## convert Mxpr.head to Expr.head
# const MOPTOJHEAD = Dict{Symbol,Symbol}()
# let mop,jhead 
#     for (mop,jhead) in ((:<, :comparision),)
# #        MOPTOJHEAD[mop] = jhead
#     end
# end
# function moptojhead(op::Symbol)
#     return haskey(MOPTOJHEAD,op) ? MOPTOJHEAD[op] : :call
# end

# Assumed head field of Expr corresponding to Mxpr if we don't know.
moptojhead(x) = :call   

## Construct Mxpr, similar to construction Expr(head,args...)
#  Set dirty bit. Guess corresponding Julia Expr field 'head'
function mxpr(op::Symbol,args...)
    theargs = mxmkargs()
    for x in args
        push!(theargs,x)
    end
    mx = Mxpr{op}(op,theargs,moptojhead(op),false)
end

## predicates

## What to choose for predicate function names
#  'isthing' or 'thingq' ?
#  'thingq' would signify difference from standard Julia
#  eg: thingq takes Mxpr as argument ?
mk_predicate_sym(sym::Symbol) = symbol(string(sym) * "q")

## generic predicate
macro mk_predicate(name0,code)
    name = mk_predicate_sym(name0)
    @eval begin
        ($name)(x) = $code
    end
end

# get Mxpr head and args
jhead(mx::Mxpr) = mx.jhead
jargs(mx::Mxpr) = mx.args
margs(mx::Mxpr) = mx.args
#args(mx::Mxpr) = mx.args
#margs(ex::Expr) = ex.args  # sometimes Expr can stand in for Mxpr
#mhead(mx::Mxpr) = mx.head
head(mx::Mxpr) = mx.head
exhead(ex::Expr) = ex.head
exargs(ex::Expr) = ex.args
exhead(x) = error("exhead: exhead not defined for $x, of type $(typeof(x))")
head(x) = error("head: head not defined for $x, of type $(typeof(x))")
#mhead(x) = error("mhead: mhead not defined for $x, of type $(typeof(x))")
#setmhead(mx::Expr, val::Symbol) = mx.head = val
sethead(mx::Expr, val::Symbol) = mx.head = val

## Is expr a call to symbol op (this is expression, typ may not be bound)
# macro mk_call_predicate(name0,op)
#     name = mk_predicate_sym(name0)
#     @eval begin
#         ($name)(x::Expr) = (x.head == :call && x.args[1] == $op)
#     end
# end

# macro mk_type_predicate(name0,typ)
#     name = mk_predicate_sym(name0)
#     @eval begin
#         ($name)(x::Expr) = typeof(x) == $typ
#     end
# end

####  index functions

# Get and set parts of expressions. mx[0] is the head
# mx[1] is the first argument, etc.
getindex(mx::Mxpr, k::Int) = return k == 0 ? head(mx) : margs(mx)[k]

# We need to think about copying in the following. Support both refs and copies ?
function getindex(mx::Mxpr, r::UnitRange)
    if r.start == 0
        return mxpr(mx[0],margs(mx)[1:r.stop]...)
    else
        return margs(mx)[r]
    end
end

function getindex(mx::Mxpr, r::StepRange)
    if r.start == 0
        return mxpr(mx[0],margs(mx)[0+r.step:r.step:r.stop]...)
    elseif r.stop == 0 && r.step < 0
        return mxpr(mx[r.start],margs(mx)[r.start-1:r.step:1]...,mx[0])
    else
        return margs(mx)[r]
    end
end

function setindex!(mx::Orderless,val,k::Int)
    if k == 0
        sethead(mx,val)
        return val  # but maybe it is no longer Orderless! Problem.
    else
        margs(mx)[k] = val
        orderexpr!(mx)
        return val
    end
end
        
setindex!(mx::Mxpr, val, k::Int) = k == 0 ? sethead(mx,val) : (margs(mx)[k] = val)
Base.length(mx::Mxpr) = length(margs(mx))
Base.length(s::Symbol) = 0  # Very useful in codes. Symbol is really a simple Mxpr
Base.endof(mx::Mxpr) = length(mx)

# Do we want 'ordered' or 'clean' ? There will be more than
# one way to be dirty, not just unordered.

is_order_clean(mx::Mxpr) = return mx.clean
set_order_dirty(mx::Mxpr) = (mx.clean = false)
set_order_clean(mx::Mxpr) = (mx.clean = true)
sortiforderless!(mx::Orderless) = orderexpr!(mx)  # regardless of dirty bit
sortiforderless!(mx) = mx

# This is deep ==, I think
function ==(a::Mxpr, b::Mxpr)
    (na,nb) = (length(a),length(b))
    na != nb && return false
    for i in 0:na
        a[i] != b[i] && return false
    end
    true
end

## Particular Mxpr types

Base.base(p::Mxpr{:mpow}) = p[1]
expt(p::Mxpr{:mpow}) = p[2]

##################################################

# Same thing is somewhere in base
is_call(ex::Expr) = exhead(ex) == :call
is_call(ex::Expr, op::Symbol) = exhead(ex) == :call && ex.args[1] == op
is_call(ex::Expr, op::Symbol, len::Int) = exhead(ex) == :call && ex.args[1] == op && length(ex.args) == len
is_op(mx::Mxpr, op::Symbol) = head(mx) == op
is_op(mx::Mxpr, op::Symbol, len::Int) = head(mx) == op && length(mx) == len
is_op(x...) = false
is_type(x,t::DataType) = typeof(x) == t
is_type_less(x,t::DataType) = typeof(x) <: t
mxprq(x) = is_type_less(x,AbstractMxpr)
is_number(x) = typeof(x) <: Number

# We check for :call repeatedly. We can optimize this later.
is_binary_minus(ex::Expr) = is_call(ex, :-, 3)
# number of args != 3 will pass through. But probably can't be used
is_division(ex::Expr) = is_call(ex, :mdiv,3)  
is_power(ex::Expr) = is_call(ex, :mpow)

# There is no binary minus and no division in Mxpr's.
rewrite_binary_minus(ex::Expr) = Expr(:call, :mplus, ex.args[2], Expr(:call,:(-),ex.args[3]))
rewrite_division(ex::Expr) = Expr(:call, :mmul, ex.args[2], Expr(:call,:mpow,ex.args[3],-1))
rewrite_binary_minus(mx::Mxpr) = mxpr(:mplus, mx[1], mxpr(:(-),mx[2]))
rewrite_division(mx::Mxpr) = mxpr(:mmul, mx[1], mxpr(:mpow,mx[2],-1))

# rewrite_expr : Expr -> Expr
# Input could be expression from cli. Output is closer to Mxpr form.
# Relative to Expr, Mxpr needs to encode more canonical semantics.
# Concrete example: a - b --> a + -b.
# We definitely need to dispatch on a hash query, or types somehow
function rewrite_expr(ex::Expr)
    if is_binary_minus(ex)  #  a - b --> a + -b.
        ex = rewrite_binary_minus(ex)
    elseif is_division(ex) # a / b --> a + b^(-1)
        ex = rewrite_division(ex)
    elseif is_call(ex, :Exp, 2)  # Exp(x) --> E^x
        ex = Expr(:call, :mpow, :E, ex.args[2])
    elseif is_call(ex,:Sqrt,2)
        ex = Expr(:call, :mpow, ex.args[2], SJRational(1//2))
    end
    return ex
end

# replace + with mplus, etc.
# We do this any expression parsed by Julia.
function replace_arithmetic_ops!(ex::Expr)
    for arg in ex.args
        replace_arithmetic_ops!(arg)
    end
    is_call(ex) ? ex.args[1] = jtomop(ex.args[1]) : nothing
    ex
end
replace_arithmetic_ops!(x) = x

# we need this always ?
@if_no_sjulia function set_symbol_self_eval(sym::Symbol)
    symquote = QuoteNode(sym)
    eval(:($sym = $symquote))
end

# Expr -> Mxpr
# Take a Expr, eg constructed from quoted input on cli, and construct an Mxpr.
function ex_to_mx!(ex::Expr)
    @mdebug(1,"ex_to_mx! start ", ex)
    ex = tryjeval(ex)  # compile it if we can
    is_type(ex,Expr) || return ex
    is_call(ex,://,3)  && is_number(ex.args[2]) && is_number(ex.args[3]) && return eval(ex)
    ex = rewrite_expr(ex)
    for i in 1:length(ex.args)
        ex.args[i] = ex_to_mx!(ex.args[i]) # convert to Mxpr at lower levels
    end
    local mxop, mxargs
    if ex.head == :call
        mxop = shift!(ex.args) # first arg is func, actually 'head' for Mxpr
        mxargs = ex.args
    else   # :hcat, etc
        mxop = ex.head
        mxargs = ex.args
    end
    @mdebug(20,"converting for printing: ", mxop, ", ", jtomop(mxop))
    mx = Mxpr{mxop}(mxop,mxargs,ex.head,false)  # expression not clean
end

@if_no_sjulia function ex_to_mx!(sym::Symbol) # if unbound, make symbol evaluate to itself
    if !isdefined(sym) 
        set_symbol_self_eval(sym)
    end
    sym
end

ex_to_mx!(x) = x

##  Convert a Mxpr to Expr.
# Note this does not revert changes that were made when constructing the mx.
# This is used for evaluation. Except, we will probably rarely evaluate directly
# with Julia eval.
# DANGER! Even if called on a copy. This works recursively, so if more than
# one ref to an object is in mx, it will be changed each time it is encountered,
# giving erroneous results
function mx_to_ex!(mx::Mxpr)
    error("mx_to_ex! should not be called!")
    ex = Expr(jhead(mx))
    a = jargs(mx)
    for i in 1:length(a)
        a[i] = mx_to_ex!(a[i]) # convert to Mxpr at lower levels
    end
    @mdebug(5,"mx_to_ex!: returning recursivley converted args: ", a)
    if jhead(mx) == :call
        unshift!(a,mtojop(head(mx)))  # identity now        
    else   # :hcat, etc
        nothing
    end    
    ex.args = a
    return ex
end
#mx_to_ex!(x) = x

# Hijack Julia display code until we can write our own.
# Conver Mxpr to Expr for printing.
function mx_to_ex(inmx::Mxpr)
    mx = deepcopy(inmx)
    @mdebug(50,"mx_to_ex: entering with won't print")    
    ex = Expr(jhead(mx))
    a = jargs(mx)
    for i in 1:length(a)
        a[i] = mx_to_ex(a[i]) # convert to Mxpr at lower levels
    end
    a = copy(a)
    @mdebug(50,"mx_to_ex: returning recursivley converted args: ", a)
    if jhead(mx) == :call
        unshift!(a,mtojop(head(mx)))  # We want Julia to *print* + instead of mplus
    else   # :hcat, etc
        nothing
    end    
    ex.args = a
    return ex
end

# Other things fall through. Though, there may be an expression lurking down there
mx_to_ex(x) = x

# Convert Mxpr to Expr for evaluation via eval()
function fast_mxpr_to_expr(inmx::Mxpr)
    @mdebug(5,"fast_mxpr_to_expr: ")
    mx = deepcopy(inmx)
    ex = Expr(jhead(mx))
    a = jargs(mx)
    if jhead(mx) == :call
        #        unshift!(a,mtojop(mhead(mx)))  # identity now
        #        println("using op ", mhead(mx))
        unshift!(a,head(mx))  #  We want Julia to call our subsitute functions
    else   # :hcat, etc
        nothing
    end        
    ex.args = a
    return ex
end

fast_jeval(mx::Mxpr) = eval(fast_mxpr_to_expr(mx))

###########################################
##  Function attributes                   #
###########################################

# The attribute 'orderless' means the function is commutative.  Binary
# ops are promoted to nary ops as in Julia. So orderless means we can
# put the args in a canonical order. We could use 'commutative'
# maybe. Using more accessible, or shorter, english terms perhaps
# better. We may want to restrict the attributes that a function may
# have.

# a key is a function name. the val is Dict holding attributes for that function
const MATTRIBUTES = Dict{Symbol,Dict{Symbol,Bool}}()

# eg get_attribute(:+, :orderless) --> true
function get_attribute(sym::Symbol,attr::Symbol)
    if haskey(MATTRIBUTES,sym)
        attrs = MATTRIBUTES[sym]
        if haskey(attrs,attr)
            return attrs[attr]
        end
    end
    return false
end

# get attribute function name from head of a particular expression
# But, we will try to use multiple dispatch instead, where possible.
get_attribute(mx::Mxpr,a) = get_attribute(head(mx),a)

# eg. set_attribute(:+, :orderless, true)
function set_attribute(sym::Symbol,attr::Symbol,val::Bool)
    local attrs
    if haskey(MATTRIBUTES,sym)
        attrs = get(MATTRIBUTES,sym)
    else
        attrs = Dict{Symbol,Bool}()
        MATTRIBUTES[sym] = attrs
    end
    attrs[attr] = val
end

# These also make up union type Orderless
for func in (:mplus, :mmul)
    set_attribute(func,:orderless,true)
end

#################################################
##  Alternates to Julia arithmetic functions    #
#################################################

# Use alternates to some Julia functions for MJulia.
# eg. integer division gives Rational or Integer.
# First we make dictionaries to look up alternate function.
# Then we define the alternate functions.
# These are used twice:
# 1) rewrite input expressions parsed by Julia
# 2) rewrite expression on output so we can use Julia show()

# Dicts to convert from Julia to MJulia symbol
const MTOJOP = Dict{Symbol,Symbol}()
const JTOMOP = Dict{Symbol,Symbol}()

for (j,m) in ((:/,:mdiv), (:*,:mmul), (:+,:mplus), (:^, :mpow))
    MTOJOP[m] = j
    JTOMOP[j] = m
end

# Following two are the interface
# Eg.  jtomop(:+) --> :mplus
function jtomop(jhead::Symbol)
    mhead = get(JTOMOP,jhead,0)
    return mhead == 0 ? jhead : mhead
end

# Eg.  mtojop(:mplus) --> :+
function mtojop(mhead::Symbol)
    jhead = get(MTOJOP,mhead,0)
    return jhead == 0 ? mhead : jhead
end    

# Divide and multiply integers and rationals
# like a CAS does. Always return exact result (no floats).
# Return Int if possible. We want to avoid infection Rationals
# and putting explicit conversions everywhere. Examples:
#  12/6 --> 2, not 2.0
#  13/6 --> 13//6
#  (13//6) * 6 --> 13
rat_to_int(r::Rational) = r.den == 1 ? r.num : r
mmul(x::Int, y::Rational) =  (res = x * y; return res.den == 1 ? res.num : res )
mmul(x::Rational, y::Int) =  (res = x * y; return res.den == 1 ? res.num : res )
mmul(x,y) = x * y
mplus(x::Rational, y::Rational) = rat_to_int(x+y)
mplus(x,y) = x + y
mdiv(x::Int, y::Int) =  rem(x,y) == 0 ? div(x,y) : x // y
mdiv(x::Int, y::Rational) = (res = x / y; return res.den == 1 ? res.num : res )
mdiv(x,y) = x/y
mpow(x::Integer,y::Integer) = y > 0 ? x^y : 1//(x^(-y))
mpow(x,y) = x^y

## copied from base/operators, a great trick
for op = (:mplus, :mmul)
    @eval begin
        # note: these definitions must not cause a dispatch loop when +(a,b) is
        # not defined, and must only try to call 2-argument definitions, so
        # that defining +(a,b) is sufficient for full functionality.
        ($op)(a, b, c)        = ($op)(($op)(a,b),c)
        ($op)(a, b, c, xs...) = ($op)(($op)(($op)(a,b),c), xs...)
        # a further concern is that it's easy for a type like (Int,Int...)
        # to match many definitions, so we need to keep the number of
        # definitions down to avoid losing type information.
    end
end

is_rat_and_int(x::Rational) = x.den == 1
is_rat_and_int(x) = false

#######################################################
## Methods for Mxpr's for Julia arithemtic functions  #
#######################################################

## FIXME
# This is the wrong way to do this. Calling Cos() should
# not run meval. Rather call methods of Cos() and return
# quoted form if we don't have a reduced form.
# All the Cos code at the bottom of this file will have to be
# modified.
# The code here is an attempt to get meval to run automatically,
# it works, but is really the wrong approach.
ordereval(x) = meval(deep_order_if_orderless!(x))
mxmkevalminus(args...) = ordereval(rewrite_binary_minus(mxpr(args...)))
mxmkevaldivision(args...) = ordereval(rewrite_division(mxpr(args...)))

mxmkeval(args...) = meval(mxpr(args...))
mxmkorderless(args...) = deep_order_if_orderless!(mxmkeval(args...))
let sym,str
    for str in ("*", "+")
        sym = symbol(str)
        mstr = string(jtomop(sym))
        @eval begin
            ($sym)(a::Symbolic, b::Symbolic, args...) = mxmkorderless(symbol($mstr),a,b,args...)
            ($sym)(a,b::Symbolic, args...) = mxmkorderless(symbol($mstr),a,b,args...)
            ($sym)(a::Symbolic, args...) = mxmkorderless(symbol($mstr),a,args...)
        end
    end
    for (str,f) in (("/", :mxmkevaldivision), ("-", :mxmkevalminus), ("^", :mxmkeval))
        sym = symbol(str)
        mstr = string(jtomop(sym))        
        @eval begin
            ($sym)(a::Symbolic, b::Symbolic) = ($f)(symbol($mstr),a,b)
            ($sym)(a::Symbolic, b::Integer) = ($f)(symbol($mstr),a,b)            
            ($sym)(a::Symbolic, b) = ($f)(symbol($mstr),a,b)
            ($sym)(a, b::Symbolic) = ($f)(symbol($mstr),a,b)  
        end
    end
end
-(a::Symbol) = mxmkeval(:-,a)
-(a::Mxpr) = -1 * a

############################################
##  Macros for constructing Mxpr easily    #
############################################

# Convert an expression to Mxpr (or number, or symbol...) Call this from
# within the two macros below, so that we don't need to quote input.
function transex(ex)
    local mx
    T = typeof(ex)
    if  T == Expr
        replace_arithmetic_ops!(ex)
        mx = ex_to_mx!(ex)
    elseif T == Symbol
        mx = ex
    else
        mx = ex  # Numbers, DataTypes, etc.
    end
    @mdebug(3,"transex: returning ",mx)
    return mx
end

# construct a Mxpr and evaluate
# Try to evaluate once.
macro sj(ex)
    mx = transex(ex)  # Translate Expr to Mxpr
    mx = meval(mx)    # order first or eval first ?
    mx = deep_order_if_orderless!(mx)
    # need this always for expressions in files    
    is_type(mx,Symbol) && return Base.QuoteNode(mx)
    mx = tryjeval(mx)  # We need this!
    mx
end

# Construct Mxpr, but don't evaluate
# This is (was) useful for debugging,
# or seeing Mxpr before any reordering or evaluation
macro sn(ex)
    transex(ex)
end

############################################
## Evaluate Mxpr                           #
############################################

# eval is "not a generic function". So we can't touch it.
# Do julia evaluation on Mxpr. This will often fail, for
# instance when there are unbound symbols.
# Ugh. we are doing a deep copy here. Need more sophisticated
# evaluation. Converting back to Mxpr would be faster, but
# still maybe not efficient. I wonder how to get inside eval ?
#jeval(mx::Mxpr) = eval(mx_to_ex!(deepcopy(mx)))
jeval(mx::Mxpr) = eval(mx_to_ex(mx))
jeval(x) = eval(x)

# Try Julia eval, else quietly return unevaluated input.
function tryjeval(mx::Union(Mxpr,Expr))
    res = try
        jeval(mx)
    catch
        mx
    end
    res
end
tryjeval(x) = x  # Don't seem to save time by letting these fall through

## Handlers for ops to be dispatched by meval
const MEVALOPFUNCS = Dict{Symbol,Function}()
_meval_has_handler(op::Symbol) = haskey(MEVALOPFUNCS,op)
_meval_get_handler(op::Symbol) = MEVALOPFUNCS[op]
_meval_call_handler(op::Symbol,mx::Mxpr) = (_meval_get_handler(op))(mx)

register_meval_func(op::Symbol, func::Function) = MEVALOPFUNCS[symbol(op)] = func
function meval_handle_or_fall_through(mx::Mxpr)
    if _meval_has_handler(mx[0])
        return _meval_call_handler(mx[0],mx)
    else
        return mx
    end
end

# In implementing the handlers, we are doing something different than
# other CAS's: We often use dynamical multiple dispatch instead if
# Dicts and branching. Eg., if an op takes two arguments, we call a
# helper function that passes the whole expression and the two
# expression arguments as arguments.  We have different methods for
# the different types of expression args.

## meval for :+ and :*
# If no operands are Mxpr, do nothing.
# If one or more operands are Mxpr and also of op :+
# Then flatten the operands, copying them out of the inner Mxpr
# FIXME: +a --> a
for (name,op) in ((:meval_plus,"mplus"),(:meval_mul,"mmul"))
    namestr = string(name)
    @eval begin
        function meval(mx::Mxpr{symbol($op)})
            @mdebug(1, $namestr, " entry: mx = ",mx)
            found_mxpr_term = false
            all_numerical_terms = true
            for i in 1:length(mx)  # check if there is at least one Mxpr of type op
                if mxprq(mx[i]) && mx[i][0] == symbol($op)
                    found_mxpr_term = true
                    all_numerical_terms = false
                    break
                end
                if !( typeof(mx[i]) <: Number )
                    all_numerical_terms = false
                end
            end
            if all_numerical_terms
                @mdebug(3, $namestr, ": all numerical terms mx = ", mx)                
                return fast_jeval(mx)  # convert to a julia :+ or :* expression and eval
            end
            found_mxpr_term == false && return mx
            nargs = mxmkargs()  # new args for the output            
            for i in 1:length(mx) # walk through all args again
                mxel = mx[i]
                if is_type(mxel,Mxpr{symbol($op)})
                    for j in 1:endof(mxel)  # got Mxpr of type op, copy elements
                        push!(nargs,mxel[j])
                    end
                else
                    push!(nargs,mx[i]) # something else, just it in
                end
            end
            newmx = Mxpr{symbol($op)}($op,nargs,:call,false) # construct new Mxpr
            return newmx
        end
#        register_meval_func(symbol($op),$name)  # register this function as handler
    end
end 

## meval for powers
#meval_pow(mx::Mxpr) = meval_pow(mx[1],mx[2],mx)
meval(mx::Mxpr{:mpow}) =  meval_pow(mx[1],mx[2],mx)
meval_pow(base::FloatingPoint, expt::FloatingPoint, mx::Mxpr) = base ^ expt
meval_pow(base::FloatingPoint, expt::Integer, mx::Mxpr) = base ^ expt
meval_pow(base::Union(FloatingPoint,Integer), expt::FloatingPoint, mx::Mxpr) = base ^ expt
meval_pow(base::Integer,expt::Integer,mx::Mxpr) = mpow(base,expt)
meval_pow(base::Integer,expt::Rational,mx::Mxpr) = mx  # Need to do more.
meval_pow(base,expt,mx::Mxpr) = mx  # generic case is to do nothing
#register_meval_func(:^,meval_pow)



## meval for assignment
function meval_assign(mx::Mxpr)
    @mdebug(3,"meval_assign entering: ",mx)
    ex = Expr(:(=))
    a = ex.args
    push!(a,mx[1])  # don't evaluate lhs of assignment
    push!(a,meval(mx[2]))  # meval the rhs
    @mdebug(3,"meval_assign made Expr: ",ex)
    eval(ex) # now do Julia eval to make the binding or assigment
end
register_meval_func(:(=),meval_assign)

## meval for division
meval_div(num::Number, den::Number, mx::Mxpr) = mdiv(num,den)
meval_div(num, den, mx::Mxpr) = mx
function meval_div(mx::Mxpr)
    @mdebug(3, "meval_div enter: mx ", mx)
    meval_div(mx[1],mx[2],mx)
end
register_meval_func(:/,meval_div)

## meval top level 
function meval(mx::Mxpr)
    length(mx) == 0 && return mx
    if mx[0] == :(=)
        return meval_assign(mx)
    end
    for i in 1:endof(mx)
        mx[i] = meval(mx[i])
        meval(mx[i])    # second eval ? Don't want this
    end
    return meval_handle_or_fall_through(mx)
end

# This is needed
function meval(sym::Symbol)
    @if_no_sjulia ! isdefined(sym) && return set_symbol_self_eval(sym)
    res =
        try
            eval(sym)
        catch
            sym
        end
    res
end

## generic meval does nothing
meval(x) = x 

## Called from src/interpreter.c
# This must be defined after the generic method for meval,
# because SJulia calls it on everything
sjulia_meval(x) = x

# FIXME , none of these are being called!
function sjulia_meval(mx::Mxpr)
    meval(mx)
end

function sjulia_meval(ex::Expr)
    mx = transex(ex)
    meval(mx)
end


############################################
## Display Mxpr                            #
############################################

# Display a Mxpr by displaying equivalent Expr. Don't show quotes on
# expression.
function Base.show(io::IO, mxin::Mxpr)
    mx = deepcopy(mxin)  # deep copy not expensive because this is cli output
    ex = mx_to_ex(mx)
    Base.show_unquoted(io,ex)    
end

@if_no_sjulia begin 
    const SHOW_QUOTED_SYMBOLS = [false]
    function show_quotes_on_symbols(val::Bool)
        SHOW_QUOTED_SYMBOLS[1] = val
    end
    # Don't show quote on symbol. This changes basic Julia behavior.
    Base.show(io::IO, s::Symbol) = Base.show_unquoted(io, SHOW_QUOTED_SYMBOLS[1] ?  QuoteNode(s) : s)
end    
    
###########################################################
## Canonical ordering of elements in orderless Mxpr       #
###########################################################

#_jslexless(x::Mxpr{:mmul},y::Mxpr{:mmul})  =  x[end]  < y[end]

const _jstypeorder = Dict{DataType,Int}()
const _jsoporder = Dict{Symbol,Int}()

# orderless (commutative) functions will have terms ordered from first
# to last according to this order of types. Then lex within types.
function _mklexorder()
    i = 1
    for typ in (Float64,Int,Any,Rational,Symbol,Expr,AbstractMxpr)
        _jstypeorder[typ] = i
        i += 1
    end
    i = 1
    for op in (:mplus,:mmul,:mpow)
        _jsoporder[op] = i
        i += 1
    end    
end

_mklexorder()

# interface: returns ordering precedence of Type, typ
function mxtypeorder(typ::DataType)
    ! haskey(_jstypeorder,typ)  && return 3  # Any
    return _jstypeorder[typ]
end

function mxoporder(op::Symbol)
    ! haskey(_jsoporder,op)  && return 4 # higher
    return _jsoporder[op]
end

function jslexless(x::Mxpr{:mpow}, y::Mxpr{:mpow})
    jslexless(base(x),base(y))  ||  jslexless(expt(x),expt(y))
end
jslexless(x::Mxpr{:mmul}, y::Mxpr{:mmul}) = jslexless(x[end],y[end])
jslexless(x::Mxpr{:mmul}, y::Mxpr{:mpow}) = jslexless(x[end],base(y))
jslexless(x::Mxpr{:mpow}, y::Mxpr{:mmul}) = jslexless(base(x),y[end])
function jslexless(x::Mxpr{:mpow}, y::Mxpr)
    if y == base(x)
        return is_type_less(expt(x),Real) && expt(x) < 0
    end
    jslexless(base(x),y)
end
function jslexless(x::Mxpr, y::Mxpr{:mpow})
    if x == base(y)
        return is_type_less(expt(y),Real) && expt(y) > 1
    end
    jslexless(x,base(y))
end
function jslexless(x::Mxpr{:mpow}, y::Symbol)
    if y == base(x)
        return is_type_less(expt(x),Real) && expt(x) < 0
    end
    jslexless(base(x),y)
end
function jslexless(x::Symbol, y::Mxpr{:mpow})
    if x == base(y)
        return is_type_less(expt(y),Real) && expt(y) > 1
    end
    jslexless(x,base(y))
end
jslexless(x::Mxpr{:mmul}, y::Symbol) = jslexless(x[end],y)
jslexless(x::Symbol, y::Mxpr{:mmul}) = jslexless(x,y[end])
jslexless(x::Mxpr{:mmul}, y::Mxpr) = jslexless(x[end],y)
jslexless(x::Mxpr, y::Mxpr{:mmul}) = jslexless(x,y[end])
jslexless(x::Symbol, y::Mxpr) = true
jslexless(x::Mxpr, y::Symbol) = false
function jslexless{T}(x::Mxpr{T},y::Mxpr{T})
    x === y && return false
    ax = margs(x)
    ay = margs(y)
    lx = length(ax)
    ly = length(ay)
    for i in 1:min(lx,ly)
        jslexless(ax[i],ay[i]) && return true
    end
    lx < ly && return true
    return false    
end
jslexless{T,V}(x::Mxpr{T},y::Mxpr{V}) = T < V
jslexless(x::Symbol, y::Mxpr) = true
# Following methods will only be called on non-Symbolic types.
_jslexless(x::DataType,y::DataType) = x <: y
_jslexless{T}(x::T,y::T) = lexless(x,y)  # use Julia definitions
jslexless{T}(x::T,y::T) = !(x === y) &&_jslexless(x,y) 
jslexless{T,V}(x::T,y::V) = mxtypeorder(T) < mxtypeorder(V)

# Order the args in orderless Mxpr.
function orderexpr!(mx::Mxpr)
    ar = jargs(mx)
    sort!(ar,lt=jslexless)
    set_order_clean(mx)
    mx
end

needs_ordering(mx::Mxpr) = get_attribute(mx,:orderless) && ! is_order_clean(mx)

function order_if_orderless!(mx::Mxpr)
    if needs_ordering(mx)
        @mdebug(3,"needs_ordering, ordering: ",mx)
        orderexpr!(mx)
        head(mx) == :mmul ? mx = compactmul!(mx) : nothing
        mxprq(mx) && head(mx) == :mplus ? mx = compactplus!(mx) : nothing
        @mdebug(4,"needs_ordering, done ordering and compact: ",mx)
    end
    mx
end
order_if_orderless!(x) = x

# Check the dirty bits of all all orderless subexpressions
function deep_order_if_orderless!(mx::Mxpr)
    for i = 1:length(mx)
        mx[i] = deep_order_if_orderless!(mx[i])
    end
    mx = order_if_orderless!(mx)
    is_rat_and_int(mx) && error("deep_order_if_orderless!: returning integer rational $mx")
    return mx
end
deep_order_if_orderless!(x) = x

##########################################################
## Sum collected numerical args in :+, (or same for :*)  #
##########################################################
# + and * are nary. Replace all numbers in the list of args, by one sum or product
for (fop,name,id) in  ((:mplus,:compactplus!,0),(:mmul,:compactmul!,1))
    @eval begin
        function ($name)(mx::Mxpr)
            @mdebug(1,"In ", $name)
            length(mx) < 2 && return mx
            a = margs(mx)
            typeof(a[1]) <: Number || return mx
            sum0 = a[1]
            while length(a) > 1
                shift!(a)
                typeof(a[1]) <: Number || break
                sum0 = ($fop)(sum0,a[1])
            end
            length(a) == 0 && return sum0            
            sum0 != $id && unshift!(a,sum0)
            $(fop == :mmul ? :(sum0 == 0 && return 0) : :())
            length(a) == 1 && return a[1]
            return mx
        end
    end
end

#function n_mul_a_plus_m_mul_b(an,bm)

# getterms(f(a,b,c),1:2) --> f(a,b)
# getterms(f(a,b,c),2:2) --> b
# TODO: support StepRange
function getterms(a::Mxpr,r::UnitRange)
    if abs(r.start-r.stop) == 0
        return a[r][1]
    else
        return mxpr(a[0],a[r]...)
    end
end

# has_numeric_coefficient(a::Mxpr{:mmul}) = is_type_less(a[1],Number)
# has_numeric_coefficient(x) = false
# numeric_coefficient(a::Mxpr{:mmul}) = has_numeric_coefficient(a) ? a[1] : 1
# numeric_coefficient(a::Mxpr{:mmul}) = has_numeric_coefficient(a) ? a[1] : 1

function numeric_coefficient(x::Mxpr{:mmul})
    local c::Number
    c = is_type_less(x[1],Number) ? x[1] : 1
end
numeric_coefficient(x::Number) = x
numeric_coefficient(x) = 1

function newfunc(a::Mxpr{:mmul},b::Mxpr{:mmul})
    if is_type_less(a[1],Number)
        if is_type_less(b[1],Number)
            if a[2:end] == b[2:end]
                println("Hi 1")                
                return (true,a[1]+b[1],getterms(a,2:length(a)))
            else
                println("Hi 2")
                return (false,0,a)
            end
        elseif a[2:end] == margs(b)
            println("Hi 3")
            return (true,a[1]+1,b)
        else
            println("Hi 4")            
            return (false,0,b)
        end
    elseif is_type_less(b[1],Number)
        if margs(a) == b[2:end]
            println("Hi 5")
            return (true,b[1]+1,a)
        else
            println("Hi 6")
            return (false,0,a)
        end
    elseif a == b
        println("Hi 7")
        return (true,2,a)
    else
        println("Hi 8")
        return (false,0,a)
    end
end

function newfunc(a::Mxpr{:mmul},b::Symbolic)
    if is_type_less(a[1],Number) &&
        getterms(a,2:length(a)) == b
            return (true,a[1]+1,b)
    else
        return (false,0,b)
    end
end

function newfunc{T}(a::T,b::T)
    a == b && return (true,2,a)
    return (false,0,a)
end


#Replace n repeated terms x by n*x, and factors x by x^n
for (op,name,id) in  ((:mplus,:collectplus!,0),(:mmul,:collectmul!,1))
    opstr = string(op)
    @eval begin
        function ($name)(mx::Mxpr)
            length(mx) < 2 && return mx
            a = margs(mx)
            n = 1
            count = 0
            coeffcount = 0
            while n < length(a)
                if a[n] == a[n+1]
                    count = 1                        
                    for i in (n+1):(length(a)-1)
                        if a[i] == a[i+1]
                            count += 1
                        else
                            break
                        end
                    end
#                    println("n=$n, count=$count")
                    newex = $(op== :mplus ?
                              :(mxpr(:mmul,count+1,a[n])) :
                              0)
                    println(newex)
                    splice!(a,n:n+count,[newex])
                end
                n += 1
            end
            return length(a) == 1 ? a[1] : mx
        end
    end
end


############################################
## Alternate math (trig, etc.) functions   #
############################################

# We do not want cos(1) to return floating point approximation, but
# rather to represent the exact value of the cosine of 1.
# We use Cos, for the name of the function that behaves as we like. Eg.
# Cos(1) --> Cos(1)
# Cos(1.0) --> 0.54...

# FIXME: We are calling meval here. This is not the right way.
# Define functions like, eg. Cos(x::Float64) = cos(x).
# You get the idea. If this idea works, then we complete the list
function mkmathfuncs() # Man, I hate looking for missing commas.
    func_list_string = "exp log cos cosh cosd sin sind sinh tan tand tanh"
    for s  in split(func_list_string)
        func = symbol(s)
        Funcstr = string(uppercase(s[1])) * s[2:end]
        Func = symbol(string(uppercase(s[1])) * s[2:end])
        set_attribute(Func,:numeric,true)        
        @eval begin
            ($Func){T<:FloatingPoint}(x::T) = return ($func)(x)
            ($Func){T<:FloatingPoint}(x::Complex{T}) = return ($func)(x)
            ($Func)(x) = mxmkeval(symbol($Funcstr),x)
            Base.@vectorize_1arg Number $Func  # only vectorizes over some numbers. Need our own macro
        end
    end
end
mkmathfuncs()

## TODO: Pi should be implmented like Julia MathConst
Cos_pi_coeff(mx::Mxpr{:Cos},c::Integer) = iseven(c) ? 1 : -1
Cos_pi_coeff(mx::Mxpr{:Cos},c::FloatingPoint) = cospi(c)
Cos_pi_coeff(mx::Mxpr{:Cos},c) = mx
function Cos_factor_arg(mx::Mxpr{:Cos},f1::Number,f2::Symbol)
    if f2 == :Pi
        return Cos_pi_coeff(mx,f1)
    else
        return mx
    end
end
Cos_factor_arg(mx::Mxpr{:Cos},f1,f2) = mx
meval_one_arg(mx::Mxpr{:Cos},arg::Symbol) = arg == :Pi ? -1 : mx
meval_one_arg(mx::Mxpr{:Cos},arg::Integer) = arg == 0 ? 0 : mx
function meval_one_arg(mx::Mxpr{:Cos},arg::Mxpr{:mmul})
    if length(arg) == 2
        return Cos_factor_arg(mx,margs(arg)...)
    else
        return mx
    end
end
meval_one_arg(mx::Mxpr{:Cos},x) = mx    
meval(mx::Mxpr{:Cos}) = length(mx) == 1 ? meval_one_arg(mx,mx[1]) : mx

# This is slow. Nothing smart implemented
# Of course, we are constructing the product and sorting the factors
# every time.
# [Cos(n * :Pi) for n in 1:10^5]
# z = 3*:Pi; [Cos(z) for n in 1.0:10^5]; is 5 times faster

@if_sjulia  sjulia_on()   # Turn on features defined in sjulia.jl
