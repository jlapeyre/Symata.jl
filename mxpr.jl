const MXDEBUGLEVEL = 0 # debug level, larger means more verbose. -1 is off
include("./mxpr_util.jl")

#type MParseError <: Exception  Not really a parse error
#    msg::String
#end
#Base.showerror(io::IO,e::MParseError) = print(io,e.msg)

##############################################
##  Mxpr type for symbolic math expression   #
##############################################

# This package is for creating and manipulating expressions. Using
# Julia Expr would be convenient, but seems not possible. Eg. we need
# a dirty bit to know eg if an expression is in canonical form. It's
# not clear how to organize the data. So: Do not access fields the
# directly!

# Mxpr.args is exactly the equivalent Julia args, if possible. This
# allows lightweight construction of Julia Expr for evaluation.
# Fortunately, Expr is mutable.  But, a better choice may be to pop
# the function name symbol from the front of the equivalent Julia
# args. We probably will not do direct Julia eval.

# Using a (singly) linked list (LL) vs. packed array of pointers (ie
# Array{Any}) for args are complementary. Retrieving an element is
# O(n) for LLs, and O(1) for arrays.  Splicing arguments is
# O(1) for LLs and O(n) for arrays. Supporting LLs
# in some way may be useful, eg, when lots of swapping parts happens
# in an internal routine. Maxima uses singly linked lists

# We share the Julia symbol table, rather than managing our own
# symbol table.

type Mxpr
    head::Symbol     # Not sure what to use here. Eg. :call or :+, or ... ?
    args::Array{Any,1}
    jhead::Symbol    # Actual exact Julia head: :call, :comparison, etc.
    clean::Bool      # In canonical order ?
end

typealias Symbolic Union(Mxpr,Symbol)

mxpr(h,a,jh,d) = Mxpr(h,a,jh,d)
# make an empty Mxpr, unused ?
mxpr(s::Symbol) = Mxpr(s,Array(Any,0),:nothing,false)

## Following is OK in principle, but nothing in it yet
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
# For constructing Mxpr from scratch, we make up a Julia head
moptojhead(x) = :call   

## Construct Mxpr, similar to construction Expr(head,args...)
#  Set dirty bit. Guess corresponding Julia Expr field 'head'
function mxpr(op::Symbol,args...)
    theargs = Array(Any,0)
    for x in args
        push!(theargs,x)
    end
    mx = Mxpr(op,theargs,moptojhead(op),false)
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

## Is expr a call to symbol op (this is expression, typ may not be bound)
macro mk_call_predicate(name0,op)
    name = mk_predicate_sym(name0)
    @eval begin
        ($name)(x::Expr) = (x.head == :call && x.args[1] == $op)
    end
end

macro mk_type_predicate(name0,typ)
    name = mk_predicate_sym(name0)
    @eval begin
        ($name)(x::Expr) = typeof(x) == $typ
    end
end

mxprq(x) = typeof(x) == Mxpr

# get Mxpr head and args
jhead(mx::Mxpr) = mx.jhead
jargs(mx::Mxpr) = mx.args
margs(mx::Mxpr) = mx.args
#margs(ex::Expr) = ex.args  # sometimes Expr can stand in for Mxpr
mhead(mx::Mxpr) = mx.head
#mhead(ex::Expr) = ex.head
mhead(x) = error("mhead: mhead not defined for $x, of type $(typeof(x))")
setmhead(mx::Expr, val::Symbol) = mx.head = val

####  index functions

# Get and set parts of expressions. mx[0] is the head
# mx[1] is the first argument, etc.
getindex(mx::Mxpr, k::Int) = return k == 0 ? mhead(mx) : margs(mx)[k]
setindex!(mx::Mxpr, val, k::Int) = k == 0 ? setmhead(mx,val) : (margs(mx)[k] = val)

Base.endof(mx::Mxpr) = length(margs(mx))
Base.length(mx::Mxpr) = length(margs(mx))
Base.length(s::Symbol) = 0  # Very useful in codes. Symbol is really a simple Mxpr

# Do we want 'ordered' or 'clean' ? There is likely more than
# one way to be dirty, not just unordered.
getorderedflag(mx::Mxpr) = mx.clean
setorderedflag(mx::Mxpr,val::Bool) = (mx.clean = val)
isclean(mx::Mxpr) = mx.clean

# This is deep ==, I think
function ==(a::Mxpr, b::Mxpr)
    (na,nb) = (length(a),length(b))
    na != nb && return false
    for i in 0:na
        a[i] != b[i] && return false
    end
    true
end

# Same thing is somewhere in base
is_call(ex::Expr, op::Symbol) = ex.head == :call && ex.args[1] == op
is_call(ex::Expr, op::Symbol, len::Int) = ex.head == :call && ex.args[1] == op && length(ex.args) == len
is_op(mx::Mxpr, op::Symbol) = mhead(mx) == op
is_op(mx::Mxpr, op::Symbol, len::Int) = mhead(mx) == op && length(mx) == len
is_op(x...) = false
is_type(x,t::DataType) = typeof(x) == t

# We check for :call repeatedly. We can optimize this later.
is_binary_minus(ex::Expr) = is_call(ex, :-, 3)
# number of args != 3 will pass through. But probably can't be used
is_division(ex::Expr) = is_call(ex, :/,3)  
is_power(ex::Expr) = is_call(ex, :^)

# rewrite_expr : Expr -> Expr
# Input could be expresion from cli. Output is closer to Mxpr form.
# Relative to Expr, Mxpr needs to encode more canonical semantics.
# Concrete example: a - b --> a + -b.
# We definitely need to dispatch on a hash query, or types somehow
function rewrite_expr(ex::Expr)
    if is_binary_minus(ex)  #  a - b --> a + -b.
        ex = Expr(:call, :+, ex.args[2], Expr(:call,:(-),ex.args[3]))
    elseif is_division(ex) # a / b --> a + b^(-1)
        ex = Expr(:call, :*, ex.args[2], Expr(:call,:(^),ex.args[3],-1))
    elseif is_call(ex, :Exp, 2)  # Exp(x) --> E^x
        ex = Expr(:call, :^, :E, ex.args[2])
    elseif is_call(ex,:Sqrt,2)
        ex = Expr(:call, :^, ex.args[2], 1//2)
    end
    return ex
end

# Expr -> Mxpr
# Take a Expr, eg constructed from quoted input on cli, and construct an Mxpr.
function ex_to_mx!(ex::Expr)
    @mdebug(1,"ex_to_mx! start ", ex)
    if ex.head == :(->)  # Try to compile anonymous functions now.
        f = tryjeval(ex) # If it succeeds, we are done with this expr.
        typeof(f) == Function && return f
    end
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
    mx = Mxpr(mxop,mxargs,ex.head,false)  # expression not clean
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
        unshift!(a,mtojhead(mhead(mx)))  # identity now        
    else   # :hcat, etc
        nothing
    end    
    ex.args = a
    return ex
end
# Other things fall through. Though, there may be an expression lurking down there
mx_to_ex!(x) = x

function mx_to_ex(inmx::Mxpr)
    mx = deepcopy(inmx)
    @mdebug(5,"mx_to_ex: entering with won't print")    
    ex = Expr(jhead(mx))
    a = jargs(mx)
    for i in 1:length(a)
        a[i] = mx_to_ex(a[i]) # convert to Mxpr at lower levels
    end
    a = copy(a)
    @mdebug(5,"mx_to_ex: returning recursivley converted args: ", a)
    if jhead(mx) == :call
        unshift!(a,mtojhead(mhead(mx)))  # identity now        
    else   # :hcat, etc
        nothing
    end    
    ex.args = a
    return ex
end

# Other things fall through. Though, there may be an expression lurking down there
mx_to_ex(x) = x

function fast_mxpr_to_expr(inmx::Mxpr)
    @mdebug(5,"fast_mxpr_to_expr: ")
    mx = deepcopy(inmx)
    ex = Expr(jhead(mx))
    a = jargs(mx)
    if jhead(mx) == :call
        unshift!(a,mtojhead(mhead(mx)))  # identity now        
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
get_attribute(mx::Mxpr,a) = get_attribute(mhead(mx),a)

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

for func in ( :+, :* )
    set_attribute(func,:orderless,true)
end

#################################################
##  Alternates to Julia arithmetic functions    #
#################################################

# Use alternates to some Julia functions for MJulia.
# eg. integer division gives Rational or Integer.
# First we make dictionaries to look up alternate function.
# Then we define the alternate functions.

# Dicts to convert from Julia to MJulia symbol
const MTOJHEAD = Dict{Symbol,Symbol}()
const JTOMHEAD = Dict{Symbol,Symbol}()

for (j,m) in ( (:/,:mdiv), (:*,:mmul))
    MTOJHEAD[m] = j
    JTOMHEAD[j] = m
end

# Following two are the interface
# Eg.  jtomhead(:+) --> :mplus
function jtomop(jhead::Symbol)
    mhead = get(JTOMHEAD,jhead,0)
    return mhead == 0 ? jhead : mhead
end

# # Eg.  mtojhead(:mplus) --> :+
# function mtojhead(mhead::Symbol)
#     jhead = get(MTOJHEAD,mhead,0)
#     return jhead == 0 ? mhead : jhead
# end    

jtomhead(x) = x
mtojhead(x) = x

# Divide and multiply integers and rationals
# like a CAS does. Always return exact result (no floats).
# Return Int if possible. We want to avoid infection Rationals
# and putting explicit conversions everywhere. Examples:
#  12/6 --> 2, not 2.0
#  13/6 --> 13//6
#  (13//6) * 6 --> 13
mmul(x::Int, y::Rational) =  (res = x * y; return res.den == 1 ? res.num : res )
mmul(x::Rational, y::Int) =  (res = x * y; return res.den == 1 ? res.num : res )
mmul(x,y) = x * y
mdiv(x::Int, y::Int) =  rem(x,y) == 0 ? div(x,y) : x // y
mdiv(x::Int, y::Rational) = (res = x / y; return res.den == 1 ? res.num : res )
mdiv(x,y) = x/y
mpow(x::Integer,y::Integer) = y > 0 ? x^y : 1//(x^(-y))

is_rat_and_int(x::Rational) = x.den == 1
is_rat_and_int(x) = false

############################################
## Julia-level functions for Mxpr's        #
############################################

mxmkeval(args...) = meval(mxpr(args...))
mxmkorderless(args...) = deep_order_if_orderless!(mxmkeval(args...))
let sym,str
    for str in ("*", "+")
        sym = symbol(str)
        @eval begin
            ($sym)(a::Symbolic, b::Symbolic, args...) = mxmkorderless(symbol($str),a,b,args...)
            ($sym)(a,b::Symbolic, args...) = mxmkorderless(symbol($str),a,b,args...)
            ($sym)(a::Symbolic, args...) = mxmkorderless(symbol($str),a,args...)
        end
    end
    for str in ("/", "-", "^")
        sym = symbol(str)
        @eval begin
            ($sym)(a::Symbolic, b::Symbolic) = mxmkeval(symbol($str),a,b)
            ($sym)(a::Symbolic, b::Integer) = mxmkeval(symbol($str),a,b)            
            ($sym)(a::Symbolic, b) = mxmkeval(symbol($str),a,b)
            ($sym)(a, b::Symbolic) = mxmkeval(symbol($str),a,b)  
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
        mx = ex_to_mx!(ex)
    elseif T == Symbol
        mx = ex
    else
        mx = ex  # Numbers, DataTypes, etc.
    end
    @mdebug(3,"transex: returning ",mx)
    return mx
end

# construct a Mxpr at the cli,
# Try to evaluate once.
macro jm(ex)
    mx = transex(ex)
    mx = meval(mx)
    mx = deep_order_if_orderless!(mx)
    typeof(mx) == Symbol && return Base.QuoteNode(mx)
    mx = tryjeval(mx)  # need this. but maybe not here
end

# Construct Mxpr, but don't evaluate
# This is useful for debugging, or seeing Mxpr before any reordering or evaluation
macro jn(ex)
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
for (name,op) in ((:meval_plus,"+"),(:meval_mul,"*"))
    @eval begin
        function ($name)(mx::Mxpr)
            @mdebug(1,$name, " entry: mx = ",mx)
            found_mxpr_term = false
            all_numerical_terms = true
            for i in 1:length(mx)  # check if there is at least one Mxpr of type op
                if typeof(mx[i]) == Mxpr && mx[i][0] == symbol($op)
                    found_mxpr_term = true
                    all_numerical_terms = false
                    break
                end
                if !( typeof(mx[i]) <: Number )
                    all_numerical_terms = false
                end
            end
            if all_numerical_terms
                @mdebug(3, $name, ": all numerical terms mx = ", mx)                
                return fast_jeval(mx)  # convert to a julia :+ or :* expression and eval
            end
            found_mxpr_term == false && return mx
#            nargs = Any[symbol($op)]  # new args for the output     ***************
            nargs = Any[]  # new args for the output            
            for i in 1:length(mx) # walk through all args again
                mxel = mx[i]
                if typeof(mxel) == Mxpr && mxel[0] == symbol($op)
                    for j in 1:endof(mxel)  # got Mxpr of type op, copy elements
                        push!(nargs,mxel[j])
                    end
                else
                    push!(nargs,mx[i]) # something else, just it in
                end
            end
            newmx = Mxpr($op,nargs,:call,false) # construct new Mxpr
            return newmx
        end
        register_meval_func(symbol($op),$name)  # register this function as handler
    end
end 

## meval for powers
meval_pow(mx::Mxpr) = meval_pow(mx[1],mx[2],mx)
meval_pow(base::FloatingPoint, expt::FloatingPoint, mx::Mxpr) = base ^ expt
meval_pow(base::FloatingPoint, expt::Integer, mx::Mxpr) = base ^ expt
meval_pow(base::Union(FloatingPoint,Integer), expt::FloatingPoint, mx::Mxpr) = base ^ expt
meval_pow(base::Integer,expt::Integer,mx::Mxpr) = mpow(base,expt)
meval_pow(base::Integer,expt::Rational,mx::Mxpr) = mx  # Need to do more.
meval_pow(base,expt,mx::Mxpr) = mx  # generic case is to do nothing
register_meval_func(:^,meval_pow)

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
        meval(mx[i])    # second eval ?
    end
    return meval_handle_or_fall_through(mx)
end

function meval(s::Symbol)
#    println(" meval evaling symbol $s")
    res =
        try
            eval(s)
        catch
            s
        end
    if typeof(res) == Function
#        println("Got Funciton ! $res")
    end
#    println("meval: symbol evaled to $res")    
    res
end

## generic meval does nothing
function meval(x)
#    println(" evaling unknown $x, type: ", typeof(x))
    x
end

############################################
## Display Mxpr                            #
############################################

# Display a Mxpr by displaying equivalent Expr. Don't show quotes on
# expression.
function Base.show(io::IO, mxin::Mxpr)
    mx = deepcopy(mxin)  # deep copy not expensive because this is cli output
#    ex = mx_to_ex!_revert(mx)  ******************  # use original Julia function names
    ex = mx_to_ex(mx)
    Base.show_unquoted(io,ex)    
end

# Don't show quote on symbol. This changes basic Julia behavior.
# Or, we could make our own symbol type: MSymbol.
Base.show(io::IO, ex::Symbol) = Base.show_unquoted(io, ex)

###########################################################
## Lexicographical ordering of elements in orderless Mxpr #
###########################################################

const _jslexorder = Dict{DataType,Int}()

# orderless (commutative) functions will have terms ordered from first
# to last according to this order of types. Then lex within types.
function _mklexorder()
    i = 1
    for typ in (Symbol,Expr,Mxpr,Rational,Any,Int,Float64)
        _jslexorder[typ] = i
        i += 1
    end
end

_mklexorder()

# interface: returns ordering precedence of Type, typ
function mxlexorder(typ::DataType)
    ! haskey(_jslexorder,typ)  && return 5  # Any
    return _jslexorder[typ]
end

#mxlexorder(x) = false

_jslexless(x,y) = lexless(x,y)
_jslexless(x::DataType,y::DataType) = x <: y

# we need these because type annotations are compared
# isless{T}(::Type{T}, ::Type{T}) = false
# function isless(::Int64, ::Int64)
#     println("In is less int64")
#     return false
# end

# function isless(::Int64, a::Symbol)
#     println("In is less int64 and dymbol")
#     return false
# end

# function isless(a::Symbol, ::Int64)
#     println("In is less int64 and dymbol")
#     return false
# end

function _jslexless(x::Union(Mxpr,Expr),y::Union(Mxpr,Expr))
    mhead(x) != mhead(y) && return mhead(x) < mhead(y)
    ax = margs(x)
    ay = margs(y)
    lx = length(ax)
    ly = length(ay)
    for i in 1:min(lx,ly)
#        println("_jslexless: trying $i th parts")
#        println(" ", ax[i],"  ", ay[i])
        _jslexless(ax[i],ay[i]) && return true
    end
#    if typeof(lx) == DataType && typeof(ly) == DataType
#        lx <: ly && return true
#    else
        lx < ly && return true
#    end
    return false
end

# comparision function for sort routine
function jslexless(x,y)
    tx = typeof(x)
    ty = typeof(y)
#    println("cmp '$x' '$y'")
    if tx != ty
        return mxlexorder(tx) < mxlexorder(ty)
    end
#    if tx == DataType && ty == DataType
#        return x <: y
#    end
    return _jslexless(x,y)
end


# Order the args in orderless Mxpr.
# Now it is a disadvantage that the op is in the args array.
# We have to remove op to avoid sorting it.
function orderexpr!(mx::Mxpr)
    ar = jargs(mx)
    sort!(ar,lt=jslexless)
    setorderedflag(mx,true)
    mx
end

function needs_ordering(mx::Mxpr)
#    println("needs_ordering: $mx")    
    get_attribute(mx,:orderless) && ! getorderedflag(mx)
end

function order_if_orderless!(mx::Mxpr)
    if needs_ordering(mx)
        @mdebug(3,"needs_ordering, ordering: ",mx)
        orderexpr!(mx)
        mhead(mx) == :* ? mx = compactmul!(mx) : nothing
        mxprq(mx) && mhead(mx) == :+ ? mx = compactplus!(mx) : nothing
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
for (fop,name,id) in  ((:+,:compactplus!,0),(:mmul,:compactmul!,1))
#    altop = jtomop(fop)
    @eval begin
        function ($name)(mx::Mxpr)
            length(mx) < 2 && return mx
            a = margs(mx)
            typeof(a[end]) <: Number || return mx
            sum0 = a[end]
            while length(a) > 1
                pop!(a)
                typeof(a[end]) <: Number || break
                sum0 = ($fop)(sum0,a[end]) # call alternate Julia func
            end
            length(a) == 0 && return sum0            
            sum0 != $id && push!(a,sum0)            
            length(a) == 1 && return a[1]
            return mx
        end
    end
end



# Replace n repeated terms x by n*x, and factors x by x^n
# for (fop,name,id) in  ((:+,:collectplus!,0),(:*,:collectmul!,1))
#     altop = jtomop(fop)
#     @eval begin
#         function ($name)(mx::Mxpr)
#             length(mx) < 2 && return mx
#             a = margs(mx)
#             n = 1
#             while length(a) > 2
#                 dupflag = false
#                 firstdupflag = false
#                 count = 0
#                 if n < length(a)
#                     if a[n] == a[n+1]
#                 for i in n:(length(a)-1)
#                     if dupflag
#                         if a[i] == a[i+1]
#                             count += 1
#                         else
#                             break
#                         end
#                     else
#                         if a[i] == a[i+1]
#                             firstdupflag = true
#                             dupflag = true
#                             count = 1
#                         end

#                     end
#                     else
#                     end
#             end
#             length(a) == 0 && return sum0            
#             sum0 != $id && push!(a,sum0)            
#             length(a) == 1 && return a[1]
#             return mx
#         end
#     end
# end



############################################
## Alternate math (trig, etc.) functions   #
############################################

# We do not want cos(1) to return floating point approximation, but
# rather to represent the exact value of the cosine of 1.
# We use Cos, for the name of the function that behaves as we like. Eg.
# Cos(1) --> Cos(1)
# Cos(1.0) --> 0.54...

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

function meval_Cos(cmx::Mxpr)
    if length(cmx) == 1
        mx = cmx[1]
        if length(mx) == 2 && is_op(mx,:*,2) && mx[1] == :Pi
            typeof(mx[2]) <: Integer  && return iseven(mx[2]) ? 1 : -1
            typeof(mx[2]) <: FloatingPoint && return cospi(mx[2])
        end
    end
    return cmx
end
register_meval_func(:Cos,meval_Cos)


############################################
## Apply, Map, etc.
############################################

# function meval_vcat(mx::Mxpr)
#     @mdebug(1,"meval_vcat: enter ",mx)
#     println("length ", length(mx))
#     if length(mx) == 0 return Any[]
#     end
#     mx
# end
# register_meval_func(:vcat,meval_vcat)
