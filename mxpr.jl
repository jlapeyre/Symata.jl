macro mdebug(level, a...)
    mxdebuglevel = -1  # larger means more verbose
    if level <= mxdebuglevel
        :(println($(a...)))
    else
        nothing
    end
end

##############################################
##  Mxpr type for symbolic math expression   #
##############################################

# This package is for creating and manipulating expressions. Using
# Julia Expr would be convenient, but seems not possible. Eg. we need
# a dirty bit to know eg if an expression is in canonical form. It's not
# clear how to organize the data. So: Do not access fields the
# directly!

# Mxpr.args is exactly the equivalent Julia args, if possible. This
# allows lightweight construction of Julia Expr for evaluation.
# Fortunately, Expr is mutable.  But, a better choice may be to pop
# the function name symbol from the front of the equivalent Julia
# args. We probably will not do direct Julia eval.

# Using a (singly) linked list vs. packed array of pointers (ie Array{Any}) for args
# are complementary. Retrieving an element is O(n) for linked lists, and O(1) for arrays.
# Splicing arguments is O(1) for linked lists and O(n) for arrays. Supporting in some
# way linked lists may be useful, eg, when lots of swapping parts happens in an internal
# routine.

type Mxpr
    head::Symbol     # Not sure what to use here. Eg. :call or :+, or ... ?
    args::Array{Any,1}
    jhead::Symbol    # Julia head. Redundant now.
    clean::Bool      # In canonical order ?
end
mxpr(h,a,j,d) = Mxpr(h,a,j,d)
# make an empty Mxpr
mxpr(s::Symbol) = Mxpr(s,Array(Any,0),:nothing,false)
ismxpr(x) = typeof(x) == Mxpr

# get Mxpr head and args
jhead(mx::Mxpr) = mx.jhead
jargs(mx::Mxpr) = mx.args
margs(mx::Mxpr) = mx.args
margs(ex::Expr) = ex.args  # sometimes Expr can stand in for Mxpr
mhead(mx::Mxpr) = mx.head
mhead(ex::Expr) = ex.head
#mhead(r::Rational) = Rational
#mhead(r::Float64) = Float64
mhead(x) = error("mhead: Can't take mhead of $x, of type $(typeof(x))")
setmhead(mx::Expr, val::Symbol) = mx.head = val

####  index functions

# Get and set parts of expressions. mx[0] is the head
# mx[1] is the first argument, etc.
function getindex(mx::Mxpr, k::Int)
    k == 0 && return mhead(mx)    
    if mhead(mx) == :(=)
        return margs(mx)[k]
    else
        return margs(mx)[k+1]
    end
end
function setindex!(mx::Mxpr, val, k::Int)
    if  mhead(mx) == :(=)
        return margs(mx)[k] = val
    end 
    k == 0 && return setmhead(mx,val)
    margs(mx)[k+1] = val
end

function Base.endof(mx::Mxpr)
    mhead(mx) == :(=)  && return length(margs(mx))    
    length(margs(mx)) - 1
end

function  Base.length(mx::Mxpr)
    mhead(mx) == :(=)  && return length(margs(mx))
    length(margs(mx)) - 1
end

# Currently the op is in position 1
# These look redundant how.
nummxargs(a::Array{Any,1}) = length(a) - 1
nummxargs(mx::Mxpr) = nummxargs(margs(mx))

# Do we want 'ordered' or 'clean' ? There is likely more than
# one way to be dirty, not just unordered.
getorderedflag(mx::Mxpr) = mx.clean
setorderedflag(mx::Mxpr,val::Bool) = (mx.clean = val)
isclean(mx::Mxpr) = mx.clean

function ==(a::Mxpr, b::Mxpr)
    (na,nb) = (length(a),length(b))
    na != nb && return false
    for i in 0:na
        a[i] != b[i] && return false
    end
    true
end

# Convert Expr to Mxpr
# Take a Expr, eg constructed from quoted input on cli,
# and construct an Mxpr.
function exprtomxpr!(ex::Expr)
#    println("exprtomxpr! Got expr $ex")    
    hd = ex.head == :call ? ex.args[1] : ex.head
    a = ex.args
    for i in 1:length(a)
        ex.args[i] = exprtomxpr!(ex.args[i]) # convert to Mxpr at lower levels
    end
    if ex.head == :call
        new_julia_call = jtomhead(hd)
        a[1] = new_julia_call
    end
    mx = mxpr(hd,a,ex.head,false)  # expression not clean
end

function exprtomxpr!(s::Symbol)
#    println("exprtomxpr! Got symbol $s")
    s
end

# everything other than Expr falls through
function exprtomxpr!(x)
#    println("exprtomxpr! Got unknown $x: type, " , typeof(x))
    x
end

##  Convert a Mxpr to Expr.
# Note this does not revert changes that were made when constructing the mx.
# This is used for evaluation. Except, we will probably rarely evaluate directly
# with Julia eval.
function mxprtoexpr(mx::Mxpr)
    ex = Expr(jhead(mx))
    a = jargs(mx)
    for i in 1:length(a)
        a[i] = mxprtoexpr(a[i]) # convert to Mxpr at lower levels
    end    
    ex.args = a
    return ex
end
# Other things fall through
mxprtoexpr(x) = x

#  Convert a Mxpr to Expr reverting alternate functions to normal Julia functions
#  This is used for display.
function mxprtoexpr_revert(mx::Mxpr)
    ex = Expr(jhead(mx))
    a = jargs(mx)
    for i in 1:length(a)
        a[i] = mxprtoexpr_revert(a[i]) # convert to Mxpr at lower levels
    end    
    ex.args = a
    if ex.head == :call
        ex.args[1] = mtojhead(ex.args[1])
    end    
    return ex
end
# Other things fall through
mxprtoexpr_revert(x) = x

function fast_mxpr_to_expr(mx::Mxpr)
    ex = Expr(jhead(mx))
    ex.args = jargs(mx)
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
        else
            return false
        end
    else
        return false
    end
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

###########################################
##  Alternates to Julia math functions    #
###########################################

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
function jtomhead(jhead::Symbol)
    mhead = get(JTOMHEAD,jhead,0)
    return mhead == 0 ? jhead : mhead
end

# Eg.  mtojhead(:mplus) --> :+
function mtojhead(mhead::Symbol)
    jhead = get(MTOJHEAD,mhead,0)
    return jhead == 0 ? mhead : jhead
end    

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

is_rat_and_int(x::Rational) = x.den == 1
is_rat_and_int(x) = false

############################################
##  Macros for constructing Mxpr easily    #
############################################

# Convert an expression to Mxpr (or number, or symbol...) Call this from
# within the two macros below, so that we don't need to quote input.
function transex(ex)
    local mx
    T = typeof(ex)
    if  T == Expr
        mx = exprtomxpr!(ex)
    elseif T == Symbol
#        mx = Expr(:quote,ex)
        mx = ex
    else
        mx = ex  # Numbers, DataTypes, etc.
    end
    return mx
end

# construct a Mxpr at the cli,
# and do some evaluation.
macro jm(ex)
    mx = transex(ex)
    mx = meval(mx)
#    println("@jm after eval $mx")
    mx = deep_order_if_orderless!(mx)
#    println("done deep order @jm $mx")
    if  typeof(mx) == Symbol
        return Base.QuoteNode(mx)
    end
    mx = tryjeval(mx)
    mx
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
jeval(mx::Mxpr) = eval(mxprtoexpr(deepcopy(mx)))
jeval(x) = eval(x)

# Not using this now!
# Try Julia eval, else quietly return unevaluated input.
function tryjeval(mx)
    goodflag = true
    res = try
        jeval(mx)
    catch
        mx
    end
    res
end

## Register handlers for ops to be dispatched by meval
const MEVALOPFUNCS = Dict{Symbol,Function}()
register_meval_func(op::Symbol, func::Function) = MEVALOPFUNCS[symbol(op)] = func

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
            for i in 1:nummxargs(mx)  # check if there is at least one Mxpr of type op
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
            nargs = Any[symbol($op)]  # new args for the output
            for i in 1:nummxargs(mx)
                mxel = mx[i]
                if typeof(mxel) == Mxpr && mxel[0] == symbol($op)
                    for j in 1:endof(mxel)  # got Mxpr of type op, copy elements
                        push!(nargs,mxel[j])
                    end
                else
                    push!(nargs,mx[i]) # something else, just it in
                end
            end
            newmx = mxpr($op,nargs,:call,false) # construct new Mxpr
            return newmx
        end
        register_meval_func(symbol($op),$name)  # register this function as handler
    end
end 

## meval for powers
meval_pow(mx::Mxpr) = meval_pow(mx[1],mx[2],mx)
meval_pow(base,expt,mx::Mxpr) = mx  # generic case is to do nothing
meval_pow(base::Number, expt::Number, mx::Mxpr) = base^expt  # treat numbers
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
    nummxargs(mx) == 0 && return mx
    if mx[0] == :(=)
        return meval_assign(mx)
    end
    for i in 1:endof(mx)
#        println("pre eval: $i:  $mx, :: $(mx[i])")
        res = meval(mx[i])
#        println("resutl of reval *$i* is $res")
        mx[i] = res
        meval(mx[i])        
#        println("pose eval: $i,  $mx, :: $(mx[i])")        
    end
    if  haskey(MEVALOPFUNCS,mx[0])  # meval specialized on the operator
        mx = MEVALOPFUNCS[mx[0]](mx)
    end
#    println("Done with meval: returning $mx, of type $(typeof(mx))")
    return mx
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
    ex = mxprtoexpr_revert(mx)  # use original Julia function names
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

_jslexless(x,y) = lexless(x,y)

function _jslexless(x::Union(Mxpr,Expr),y::Union(Mxpr,Expr))
    mhead(x) != mhead(y) && return mhead(x) < mhead(y)
    ax = margs(x)
    ay = margs(y)
    lx = length(ax)
    ly = length(ay)
    for i in 1:min(lx,ly)
        _jslexless(ax[i],ay[i]) && return true
    end
    lx < ly && return true
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
    return _jslexless(x,y)
end


# Order the args in orderless Mxpr.
# Now it is a disadvantage that the op is in the args array.
# We have to remove op to avoid sorting it.
function orderexpr!(mx::Mxpr)
#    println(" getting args $mx")        
    ar = jargs(mx)
#    println("args are $ar")            
    op = shift!(ar)
#    println("done shift op is $op")                
#    println("starting sort sort or $ar")    
    sort!(ar,lt=jslexless)
#    println(" done sort")        
    unshift!(ar,op)
    setorderedflag(mx,true)
    mx
end

function needs_ordering(mx::Mxpr)
#    println("needs_ordering: $mx")    
    get_attribute(mx,:orderless) && ! getorderedflag(mx)
end

function order_if_orderless!(mx::Mxpr)
    if needs_ordering(mx)
#        println(" starting ordering $mx")        
        orderexpr!(mx)
#        println(" done ordering $mx")
        mhead(mx) == :* ? mx = compactmul!(mx) : nothing
#        println("maybe did compactmul! $mx")        
        ismxpr(mx) && mhead(mx) == :+ ? mx = compactplus!(mx) : nothing
#        println("maybe compactplus! $mx")
    end
#    println("order_if_orderless! returning $mx")    
    mx
end
order_if_orderless!(x) = x

# Check the dirty bits of all all orderless subexpressions
function deep_order_if_orderless!(mx::Mxpr)
    for i = 1:length(mx)
#        println("deep_order, entering level with $(mx[i])")        
        mx[i] = deep_order_if_orderless!(mx[i])
    end
#    println("deep_order, done with levels $mx")
    mx = order_if_orderless!(mx)
#    println("deep_order, done with top level $mx")
    is_rat_and_int(mx) && error("deep_order_if_orderless!: returning integer rational $mx")
    return mx
end
deep_order_if_orderless!(x) = x

##########################################################
## Sum collected numerical args in :+, (or same for :*)  #
##########################################################
# sum numbers at end of + or * call
for (fop,name) in  ((:+,:compactplus!),(:*,:compactmul!))
    @eval begin
        function ($name)(mx::Mxpr)
#            println($name, ": $mx")
            nummxargs(mx) < 2 && return mx
            a = margs(mx)
            typeof(a[end]) <: Number || return mx
            sum0 = a[end]
            while length(a) > 1
                pop!(a)
                typeof(a[end]) <: Number || break
                sum0 = ($fop)(sum0,a[end])
            end
#            println($name, ": finishing $sum0, length ", length(a))
            length(a) == 1 && return sum0
#            println($name, ": pushing op $a back to front")            
            push!(a,sum0)
#            println($name, ": returing whole thing")
            return mx
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

# Define functions like, eg. Cos(x::Float64) = cos(x).
# You get the idea. If this idea works, then we complete the list
function mkmathfuncs() # Man, I hate looking for missing commas.
    func_list_string = "exp log cos cosh cosd sin sind sinh tan tand tanh lambertw"
    for s  in split(func_list_string)
        func = symbol(s)
        Func = symbol(string(uppercase(s[1])) * s[2:end])
        set_attribute(Func,:numeric,true)        
        @eval begin
            function ($Func){T<:FloatingPoint}(x::T)
                return ($func)(x)
            end

        end
    end
end
mkmathfuncs()