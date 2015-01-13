include("./mxpr_util.jl")

##############################################
##  Mxpr type for symbolic math expression   #
##############################################

# Using Julia Expr would be convenient, but seems not possible.
# Eg. we need a dirty bit to know eg if expression is canonicalized.
# It's not clear how to organize the data. So:
# Do not access fields the directly!
# Mxpr.args is exactly the equivalent Julia args, if possible.
# This allows lightweight construction of Julia Expr for evaluation.
# Fortunately, Expr is mutable.

type Mxpr
    head::Symbol     # Not sure what to use here. Eg. :call or :+, or ... ?
    args::Array{Any,1}
    jhead::Symbol    # Julia head. Redundant now.
    clean::Bool      # In canonical order ?
end
mxpr(h,a,j,d) = Mxpr(h,a,j,d)
# make an empty Mxpr
mxpr(s::Symbol) = Mxpr(s,Array(Any,0),:nothing,false)

# get Mxpr head and args
jhead(mx::Mxpr) = mx.jhead
jargs(mx::Mxpr) = mx.args
margs(mx::Mxpr) = mx.args
mhead(mx::Mxpr) = mx.head
margs(ex::Expr) = ex.args  # sometimes Expr can stand in for Mxpr
mhead(ex::Expr) = ex.head
setmhead(mx::Expr, val::Symbol) = mx.head = val

function getindex(mx::Mxpr, k::Int)
    k == 0 && return mhead(mx)
    return margs(mx)[k+1]
end

function setindex!(mx::Mxpr, val, k::Int)
    k == 0 && return setmhead(mx,val)
    margs(mx)[k+1] = val
end

# Currently the op is in position 1
nummxargs(a::Array{Any,1}) = length(a) - 1
nummxargs(mx::Mxpr) = nummxargs(margs(mx))

getorderedflag(mx::Mxpr) = mx.clean
setorderedflag(mx::Mxpr,val::Bool) = (mx.clean = val)

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
# This is used for evaluation.
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
# This is used for display
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



###########################################
##  Function attributes                   #
###########################################

# The attribute 'orderless' means the function is commutative.
# Binary ops are promoted to nary ops as in Julia. So orderless
# means we can put the args in a canonical order.

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

# get attribute function name from a particular instance of a call
get_attribute(mx::Mxpr,a) = get_attribute(mhead(mx),a)

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
function jtomhead(jhead::Symbol)
    mhead = get(JTOMHEAD,jhead,0)
    return mhead == 0 ? jhead : mhead
end

function mtojhead(mhead::Symbol)
    jhead = get(MTOJHEAD,mhead,0)
    return jhead == 0 ? mhead : jhead
end    

# Divide and multiply integers and rationals
# like a CAS. Always return exact result (no floats).
# Return Int if possible. We want to avoid infection Rationals
# and putting explicit conversions everywhere.
mmul(x::Int, y::Rational) =  (res = x * y; return res.den == 1 ? res.num : res )
mmul(x::Rational, y::Int) =  (res = x * y; return res.den == 1 ? res.num : res )
mmul(x,y) = x * y

mdiv(x::Int, y::Int) =  rem(x,y) == 0 ? div(x,y) : x // y
mdiv(x::Int, y::Rational) = (res = x / y; return res.den == 1 ? res.num : res )
mdiv(x,y) = x/y

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
#    mx = tryjeval(transex(ex))
    order_if_orderless!(mx)
    if  typeof(mx) == Symbol
        return Base.QuoteNode(mx)
    end
    mx
end

# Construct Mxpr, but don't evaluate
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

# need to make an iterator over the args
function meval(mx::Mxpr)
    nummxargs(mx) == 0 && return mx
    a = margs(mx)
    for i in 1:nummxargs(mx)
        mx[i] = meval(mx[i])
    end
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

##############################################
## Lexicographical ordering of elements in Mxpr
##############################################

const _jslexorder = Dict{DataType,Int}()

# orderless (commutative) functions will have terms ordered from first
# to last according to this order of types. Then lex within types.
function mklexorder()
    i = 1
    for typ in (Symbol,Expr,Rational,Int,Float64)
        _jslexorder[typ] = i
        i += 1
    end
end

mklexorder()

_jslexless(x,y) = lexless(x,y)

function _jslexless(x::Union(Mxpr,Expr),y::Union(Mxpr,Expr))
    mhead(x) !=  mhead(y) && return mhead(x) < mhead(y)
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

function jslexless(x,y)
    tx = typeof(x)
    ty = typeof(y)
    if tx != ty
        return _jslexorder[tx] < _jslexorder[ty]
    end
    return _jslexless(x,y)
end

# Now it is a disadvantage that the op is in the args array.
# We have to remove op to avoid sorting it.
function orderexpr!(mx::Mxpr)
    ar = jargs(mx)
    op = shift!(ar)
    sort!(ar,lt=jslexless)
    unshift!(ar,op)
    setorderedflag(mx,true)
    mx
end

function order_if_orderless!(mx::Mxpr)
    if get_attribute(mx,:orderless) && ! getorderedflag(mx)
        orderexpr!(mx)
        mhead(mx) == :* ? mx = compactmul!(mx) : nothing
        mhead(mx) == :+ ? mx = compactplus!(mx) : nothing        
    end
    mx
end
order_if_orderless!(x) = x

###################################################
## Perform op on numerical args to  :* and :+     #
###################################################
# sum numbers at end of + or * call
for (fop,name) in  ((:+,:compactplus!),(:*,:compactmul!))
    @eval begin
        function ($name)(mx::Mxpr)
            nummxargs(mx) < 2 && return mx
            a = margs(mx)
            typeof(a[end]) <: Number || return mx
            sum0 = a[end]
            while length(a) > 1
                pop!(a)
                typeof(a[end]) <: Number || break
                sum0 = ($fop)(sum0,a[end])
            end
            length(a) == 1 && return sum0
            push!(a,sum0)
            return mx
        end
    end
end

############################################
## Alternate math (trig, etc.) functions   #
############################################

# We do not want cos(1) to return floating point approximation, but
# rather to represent the exact value of the cosine of 1.
# We use Cos, for the name of the function that behaves as we like

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

