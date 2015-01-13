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
    dirty::Bool      # In canonical order ?
end
mxpr(h,a,j,d) = Mxpr(h,a,j,d)
# make an empty Mxpr
mxpr(s::Symbol) = Mxpr(s,Array(Any,0),:nothing,false)

# get Mxpr head and args
jhead(mx::Mxpr) = mx.jhead
jargs(mx::Mxpr) = mx.args
margs(mx::Mxpr) = mx.args
mhead(mx::Mxpr) = mx.head

# Convert Expr to Mxpr
# Take a Expr, eg constructed from quoted input on cli,
# and construct an Mxpr.
function exprtomxpr!(ex::Expr)
    hd = ex.head == :call ? ex.args[1] : ex.head
    a = ex.args
    for i in 1:length(a)
        ex.args[i] = exprtomxpr!(ex.args[i]) # convert to Mxpr at lower levels
    end
    if ex.head == :call
        new_julia_call = jtomhead(hd)
        a[1] = new_julia_call
    end
    mx = mxpr(hd,a,ex.head,true)
end

# everything other than Expr falls through
exprtomxpr!(x) = x

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

const MATTRIBUTES = Dict{Symbol,Dict{Symbol,Bool}}()

function get_attribute(sym::Symbol,attr::Symbol)
    if haskey(MATTRIBUTES,sym)
#        println("attrs Key is there")        
        attrs = MATTRIBUTES[sym]
#        println("Got the dict $attrs")
        if haskey(attrs,attr)
#            println("atribute is there $attrs")            
            return attrs[attr]
        else
            return false
        end
    else
        return false
    end
end

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
        mx = Expr(:quote,ex)
    else
        mx = ex
    end
    return mx
end


# construct a Mxpr at the cli,
# and do some evaluation
macro jm(ex)
    mx = tryjeval(transex(ex))
    ordermaybe!(mx)
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
jeval(mx::Mxpr) = eval(mxprtoexpr(mx))
jeval(x) = eval(x)

# Try Julia eval, else quietly return unevaluated input.
function tryjeval(mx)
    res = try
        jeval(mx)
    catch
        mx
    end
    res
end

############################################
## Display Mxpr                            #
############################################

# Display a Mxpr by displaying equivalent Expr. Don't show quotes on
# expression.
function Base.show(io::IO, mxin::Mxpr)
    mx = deepcopy(mxin)
    ex = mxprtoexpr_revert(mx)  # use original Julia function names
    Base.show_unquoted(io,ex)    
end

# Don't show quote on symbol
Base.show(io::IO, ex::Symbol) = Base.show_unquoted(io, ex)

##############################################
## Lexicographical ordering of elements in Mxpr
##############################################

const _jslexorder = Dict{DataType,Int}()

function mklexorder()
    i = 1
    for typ in (Float64,Int,Rational,Symbol,Expr)
        _jslexorder[typ] = i
        i += 1
    end
end

mklexorder()

_jslexless(x,y) = lexless(x,y)

function _jslexless(x::Mxpr,y::Mxpr)
    jhead(x) !=  jhead(y) && return jhead(x) < jhead(y)
    ax = jargs(x)
    ay = jargs(y)
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
    mx
end

function ordermaybe!(mx::Mxpr)
    get_attribute(mx,:orderless) && orderexpr!(mx)
    mx
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
        @eval begin
            ($Func){T<:FloatingPoint}(x::T) = ($func)(x)
        end
    end
end
mkmathfuncs()
