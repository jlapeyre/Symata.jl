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

# Convert Expr to Mxpr
# Take a Expr, eg constructed from quoted input on cli,
# and construct an Mxpr.
function exprtomxpr!(ex::Expr)
    hd = ex.head == :call ? ex.args[1] : ex.head
    a = ex.args
    for i in 1:length(a)
        a[i] = exprtomxpr!(a[i]) # convert to Mxpr at lower levels
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

# construct a Mxpr at the cli,
# and do some evaluation
macro jm(ex)
    mx = exprtomxpr!(ex)
    tryjeval(mx)
end

# Construct Mxpr, but don't evaluate
macro jn(ex)
    mx = exprtomxpr!(ex)
end


############################################
## Evaluate Mxpr                           #
############################################

# eval is "not a generic function". So we can't touch it.
# Do julia evaluation on Mxpr. This will often fail, for
# instance when there are unbound symbols.
jeval(mx::Mxpr) = eval(mxprtoexpr(mx))

# Try Julia eval, else quietly return unevaluated input.
function tryjeval(mx::Mxpr)
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

# Display a Mxpr by displaying equivalent Expr.  Temporarily replace
# alternate functions with usual functions and let Julia display them.
# Alters input temporarily!  Eg. an interrupt may leave mx altered.
#
# We do this because Julia code for display currently uses
# conditionals in a big function, finds bin ops based on precedence.
# Does not seem to be a good way to hook into this.

function Base.show(io::IO, mx::Mxpr)
    ex = mxprtoexpr(mx)
    func = :nothing
    if ex.head == :call
        func = ex.args[1]
        ex.args[1] = mtojhead(ex.args[1])
    end
    Base.show_unquoted(io,ex)
    if func != :nothing
        ex.args[1] = func
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
        @eval begin
            ($Func){T<:FloatingPoint}(x::T) = ($func)(x)
        end
    end
end
mkmathfuncs()
