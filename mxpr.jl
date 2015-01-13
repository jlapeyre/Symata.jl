###########################################
##  Mxpr math expression type             #
###########################################

type Mxpr
    head::Symbol
    args::Array{Any,1}
    jhead::Symbol    # Julia head
    dirty::Bool
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
const MTOJHEAD = Dict{Symbol,Symbol}()
const JTOMHEAD = Dict{Symbol,Symbol}()

for (j,m) in ( (:/,:mdiv), (:*,:mmul))
    MTOJHEAD[m] = j
    JTOMHEAD[j] = m
end

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
##  END Alternates to Julia math functions #
############################################

# construct a Mxpr at the cli
# And evaluate
macro jm(ex)
    mx = exprtomxpr!(ex)
    tryjeval(mx)
end

# Construct Mxpr, but don't evaluate
macro jn(ex)
    mx = exprtomxpr!(ex)
end

# hmm not a better way ?
# display a Mxpr by displaying equivalent Expr
function Base.show(io::IO, mex::Mxpr)
    ex = mxprtoexpr(mex)
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

## Evaluate of Mxpr

# eval is "not a generic function".
# Do julia evaluation on Mxpr
jeval(mx::Mxpr) = eval(mxprtoexpr(mx))

# Try Julia eval, else return unevaluated input
function tryjeval(mx::Mxpr)
    res = try
        jeval(mx)
    catch
        mx
    end
    res
end
