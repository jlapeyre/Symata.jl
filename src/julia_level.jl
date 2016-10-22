# This is for testing using Symata code from Julia

for f in (:Expand, :Factor)
    @eval ($f)(mx::Mxpr) = apprules(mxpr($(QuoteNode(f)),mx))
    @eval ($f)(x) = x
    @eval export $f
end

# Need to handle two arg functions, etc.
for f in (:cos, :sin, :abs, :tan, :exp, :log, (:acos, :ArcCos) , (:asin, :ArcSin), (:atan, :ArcTan ),
          :cot, :cosh, :sinh, :tanh, :sqrt, :erf, :gamma)
    local uf
    if length(f) == 2
        uf = f[2]
        f = f[1]
    else        
        uf = Symbol(ucfirst(string(f)))
    end
    @eval ($uf)(x::AbstractFloat) = ($f)(x)
    @eval ($uf){T<:AbstractFloat}(x::AbstractArray{T}) = ($f)(x)
    @eval ($uf){T<:AbstractFloat}(x::Complex{T}) = ($f)(x)        
    @eval ($uf)(x) = apprules(mxpr($(QuoteNode(uf)),x))
    @eval export $uf
end

# Factor(mx::Mxpr) = apprules(mxpr(:Factor,mx))
# Factor(x) = x
# export Factor

Integrate(mx::Mxpr,symorlist) = apprules(mxpr(:Integrate,mx,symorlist))
export Integrate

const Pi = :Pi
export Pi

const E = e     # could make this the symbol ?
export E

# Already has a value
# I = im
# export I

# TODO: make an infix assignment operator... hm or  macro

# Do assigment in Symata and bind to julia symbol of the same name
# This is kinda broken !
# ERROR: UndefVarError: n not defined.
# But, the variable is defined in Main afterall
# We may not always want to define a variable in Main. But, it is hardcoded
# Usage:
# @aex x = 1
# Assigns x to 1 in symata, and x to :x in main
macro aex(e)
    quote
        @ex $(esc(e))
    end
    sym = e.args[1]
    symstr = string(sym)
    expr = :( $sym = Symbol($symstr) )
    Main.eval(expr)   # if we use Main., we get an 'n undefined error', but n is successfully defined.
end
