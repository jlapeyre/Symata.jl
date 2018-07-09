### MittagLefflerE

@sjdoc MittagLefflerE """
    MittagLeffler(α,β,z)

    MittagLeffler(α,z)

represents the Mittag-Leffler function.
"""

## FIXME: organize this more like polylog is organized
## in general, we need a way to combine effective pattern matching with efficient dispatch.
## i.e. we want fewer `isa(...)`s.
## Maybe one of the Julia matching packages will do this for us.
## PatternDispatch.jl ?
## Match.jl appears to not write functions taking advantage of multiple dispatch
## Stefan mentioned something about ameliorating ambiguity hell in v0.6.
##
## for alpha == 0 , must have abs(z) < 1
## Mma does not return conditions, I think.
## We could optionally return conditions.

@mkapprule MittagLefflerE :nargs => 2:3

@doap MittagLefflerE(α,z) = mittagleffler(mx,α,z)

@doap function MittagLefflerE(α,β,z)
    β == 1  && return mittagleffler(mx,α,z)
    mittagleffler(mx,α,β,z)
end

function mittagleffler(mx,α,z)
    α == 1//2 && return Exp(mpow(z,2)) * Erfc(mminus(z))
    α == -1//2 && return Exp(mpow(z,2)) * Erfc(z)
    α == 0 && return mpow(mminus(1,z),-1)
    α == 1 && return Exp(z)
    α == 2 && return Cosh(Sqrt(z))
    α == 3 && return (1//3)* (Exp(mpow(z,(1//3))) + 2*Exp(-mpow(z,1//3)/2) * Cos(Sqrt(3)/2 * mpow(z,1//3)))
    α == 4 && return (1//2)* ( Cosh(mpow(z,1//4)) + Cos(mpow(z,1//4)))
    if (isa(α,Number) && isa(z, Number)) &&
        (isa(α,AbstractFloat) || isa(z, FloatRC))
        return MittLeff.mittleff(float(α),float(z))
    end
    z == 0 && return mpow(Gamma(β),-1)
    mx
end

function mittagleffler(mx,α,β,z)
    if (isa(α,AbstractFloat) || isa(β,AbstractFloat) || isa(z, FloatRC)) &&
        (isa(α,Number) && isa(β,Number) && isa(z, Number))
       return MittLeff.mittleff(float(α),float(β),float(z))
    end
    z == 0 && return mpow(Gamma(β),-1)
    if α == 1
        β == 2 && return (mmul(mplus(Exp(z),-1),mpow(z,-1)))
        return (mpow(:E,z)*mpow(z,mminus(1,β)) * mxpr(:GammaRegularized, mminus(β,1),0,z))
    end
    α == 2 && β == 2 && return mmul(Sinh(mpow(z,1//2)), mpow(mpow(z,1//2),-1))
    mx
end
