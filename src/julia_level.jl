# This is for testing using SJulia code from Julia
Expand(mx::Mxpr) = apprules(mxpr(:Expand,mx))
Expand(x) = x
export Expand

Cos(x) = apprules(mxpr(:Cos,x))
export Cos

Pi = :Pi
export Pi

# TODO: make an infix assignment operator... hm or  macro

# Do assigment in SJulia and bind to julia symbol of the same name
# This is kinda broken !
# ERROR: UndefVarError: n not defined.
# But, the variable is defined in Main afterall
# We may not always want to define a variable in Main. But, it is hardcoded
# Usage:
# @aex x = 1
# Assigns x to 1 in sjulia, and x to :x in main
macro aex(e)
    quote
        @ex $(esc(e))
    end
    sym = e.args[1]
    symstr = string(sym)
    expr = :( $sym = Symbol($symstr) )
    Main.eval(expr)   # if we use Main., we get an 'n undefined error', but n is successfully defined.
end
