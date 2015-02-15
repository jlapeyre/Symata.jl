using Base.Test

@ex TimeOff()   # don't print hundreds of diagnostic lines
include("trig_exp_test.jl")
include("somemath_test.jl")
include("flowcontrol_test.jl")
include("evaluation_test.jl")
include("expressions_test.jl")
include("arithmetic_test.jl")
include("downvalues_test.jl")
include("mxpr_test.jl")
include("orderless_test.jl")
include("patterns_test.jl")
include("simple_expression_test.jl")
