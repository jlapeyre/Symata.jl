VERSION >= v"0.4.0-dev+6521" && __precompile__()

module SJulia

import Base: /, *, +, -, ^, setindex!, getindex, replace
export @ex, @testex, symval, symname, @aex, meval, doeval, infseval

# For development
export sympy, pytypeof

include("sjcompat.jl")
include("early_kernelstate.jl")
include("mxpr_util.jl")
include("mxpr_type.jl")
include("exceptions.jl")
include("sjiterator.jl")
include("sortpattern.jl")
include("julia_level_math.jl")
include("AST_translation_tables.jl")
include("julia_level.jl")
include("arithmetic.jl")
include("apprules_core.jl")
include("sjuliaconstants.jl")
include("namedparts.jl")
include("AST_translation.jl")
include("expression_utils.jl")
include("alteval.jl")
include("doc.jl")
include("misc_doc.jl")
include("symbols.jl")
include("apprules.jl")
include("kernelstate.jl")
include("evaluation.jl")
include("comparison_logic.jl")
include("IO.jl")
include("predicates.jl")
include("sjulia_julia_interface.jl")
include("measurements.jl")
include("flowcontrol.jl")
include("output.jl")
include("match_and_replace.jl")
include("pattern.jl")
include("parts.jl")
include("lists.jl")
include("expressions.jl")
include("flatten.jl")
include("sortorderless.jl")
include("module.jl")
# include("trig.jl")  # this is done is sympy
include("strings.jl")
include("wrappers.jl")
include("sympy.jl")
include("math_functions.jl")
include("sympy_application.jl")
#include("LambertW.jl") # not yet used
include("protected_symbols.jl")
include("REPL_sjulia.jl")
include("client_sjulia.jl")

function __init__()
    init_sympy()
    if ! isdefined(Base.Test, :testset_forloop)
        eval(Main, :(macro testset(expr) end ))   # Compatibility for versions more recent than around 0.5 from May 2016
    end
    Main.eval( :( using SJulia ))
    sjimportall(:System, :Main)
    set_current_context(:Main)
    if isinteractive()
        if isdefined(Base, :active_repl)
            RunSJuliaREPL(Base.active_repl)
        else
            SJulia_start()
            exit()
        end
    else
        nothing
    end
end

end # module SJulia

nothing
