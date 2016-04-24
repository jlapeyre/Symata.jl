VERSION >= v"0.4.0-dev+6521" && __precompile__()

module SJulia

import Base: /, *, +, -, ^, setindex!, getindex
export @ex, @testex, symval, symname, @aex, meval, doeval, infseval

include("mxpr_util.jl")
include("mxpr_type.jl")
include("sjiterator.jl")
include("sortpattern.jl")
include("julia_level_math.jl")
include("AST_translation_tables.jl")
include("julia_level.jl")
include("arithmetic.jl")
include("sjuliaconstants.jl")
include("namedparts.jl")
include("AST_translation.jl")
include("evaluation.jl")
include("expression_utils.jl")
include("alteval.jl")
include("doc.jl")
include("misc_doc.jl")
include("apprules.jl")
include("file_IO.jl")
include("predicates.jl")
include("sjulia_julia_interface.jl")
include("measurements.jl")
include("flowcontrol.jl")
include("output.jl")
include("mpatrule.jl")
include("pattern.jl")
include("parts.jl")
include("lists.jl")
include("expressions.jl")
include("flatten.jl")
include("sortorderless.jl")
include("module.jl")
include("trig.jl")
include("math_functions.jl")
include("strings.jl")
include("sympy.jl")
include("sympy_application.jl")
include("LambertW.jl")
include("protected_symbols.jl")
include("REPL_sjulia.jl")
include("client_sjulia.jl")

#include("code_in_SJulia.jl")   # This file probably conflicts with sympy math functions

function sjulia_repl()  # This code can be moved elsewhere
    isdefined(SJulia, :active_repl) && return SJulia.active_repl
    isdefined(Base, :active_repl) && return Base.active_repl
    error("Can't find the active REPL.")
end

# For now, julia and sjulia share history. So, either mode 1 or 2 is ok
sjulia_repl_mode() = sjulia_repl().interface.modes[1]

sjulia_repl_history() = sjulia_repl_mode().hist

function __init__()
    init_sympy()
    setsymval(:ShowSymPyDocs!, true)  # show sympy docs for 'functions' if available.
    setsymval(:ReturnSymPy!, false)
    Main.eval( :( using SJulia ))

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
