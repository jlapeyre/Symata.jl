# VERSION >= v"0.4.0-dev+6521" && __precompile__()
# compiling does not really work.
# 1. must comment out RunSJuliaREPL() at end of sjulia_repl.jl
#    otherwise compilation fails
# 2. Very often (eg. 1+1), we find
#    ERROR: TypeError: subtype: expected Type{T}, got Int64
#    But, repeat the command 3 or 4 times and it works, with no error.
# 3. Integrate, which calls SymPy, causes a segfault

module SJulia

import Base: /, *, +, -, ^, setindex!, getindex
export @ex, @testex, symval, symname

include("mxpr_util.jl")
include("mxpr_type.jl")
include("sjiterator.jl")
include("sortpattern.jl")
include("arithmetic.jl")
include("julia_level_math.jl")
include("mxpr_top.jl")
include("julia_level.jl")
include("namedparts.jl")
include("parse.jl")
include("evaluation.jl")
include("expression_utils.jl")
include("alteval.jl")
include("doc.jl")
include("misc_doc.jl")
include("predicates.jl")
include("apprules.jl")
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
include("protected_symbols.jl")
include("math_functions.jl")
include("strings.jl")
include("sympy.jl")
include("sympy_application.jl")
include("code_in_SJulia.jl")   # This file loads slowly because it has to jit a lot of code
include("sjulia_repl.jl")

function __init__()
    if isinteractive() RunSJuliaREPL()  end # this will be needed if we get compilation working
end
    
end # module SJulia

nothing
