VERSION >= v"0.4.0-dev+6521" && __precompile__()

module Symata

using Compat  # only used in code copied from REPL_symata.jl
import Compat.String
import Compat.view

# We don't want to import the math functions.... If we don't import * we can write a symata version
# that only works in Symata module.
import Base: setindex!, getindex, replace

#import Base: /, *, +, -, ^, setindex!, getindex, replace

# These are of general use
# Need to export @ex until we can track down all uses and remove them.
# Prefer @sym
export @sym, doeval, @ex
export mmul, mplus, mpow, mabs, mminus, symatamath

# For development
function devimport()
    eval(Main, parse("""
import Symata: @ex, @testex, symval, symname, setsymval, @aex, meval, doeval, infseval, getpart, setpart!,
       sympy, pytypeof, mxpr, canonexpr!
"""))
end

export devimport

# For IJulia. We could probably import insymata in the interface code instead.
export isymata, insymata

include("sjcompat.jl")
include("early_kernelstate.jl")
include("mxpr_util.jl")
include("mxpr_type.jl")
include("exceptions.jl")
include("level_specification.jl")
include("sequence_specification.jl")
include("sjiterator.jl")
include("sortpattern.jl")
include("AST_translation_tables.jl")
include("julia_level.jl")
include("arithmetic.jl")
include("apprules_core.jl")
include("symataconstants.jl")
include("namedparts.jl")
include("AST_translation.jl")
include("alteval.jl")
include("doc.jl")
include("misc_doc.jl")
include("symbols.jl")
include("apprules.jl")
include("random.jl")
include("kernelstate.jl")
include("evaluation.jl")
include("comparison_logic.jl")
include("test.jl")
include("wrapout.jl")
include("IO.jl")
include("predicates.jl")
include("symata_julia_interface.jl")
include("measurements.jl")
include("flowcontrol.jl")
include("output.jl")
include("latex.jl")
include("pattern.jl")
include("parts.jl")
include("lists.jl")
#include("table.jl")
include("table_dyn.jl")
include("expressions.jl")
include("expanda.jl")
include("flatten.jl")
include("sortorderless.jl")
include("module.jl")
include("strings.jl")
include("wrappers.jl")
include("keyword_translation.jl")
include("sympy.jl")
include("mittleff.jl")
include("math_functions.jl")
include("sympy_application.jl")
include("matrix.jl")
include("numerical.jl")
include("protected_symbols.jl")
include("REPL_symata.jl")
include("client_symata.jl")
include("isymata.jl")
include("plot.jl")
include("autoload.jl")
include("docautoloaded.jl")
include("function.jl")

function __init__()
    have_ijulia = isdefined(Main, :IJulia)
    init_sympy()
    if ! isdefined(Base.Test, :testset_forloop)
        eval(Main, :(macro testset(expr) end ))   # Compatibility for versions more recent than around 0.5 from May 2016
    end
    Main.eval( :( using Symata ))
    sjimportall(:System, :Main)
    set_current_context(:Main)
    if have_ijulia
        isymata()
    elseif isinteractive()
        if isdefined(Base, :active_repl)
            RunSymataREPL(Base.active_repl)
        else
            Symata_start()
            exit()
        end
    else
        nothing
    end
end

end # module Symata

nothing
