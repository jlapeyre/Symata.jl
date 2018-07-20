__precompile__(true)

module Symata

import MacroTools
import SpecialFunctions
import REPL
import Markdown
import InteractiveUtils
import Base: setindex!, getindex, replace
#import Base64
import Dates
import Formatting

export @symExpr, @extomx
export @sym, symeval, symtranseval, setsymata, getsymata, mxpr, mxpra, Mxpr, symprintln,
       unpacktoList, mhead, margs, newargs, symparseeval, symparsestring, sjtopy, pytosj,
       isympy
export mmul, mplus, mpow, mabs, mminus, symatamath
export debugmxpr

"""
    devimport()

import some symbols from `Symata` into `Main` that are useful for development.
"""
function devimport()
    Core.eval(Main, Meta.parse("""
       import Symata: @testex, symval, symname, setsymval, meval, doeval, infseval, getpart, setpart!,
                      sympy, mpmath, pytypeof, mxpr, canonexpr!, wrap_symata
"""))
end

export devimport

# FIXME: why export these *and* have devimport ?
export  @testex, symval, symname, setsymval, meval, doeval, infseval, getpart, setpart!,
sympy, mpmath, pytypeof, mxpr, canonexpr!, wrap_symata

## Set const debugging parametres at compile-time in debug.jl
include("debug.jl") # must use include here, because @inc is defined in debug.jl
@inc("LambertW.jl")  # remove this when Pkg.jl is better developed.
@inc("version.jl")
@inc("util.jl")
@inc("sjcompat.jl")
@inc("mxpr.jl")
@inc("attributes.jl")
@inc("mxpr_util.jl")
@inc("exceptions.jl")
@inc("level_specification.jl")
@inc("sequence_specification.jl")
@inc("sjiterator.jl")
@inc("sortpattern.jl")
@inc("AST_translation_tables.jl")
@inc("julia_level.jl")
@inc("arithmetic.jl")
@inc("apprules_core.jl")
@inc("constants.jl")
@inc("autoload.jl")
@inc("namedparts.jl")
@inc("AST_translation.jl")
@inc("alteval.jl")
@inc("doc.jl")
@inc("misc_doc.jl")
@inc("symbols.jl")
@inc("random.jl")
@inc("numcomb.jl")
@inc("kernelstate.jl")
@inc("evaluation.jl")
@inc("comparison_logic.jl")
@inc("test.jl")
@inc("wrapout.jl")
#@inc("formatting/Formatting.jl")
@inc("predicates.jl")
@inc("symata_julia_interface.jl")
@inc("measurements.jl")
@inc("flowcontrol.jl")
@inc("Output.jl")
@inc("system.jl")
@inc("IO.jl")
@inc("latex.jl")
@inc("pattern.jl")
## experimental, eventually replace pattern with this.
#@inc("pattern2.jl")
@inc("parts.jl")
@inc("lists.jl")
@inc("stats.jl")
#@inc("table.jl")
@inc("table_dyn.jl")
@inc("expressions.jl")
@inc("algebra.jl")
@inc("expanda.jl")
@inc("flatten.jl")
@inc("lexcmp.jl")
@inc("sortorderless.jl")
@inc("module.jl")
@inc("strings.jl")
@inc("wrappers.jl")
@inc("keyword_translation.jl")
@inc("sympy.jl")
@inc("math_functions.jl")
# @inc("mittleff_glue.jl") moved mittag-leffler to external dependence. But, it's not registered.
@inc("functionzeros.jl")
@inc("sympy_application.jl")
@inc("alternate_syntax.jl")
@inc("matrix.jl")
@inc("numerical.jl")
@inc("protected_symbols.jl")
@inc("REPL_symata.jl")
@inc("client_symata.jl")
@inc("Jupyter/Jupyter.jl")
@inc("plot.jl")
@inc("docautoloaded.jl")
@inc("function.jl")

import .Jupyter
import .Jupyter: isymata
import .Jupyter: is_isymata_mode
# These are for IJulia. We could probably import insymata in the interface code instead.
export isymata, insymata

function __init__()
    do_init()
end

is_loaded_IJulia() = isdefined(Main, :IJulia)

function do_init()
    init_sympy()
    if ! isdefined(Test, :testset_forloop)
        Core.eval(Main, :(macro testset(expr) end)) # Compatibility for versions more recent than around 0.5 from May 2016
    end
    sjimportall(:System, :Main) # These are Symata "contexts"
    set_current_context(:Main)
    if is_loaded_IJulia() # Use the Jupyter interface
        Jupyter.isymata()
    elseif isinteractive() # Started perhaps from the Julia REPL
        if isdefined(Base, :active_repl)
            RunSymataREPL(Base.active_repl)
        else
            nothing
        end
    else
        nothing
    end
end

end # module Symata

nothing
