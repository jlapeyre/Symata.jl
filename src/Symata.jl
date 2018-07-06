__precompile__()
module Symata

using Compat
import Compat.String
import Compat.view

import Base: setindex!, getindex, replace

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
    eval(Main, parse("""
       import Symata: @testex, symval, symname, setsymval, @aex, meval, doeval, infseval, getpart, setpart!,
                      sympy, mpmath, pytypeof, mxpr, canonexpr!, wrap_symata
"""))
end

export devimport

export  @testex, symval, symname, setsymval, @aex, meval, doeval, infseval, getpart, setpart!,
sympy, mpmath, pytypeof, mxpr, canonexpr!, wrap_symata
   
export name, typename

# These are for IJulia. We could probably import insymata in the interface code instead.
export isymata, insymata

include("version.jl")
include("util.jl")
include("sjcompat.jl")
include("mxpr.jl")
include("attributes.jl")
include("mxpr_util.jl")
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
include("autoload.jl")
include("namedparts.jl")
include("AST_translation.jl")
include("alteval.jl")
include("doc.jl")
include("misc_doc.jl")
include("symbols.jl")
include("random.jl")
include("numcomb.jl")
include("kernelstate.jl")
include("evaluation.jl")
include("comparison_logic.jl")
include("test.jl")
include("wrapout.jl")
include("formatting/Formatting.jl")
include("predicates.jl")
include("symata_julia_interface.jl")
include("measurements.jl")
include("flowcontrol.jl")
include("output.jl")
include("system.jl")
include("IO.jl")
include("latex.jl")
include("pattern.jl")
## experimental, eventually replace pattern with this.
#include("pattern2.jl")
include("parts.jl")
include("lists.jl")
include("stats.jl")
#include("table.jl")
include("table_dyn.jl")
include("expressions.jl")
include("algebra.jl")
include("expanda.jl")
include("flatten.jl")
include("sortorderless.jl")
include("module.jl")
include("strings.jl")
include("wrappers.jl")
include("keyword_translation.jl")
include("sympy.jl")
include("mittleff.jl")
include("LambertW.jl")
include("math_functions.jl")
include("functionzeros.jl")
include("sympy_application.jl")
include("alternate_syntax.jl")
include("matrix.jl")
include("numerical.jl")
include("protected_symbols.jl")
include("REPL_symata.jl")
include("client_symata.jl")
include("isymata.jl")
include("plot.jl")
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
