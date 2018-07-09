__precompile__()
module Symata

using Compat
import Compat.String
import Compat.view
import MacroTools
import LambertW
import SpecialFunctions
import REPL
import Markdown
import InteractiveUtils
import Base: setindex!, getindex, replace
import Base64


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

## Set const debugging parametres at compile-time in debug.jl
include("debug.jl")

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
@inc("symataconstants.jl")
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
@inc("formatting/Formatting.jl")
@inc("predicates.jl")
@inc("symata_julia_interface.jl")
@inc("measurements.jl")
@inc("flowcontrol.jl")
@inc("output.jl")
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
@inc("isymata.jl")
@inc("plot.jl")
@inc("docautoloaded.jl")
@inc("function.jl")

function __init__()
   # do_init()
end

export symata

"""
    symata()

"""
symata() = do_init()

function do_init()
    println("*************  Entering __init__**************")
    flush(stdout)
#    exit()
    have_ijulia = isdefined(Main, :IJulia)
    init_sympy()
    println("*************  sympy initialized **************")
    if ! isdefined(Test, :testset_forloop)
        eval(Main, :(macro testset(expr) end ))   # Compatibility for versions more recent than around 0.5 from May 2016
    end
    println("*************  using Symata **************")
#    Main.eval( :( using Symata ))
    println("*************  importing **************")    
    sjimportall(:System, :Main)
    set_current_context(:Main)
    println("*************  entering loop **************")
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
