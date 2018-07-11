### SymataSyntax

global symatasyntax_inited = false

function _init_symatasyntax()
    if ! symatasyntax_inited
        try
            @eval using SymataSyntax
            global symatasyntax_inited = true
            return true
        catch e
            @warn("Unable to load 'SymataSyntax'. If the module is not installed, " *
                  "read https://github.com/jlapeyre/SymataSyntax.jl`.")
            rethrow(e)
        end
    else
        nothing
    end
    return true
end

### MmaSyntax

@sjdoc MmaSyntax """
    MmaSyntax()

enters a Mathematica-syntax mode. Type `ctrl-d` to return to the usual Symata mode in the terminal.
In Jupyter, type `MmaSyntax[False]` to return to Symata syntax.

In a terminal, the Mathematica-syntax mode is a modified version of the mathics REPL (terminal interface).
Input expressions must be valid Mathematica expressions. Output is printed as Mathematica expressions.
In Jupyter, Mathematica syntax is expected as input.

This mode uses the mathics parser to read input and the mathics formatter to write output. But, all evaluation
is done by Symata. Symata often calls the python package sympy, and builtin Julia functions.

This mode requires the Julia package `SymataSyntax`, which requires the python package `mathics`.

The only relation to Mathematica is the syntax. This mode does not interact with Mathematica in any way and is independent
of Mathematica and Wolfram language and all other Wolfram company products.
"""
@mkapprule MmaSyntax nargs => 0:1

@doap function MmaSyntax()
    (! _init_symatasyntax()) && return
    if isymata_inited()
        isymata_mma_mode(true)
    else
        Base.invokelatest(SymataSyntax.mmasyntax_REPL)
    end
end

@doap function MmaSyntax(b::Bool)
    (! _init_symatasyntax()) && return
    if isymata_inited()
        isymata_mma_mode(b)
    else
        if b
            SymataSyntax.mmasyntax_REPL()
        else
            symprintln("Use ctrl-d to exit the Mma syntax REPL")
        end
    end
end

@mkapprule SymataSyntax

@doap function SymataSyntax()
    (! _init_symatasyntax()) && return
    if isymata_inited()
        isymata_mma_mode(false)
    else
        symprintln("Use ctrl-d to exit the Mma syntax REPL")
    end
end

### Mathics

@sjdoc Mathics """
    Mathics()

enters the mathics shell. Type `ctrl-d` to return to the Symata.

The mathics process is separate from the Symata process. In particular, setting a variable in mathics
does nothing in Symata and vice-versa.
"""

@mkapprule Mathics nargs => 0

@doap function Mathics()
    (! _init_symatasyntax()) && return
    SymataSyntax.mathics_REPL()
end


### GetMma

@mkapprule GetMma

@sjdoc GetMma """
    GetMma(filename)

read and evaluate Mathematica-syntax expressions from file `filename`.
"""

@doap function GetMma(fname::String)
    (! _init_symatasyntax()) && return
    SymataSyntax.read_file(fname)
end

