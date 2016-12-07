### SymataSyntax

global symatasyntax_inited = false

function _init_symatasyntax()
    if ! symatasyntax_inited
        try
            @eval using SymataSyntax
            return true
        catch
            error("Unable to load 'SymataSyntax'. If the module is not installed, read https://github.com/jlapeyre/SymataSyntax.jl`")
            return false
        end
    end
    true
end

### MmaSyntax

@sjdoc MmaSyntax """
    MmaSyntax()

enters a Mathematica-syntax mode. Type `ctrl-d` to return to the usual Symata mode.

The Mathematica-syntax mode is a modified version of the mathics REPL (terminal interface).
Input expressions must be valid Mathematica expressions. Output is printed as Mathematica expressions.

This mode uses the mathics parser to read input and the mathics formatter to write output. But, all evaluation
is done by Symata. Symata often calls the python package sympy, and builtin Julia functions.

This mode requires the Julia package `SymataSyntax`, which requires the python package `mathics`.

The only relation to Mathematica is the syntax. This mode does not interact with Mathematica in any way and is independent
of Mathematica and Wolfram language and all other Wolfram company products.
"""

@mkapprule MmaSyntax nargs => 0

@doap function MmaSyntax()
    (! _init_symatasyntax()) && return
    SymataSyntax.mmasyntax_REPL()
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

