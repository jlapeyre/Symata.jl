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

@mkapprule MmaSyntax :nargs => 0

@doap function MmaSyntax()
    (! _init_symatasyntax()) && return
    SymataSyntax.mathics_REPL()
end
