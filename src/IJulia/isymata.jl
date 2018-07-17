## `set_jupyter_input_prompt_color` is not very useful now because
## it changes the color for the entire notebook, rather than the current
## cell.
"""
    set_jupyter_input_prompt_color(c)

Set the color of the Jupyter prompt.
"""
function set_jupyter_input_prompt_color(c)
    display("text/javascript",string("\$(\"div.input_prompt\").css({\"color\": \"", c, "\"})"))
end

"""
    isymata()

Enter Symata mode from within Jupyter (IPython).
From within Symata, `Julia()` exits Symata
and returns to Julia mode.
"""
function isymata()
    if isymata_inited()
        invokelatest(set_isymata_mode)
    else
        init_isymata()
        if isymata_inited()
            invokelatest(set_isymata_mode)
        else
            error("Unable to initialize isymata in IJulia")
        end
    end
#    set_jupyter_input_prompt_color("blue") # This now applies to all cells. So we can't switch between
    nothing
end

"""
    init_isymata()

Check if `IJulia` is loaded. If so, install an "execute_request" handler that
will translate input and output for `Symata`. See `isymata`.
"""
function init_isymata()
    if isdefined(Main, :IJulia)
        try
            _init_isymata()
            _set_historylength(10)  # IJulia stores history, as well.
            global do_we_print_outstring = false # prevents symataevaluate from printing results.
        catch e
            @warn("Initializing isymata failed")
            rethrow(e)
            return
        end
    else
        @warn("IJulia not loaded. Try 'using IJulia'. Try 'Pkg.add(\"IJulia\") if IJulia is not installed.")
        return
    end
    isymata_inited(true)
    setkerneloptions(:output_style, :JupyterForm) # this is only set when initing. The user can change it
    return nothing
end

function import_IJulia()
    eval(:(const IJulia = Main.IJulia))
end

function _init_isymata()
    populate_builtins()
    load_ijulia_handlers()
    import_IJulia()
    symata_mode = invokelatest(IJulia.JupyterMode,
                               :symata,
                               code -> "@Symata.symfull " * " begin\n" * code * "\nend",
                               helpcode -> "Help(" * strip(helpcode) * ")",
                               symata_format_output
                               )
    invokelatest(IJulia.install_ijulia_mode, :symata, symata_mode)
    invokelatest(IJulia.set_ijulia_mode, :symata)
    invokelatest(IJulia.set_handler, "execute_request", IJulia.execute_request_modal)
    eval(quote
         set_isymata_mode() = IJulia.set_ijulia_mode(:symata)
         end)
    eval(quote
         set_julia_mode() = IJulia.set_ijulia_mode(:julia)
         end)
    return nothing
end

function symata_format_output(ex)
    if isymata_mma_mode()
        if using_ijulia_output()
            newex = mxpr(:TeXForm, ex)
            return try
                str = symata_expr_to_mma_string(newex)
                MyLaTeXString("\$\$ " * str *  " \$\$")
            catch
                @warn("Unable to format expression in Mathematic syntax.")
                using_ijulia_output() ? latex_display(wrapout(ex)) : wrapout(ex)
            end
        else
            # SymataSyntax.MmaOutString(symata_expr_to_mma_string(ex))
        end
    else
        return using_ijulia_output() ? latex_display(wrapout(ex)) : wrapout(ex)
    end
end

### Julia

@sjdoc Julia """
    Julia()

Exit Symata mode and returns to Julia mode from within Jupyter.
Use `isymata()` from Julia to enter Symata mode again.
"""

@mkapprule Julia :nargs => 0

@doap function Julia()
    set_julia_mode()
    nothing
#    set_jupyter_input_prompt_color("green") # This used to apply to single cells.
end
