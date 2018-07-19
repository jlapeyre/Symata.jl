function is_isymata_mode()
    _is_isymata_mode()
end

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
    if Symata.isymata_inited()
        invokelatest(set_isymata_mode)
    else
        init_isymata()
        if Symata.isymata_inited()
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
            Symata._set_historylength(10)  # IJulia stores history, as well.
            Core.eval(Symata, :(do_we_print_Out_label = false)) # prevents symataevaluate from printing results.
        catch e
            @warn("Initializing isymata failed")
            rethrow(e)
            return
        end
    else
        @warn("IJulia not loaded. Try 'using IJulia'. Try 'Pkg.add(\"IJulia\") if IJulia is not installed.")
        return
    end
    Symata.isymata_inited(true)
    Symata.setkerneloptions(:output_style, :JupyterForm) # this is only set when initing. The user can change it
    return nothing
end

function import_IJulia()
    eval(:(const IJulia = Main.IJulia))
end

function _symata_code_hook(code::AbstractString)
    if Symata.iscomment(code)
        return ""
    else
        return "@Symata.symfull " * " begin\n" * code * "\nend"
    end
end

function _init_isymata()
    Symata.populate_builtins()
    load_ijulia_handlers()
    import_IJulia()
    # create struct with hooks used in modal handlers
    symata_mode = invokelatest(IJulia.JupyterMode,
                               :symata,   # language mode name
                               _symata_code_hook, # to transform input
                               helpcode -> Symata.print_doc(strip(helpcode)),
                               symata_format_output, # to transform output
                               Symata.symata_completions # to choose symbols for completion
                               )
##  install Symata-language hooks, switch to Symata mode, install modal handlers.
    invokelatest(IJulia.install_ijulia_mode, :symata, symata_mode)
    invokelatest(IJulia.set_ijulia_mode, :symata)
    invokelatest(IJulia.set_handler, "execute_request", IJulia.execute_request_modal)
    invokelatest(IJulia.set_handler, "complete_request", IJulia.complete_request_modal)
    eval(quote
         set_isymata_mode() = IJulia.set_ijulia_mode(:symata)
         set_julia_mode() = IJulia.set_ijulia_mode(:julia)
         _is_isymata_mode() = IJulia.get_current_ijulia_modename() == :julia
         end)
    return nothing
end

function symata_format_output(ex)
    if Symata.isymata_mma_mode()
        if Symata.using_ijulia_output()
            newex = Symata.mxpr(:TeXForm, ex)
            return try
                str = Symata.symata_expr_to_mma_string(newex)
                Symata.MyLaTeXString("\$\$ " * str *  " \$\$")
            catch
                @warn("Unable to format expression in Mathematic syntax.")
                Symata.using_ijulia_output() ? Symata.latex_display(Symata.wrapout(ex)) : Symata.wrapout(ex)
            end
        else
            # SymataSyntax.MmaOutString(symata_expr_to_mma_string(ex))
        end
    else
        return Symata.using_ijulia_output() ? Symata.latex_display(Symata.wrapout(ex)) : Symata.wrapout(ex)
    end
end
