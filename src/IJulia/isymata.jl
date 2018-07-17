"""
    isymata()

enter Symata mode from within Jupyter (IPython).
From within Symata, `Julia()` exits Symata
and returns to Julia mode.
"""
function isymata()
    if isymata_inited()
        isymata_mode(true)
    else
        init_isymata()
        if isymata_inited()
            isymata_mode(true)
        else
            error("Unable to initialize isymata in IJulia")
        end
    end
#    set_jupyter_input_prompt_color("blue")
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
            global do_we_print_outstring = false
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
#    setkerneloptions(:output_style, :JupyterForm) # this is only set when initing. The user can change it
    setkerneloptions(:output_style, :InputForm) # this is only set when initing. The user can change it
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
                               code -> "@Symata.symfull " * " begin\n" * code * "\nend",
                               helpcode -> "Help(" * strip(helpcode) * ")",
                               symata_format_output
                               )
    invokelatest(IJulia.install_ijulia_mode, :symata, symata_mode)
    invokelatest(IJulia.set_ijulia_mode, :symata)
    invokelatest(IJulia.set_handler, "execute_request", IJulia.execute_request_modal)
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
