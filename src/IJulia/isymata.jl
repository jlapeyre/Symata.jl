function set_jupyter_input_prompt_color(c)
    display("text/javascript","\$(\"div.input_prompt\").css({\"color\": \"" * c * "\"})")
end

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
    set_jupyter_input_prompt_color("blue")
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
    setkerneloptions(:output_style, :JupyterForm) # this is only set when initing. The user can change it
    nothing
end


#### For use with IJulia v1.3.2

function _init_isymata()
    Core.eval(Main.IJulia, quote

#          import Symata: latex_display, wrapout, symata_completions, populate_builtins, retrieve_doc, isymata_mode, isymata_mma_mode, using_ijulia_output, doeval, mxpr,
#              MyLaTeXString
export symata_execute_request, symata_complete_request, symata_inspect_request

              Core.eval(IJulia, quote
                        import Symata: retrieve_doc, isymata_mode, isymata_mma_mode, using_ijulia_output, doeval, mxpr, MyLaTeXString
                        end)


#using SymataSyntax
#import SymataSyntax: mmatosymata

Symata.populate_builtins()

function symata_complete_request(socket, msg)
    code = msg.content["code"]
    cursor_chr = msg.content["cursor_pos"]
    cursorpos = chr2ind(msg, code, cursor_chr)
    if all(isspace, code[1:cursorpos])
        send_ipython(requests[], msg_reply(msg, "complete_reply",
                                 Dict("status" => "ok",
                                              "metadata" => Dict(),
                                              "matches" => String[],
                                              "cursor_start" => cursor_chr,
                                              "cursor_end" => cursor_chr)))
        return
    end

              codestart = find_parsestart(code, cursorpos)
    if isymata_mode()
              comps, positions = Symata.symata_completions(code[codestart:end], cursorpos-codestart+1)
    else
              comps_, positions = REPLCompletions.completions(code[codestart:end], cursorpos-codestart+1)
    end
    if ! isymata_mode()
              @static if isdefined(REPLCompletions, :completion_text)
              comps = REPLCompletions.completion_text.(comps_) # julia#26930
              comps, positions = Symata.symata_completions(code[codestart:end], cursorpos-codestart+1)
                   else
              comps = comps_
              end
    end

    # positions = positions .+ (codestart - 1) on Julia 0.7
    positions = (first(positions) + codestart - 1):(last(positions) + codestart - 1)
    metadata = Dict()
    if isempty(comps)
        # issue #530: REPLCompletions returns inconsistent results
        # for positions when no completions are found
        cursor_start = cursor_end = cursor_chr
    elseif isempty(positions) # true if comps to be inserted without replacement
        cursor_start = (cursor_end = ind2chr(msg, code, last(positions)))
    else
        cursor_start = ind2chr(msg, code, prevind(code, first(positions)))
        cursor_end = ind2chr(msg, code, last(positions))
        metadata["_jupyter_types_experimental"] = complete_types(comps)
    end
    send_ipython(requests[], msg_reply(msg, "complete_reply",
                                     Dict("status" => "ok",
                                                  "matches" => comps,
                                                  "metadata" => metadata,
                                                  "cursor_start" => cursor_start,
                                                  "cursor_end" => cursor_end)))
end

function inspect_request(socket, msg)
    try
        code = msg.content["code"]
        s = get_token(code, chr2ind(msg, code, msg.content["cursor_pos"]))
        if isempty(s)
            content = Dict("status" => "ok", "found" => false)
        else
            if isymata_mode()
                d = display_dict(retrieve_doc(s))
            else
              d = docdict(s)
            end
            content = Dict("status" => "ok",
                           "found" => !isempty(d),
                           "data" => d)
        end
        send_ipython(requests[], msg_reply(msg, "inspect_reply", content))
    catch e
        content = error_content(e, backtrace_top=:inspect_request);
        content["status"] = "error"
        send_ipython(requests[],
                     msg_reply(msg, "inspect_reply", content))
    end
end

function symata_format_outptut(ex)
            if isymata_mode()
                if isymata_mma_mode()
                    if using_ijulia_output()
                        newex = mxpr(:TeXForm, ex)
                        try
                            str = symata_expr_to_mma_string(newex)
                            MyLaTeXString("\$\$ " * str *  " \$\$")
                        catch
                            @warn("Unable to format expression in Mathematic syntax.")
                            using_ijulia_output() ? Symata.latex_display(Symata.wrapout(ex)) : Symata.wrapout(ex)
                        end
                    else
#                        SymataSyntax.MmaOutString(symata_expr_to_mma_string(ex))
                    end
                else
                    using_ijulia_output() ? Symata.latex_display(Symata.wrapout(ex)) : Symata.wrapout(ex)
                end
            else ## we are in julia mode
                ex
            end
        end

function execute_request(socket, msg)
    code = msg.content["code"]
    @vprintln("EXECUTING ", code)
    global execute_msg = msg
    global n, In, Out, ans
    stdio_bytes[] = 0
    silent = msg.content["silent"]
    store_history = get(msg.content, "store_history", !silent)
    empty!(execute_payloads)

    if !silent
        n += 1
        send_ipython(publish[],
                     msg_pub(msg, "execute_input",
                             Dict("execution_count" => n,
                                          "code" => code)))
    end

    silent = silent || REPL.ends_with_semicolon(code)
    if store_history
        In[n] = code
    end

    # "; ..." cells are interpreted as shell commands for run
    code = replace(code, r"^\s*;.*$" =>
                   m -> string(replace(m, r"^\s*;", "Base.repl_cmd(`"),
                               "`, ", stdout_name, ")"))

    # a cell beginning with "? ..." is interpreted as a help request
    hcode = replace(code, r"^\s*\?" => "")

    try
        for hook in preexecute_hooks
            invokelatest(hook)
        end

    if hcode != code
        if isymata_mode()
            code = "Help(" * strip(code)[2:end] * ")"
            println(code)
        else
            code = Core.eval(Main, helpmode(hcode))
        end
    else
      if isymata_mode()
        if isymata_mma_mode()
#            code = "Symata.doeval(SymataSyntax.mmatosymata(\"\"\"" * code  * "\"\"\"))"
        else
            code = "@Symata.ex " * " begin\n" * code * "\nend"  # begin end to wrap multi-expression input
        end
     end
       #run the code!
        ans = result = occursin(magics_regex, code) ? magics_help(code) :
            include_string(current_module[], code, "In[$n]")
        end

        if silent
            result = nothing
        elseif result !== nothing
            if store_history
                Out[n] = result
            end
        end

        user_expressions = Dict()
        for (v,ex) in msg.content["user_expressions"]
            user_expressions[v] = invokelatest(parse, ex)
        end

        for hook in postexecute_hooks
            invokelatest(hook)
        end

        # flush pending stdio
        flush_all()

        undisplay(result) # dequeue if needed, since we display result in pyout
        invokelatest(display) # flush pending display requests

        if result !== nothing
            result_metadata = invokelatest(metadata, result)
            result_data = invokelatest(display_dict, symata_format_output(result))
            send_ipython(publish[],
                         msg_pub(msg, "execute_result",
                                 Dict("execution_count" => n,
                                              "metadata" => result_metadata,
                                              "data" => result_data)))

        end
        send_ipython(requests[],
                     msg_reply(msg, "execute_reply",
                               Dict("status" => "ok",
                                            "payload" => execute_payloads,
                                            "execution_count" => n,
                                            "user_expressions" => user_expressions)))
        empty!(execute_payloads)
    catch e
        bt = catch_backtrace()
        try
            # flush pending stdio
            flush_all()
            for hook in posterror_hooks
                invokelatest(hook)
            end
        catch
        end
        empty!(displayqueue) # discard pending display requests on an error
        content = error_content(e,bt)
        send_ipython(publish[], msg_pub(msg, "error", content))
        content["status"] = "error"
        content["execution_count"] = n
        send_ipython(requests[], msg_reply(msg, "execute_reply", content))
    end
end

IJulia.handlers["execute_request"] = IJulia.symata_execute_request
IJulia.handlers["complete_request"] = IJulia.symata_complete_request
IJulia.handlers["inspect_request"] = IJulia.symata_inspect_request

 end)
    nothing
end
