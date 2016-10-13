"""
    isymata()

enter Symata mode from within Jupyter (IPython).
From within Symata, `Julia()` exits Symata
and returns to Julia mode.
"""
function isymata()
    if isymata_inited
        eval(Main, :(insymata = true))
    else
        init_isymata()
        if isymata_inited
            eval(Main, :(insymata = true))
        else
            warn("Unable to initialize isymata in IJulia")
        end
    end
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
            _init_isymata_v1_3_2()
            global isymata_inited = true
            _set_historylength(0)
            global do_we_print_outstring = false
        catch
            warn("Initializing isymata failed")
        end
    else
        warn("IJulia not loaded. Try 'using IJulia'. Try 'Pkg.add(\"IJulia\") if IJulia is not installed.")
    end
    nothing
end



#### For use with IJulia v1.3.2

function _init_isymata_v1_3_2()
    eval( Main.IJulia, quote

Main.populate_builtins()

function symata_complete_request(socket, msg)
    code = msg.content["code"]
    cursor_chr = msg.content["cursor_pos"]
    cursorpos = cursor_chr <= 0 ? 0 : chr2ind(code, cursor_chr)
    if isspace(code[1:cursorpos])
        send_ipython(requests[], msg_reply(msg, "complete_reply",
                                 Dict("status" => "ok",
                                              "matches" => String[],
                                              "cursor_start" => cursor_chr,
                                              "cursor_end" => cursor_chr)))
        return
    end

    codestart = find_parsestart(code, cursorpos)
    insymata = (isdefined(Main, :insymata) && Main.insymata)
    local comps
    local positions
    if insymata
        comps, positions = Main.symata_completions(code[codestart:end], cursorpos-codestart+1)
    else
        comps, positions = Base.REPLCompletions.completions(code[codestart:end], cursorpos-codestart+1)
    end
    positions += codestart-1
    if isempty(positions) # true if comps to be inserted without replacement
        cursor_start = (cursor_end = ind2chr(code, last(positions)))
    else
        cursor_start = ind2chr(code, first(positions)) - 1
        cursor_end = ind2chr(code, last(positions))
    end
    send_ipython(requests[], msg_reply(msg, "complete_reply",
                                     Dict("status" => "ok",
                                                  "matches" => comps,
                                                  "cursor_start" => cursor_start,
                                                  "cursor_end" => cursor_end)))
end

# This works a little bit. Part of the documentation is returned. But newlines \n, etc.
# are printed literally.
function symata_inspect_request(socket, msg)
    try
        code = msg.content["code"]
        s = get_token(code, chr2ind(code, msg.content["cursor_pos"]))
        if isempty(s)
            content = Dict("status" => "ok", "found" => false)
        else

            insymata = (isdefined(Main, :insymata) && Main.insymata)
            local d
            if insymata
                d = display_dict(Main.retrieve_doc(s))
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

function symata_execute_request(socket, msg)
    code = msg.content["code"]
    @vprintln("EXECUTING ", code)
    global execute_msg = msg
    global n, In, Out, ans
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

    silent = silent || ismatch(r";\s*$", code)
    if store_history
        In[n] = code
    end

    insymata = (isdefined(Main, :insymata) && Main.insymata)

    # "; ..." cells are interpreted as shell commands for run
    code = replace(code, r"^\s*;.*$",
                   m -> string(replace(m, r"^\s*;", "Base.repl_cmd(`"),
                               "`, STDOUT)"), 0)

    # a cell beginning with "? ..." is interpreted as a help request
    hcode = replace(code, r"^\s*\?", "")
    if hcode != code
        if insymata
            code = "Help(" * strip(code)[2:end] * ")"
            println(code)
        else
            code = helpcode(hcode)
        end
    end


    if insymata
        code = "@Symata.ex " * code
    end

    try
        for hook in preexecute_hooks
            hook()
        end

        #run the code!
        ans = result = ismatch(magics_regex, code) ? magics_help(code) :
            include_string(code, "In[$n]")

        if silent
            result = nothing
        elseif result != nothing
            if store_history
                if result != Out # workaround for Julia #3066
                    Out[n] = result
                end
            end
        end

        user_expressions = Dict()
        for (v,ex) in msg.content["user_expressions"]
            user_expressions[v] = eval(Main,parse(ex))
        end

        for hook in postexecute_hooks
            hook()
        end

        # flush pending stdio
        flush_all()

        undisplay(result) # dequeue if needed, since we display result in pyout
        display() # flush pending display requests

        if result !== nothing
            # Work around for Julia issue #265 (see # #7884 for context)
            result_metadata = eval(:(metadata($(QuoteNode(result)))))
            send_ipython(publish[],
                         msg_pub(msg, "execute_result",
                                 Dict("execution_count" => n,
                                      "metadata" => result_metadata,
                                      "data" => display_dict(insymata ? Main.wrapout(result) : result))))

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
                hook()
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

#### For use with IJulia master 8abf276  Tue Oct 11 18:43:25 2016 +0100

function _init_isymata()
 eval( Main.IJulia, quote
function symata_execute_request(socket, msg)
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

    silent = silent || ismatch(r";\s*$", code)
    if store_history
        In[n] = code
    end

    # "; ..." cells are interpreted as shell commands for run
    code = replace(code, r"^\s*;.*$",
                   m -> string(replace(m, r"^\s*;", "Base.repl_cmd(`"),
                               "`, STDOUT)"), 0)

    # a cell beginning with "? ..." is interpreted as a help request
    hcode = replace(code, r"^\s*\?", "")
    if hcode != code
        if insymata
            code = "Help(" * code * ")"
        else
            code = helpcode(hcode)
        end
    end

    insymata = (isdefined(Main, :insymata) && Main.insymata)
    if insymata
        code = "@Symata.ex " * code
    end

    try
        for hook in preexecute_hooks
            hook()
        end

        #run the code!
        ans = result = ismatch(magics_regex, code) ? magics_help(code) :
            include_string(code, "In[$n]")

        if silent
            result = nothing
        elseif result != nothing
            if store_history
                if result != Out # workaround for Julia #3066
                    Out[n] = result
                end
            end
        end

        user_expressions = Dict()
        for (v,ex) in msg.content["user_expressions"]
            user_expressions[v] = eval(Main,parse(ex))
        end

        for hook in postexecute_hooks
            hook()
        end

        # flush pending stdio
        flush_all()

        undisplay(result) # dequeue if needed, since we display result in pyout
        display() # flush pending display requests

        if result !== nothing
            # Work around for Julia issue #265 (see # #7884 for context)
            result_metadata = eval(:(metadata($(QuoteNode(result)))))
            send_ipython(publish[],
                         msg_pub(msg, "execute_result",
                                 Dict("execution_count" => n,
                                            "metadata" => result_metadata,
                                            "data" => display_dict(insymata ? Main.wrapout(result) : result))))

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
                hook()
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
 end)
    nothing
end
