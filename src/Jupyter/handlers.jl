# This file contains code that is conditionally evaluated in the IJulia modue.
# It provides an alternate "modal" handler functions.
# These handler functions read global state in the variable current_mode[] to
# execute conditionally small parts of code that are specific to DSLs (and Julia)
#
# To edit the code here, comment out the first line quoting the function or
# set of functions that you want to edit. This allows emacs to do normal indentation. Then uncomment
# these lines when finished editing.

# The code to insert in IJulia is contained in several quoted expressions.
# Here, we evaluate each of them in the IJulia module. We must hide the module
# name IJulia from the package manager.
function load_ijulia_handlers()
    themodule = eval(:(Main.IJulia))
    for code in (manage_handler_code, execute_request_code, complete_request_code)
        Core.eval(themodule, code)
    end
end

# This code defines the struct whose fields are functions that alter user
# input and the result data. An instance of this struct is created for each
# "mode" (overused word), i.e. Julia mode, Symata mode. The default mode,
# for Julia, is created here, as well as code to switch between modes and
# to install these alternative handlers.
manage_handler_code = quote
# fields are functions mapping strings to strings
struct JupyterMode
    modename::Symbol
    code_hook     # function that alters user input `code`
    helpcode_hook # function that alters user help request `hcode` (with '?' already stripped)
    output_hook   # function that alters reponse `result_data`.
    completion_hook # function to get completions
end

function completions_positions_julia_languge(codepart, inposition)
#    comps_, positions = REPLCompletions.completions(code[codestart:end], cursorpos-codestart+1)
    comps_, positions = REPLCompletions.completions(codepart, inposition)
    @static if isdefined(REPLCompletions, :completion_text)
        comps = REPLCompletions.completion_text.(comps_) # julia#26930
    else
        comps = comps_
    end
    return (comps, completions)
end

# default mode is plain-Julia mode.
const default_mode = JupyterMode(
    :julia,
    identity,   # code
    (hcode) -> Core.eval(Main, REPL.helpmode(hcode)),  # helpcode
    identity,  # output
    completions_positions_julia_languge
)

#  ijulia_modes contains all modes.
const ijulia_modes = Dict()

function install_ijulia_mode(modename::Symbol, mode::JupyterMode)
    ijulia_modes[modename] = mode
end

# plain-Julia mode
install_ijulia_mode(:julia, default_mode)

const current_language_mode = Ref(default_mode)

# Used to switch among modes
function set_ijulia_mode(modename::Symbol)
    if ! haskey(ijulia_modes, modename)
        throw(KeyError(modename))
    end
    current_language_mode[] = ijulia_modes[modename]
end

function get_current_ijulia_modename()
    return current_language_mode[].modename
end

# Set the handler. This is called from isymata.jl
function set_handler(handlername, handlerfunction)
    handlername_string = String(handlername)
    if ! haskey(handlers, handlername_string)
        throw(KeyError(handlername_string))
    end
    handlers[String(handlername)] = handlerfunction
end

end # quote manage_handler_code

execute_request_code = quote
function execute_request_modal(socket, msg)
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

        if hcode != code # help request
            current_language_mode[].helpcode_hook(hcode)
            result = nothing
        else
            #run the code!
            code = current_language_mode[].code_hook(code)
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
            result_data = invokelatest(display_dict, current_language_mode[].output_hook(result))
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
end # quote execute_request_code


complete_request_code = quote
function complete_request_modal(socket, msg)
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
    comps, positions = current_language_mode[].completion_hook(code[codestart:end], cursorpos-codestart+1)
    # comps_, positions = REPLCompletions.completions(code[codestart:end], cursorpos-codestart+1)
    # @static if isdefined(REPLCompletions, :completion_text)
    #     comps = REPLCompletions.completion_text.(comps_) # julia#26930
    # else
    #     comps = comps_
    # end

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

end # quote complete_request_code

nothing
