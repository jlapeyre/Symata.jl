import Base: LineEdit, REPL, Terminals

import Base.LineEdit: CompletionProvider, transition

import Base.REPL: LineEditREPL, BasicREPL, StreamREPL, ends_with_semicolon, print_response,
REPLCompletionProvider, return_callback, Prompt, respond, ShellCompletionProvider,
REPLHistoryProvider, find_hist_file, print_response, outstream, hist_from_file, hist_getline,
history_reset_state, LatexCompletions, edit_insert, mode_keymap, ModalInterface

import Base.REPLCompletions: bslash_completions, non_identifier_chars, should_method_complete,
find_start_brace, complete_path

import Base.REPL: AbstractREPL, start_repl_backend, run_frontend, REPLBackendRef,
   LineEditREPL, REPLDisplay, run_interface, setup_interface, LineEdit, REPL

type SJuliaCompletionProvider <: CompletionProvider
    r::LineEditREPL
end

function SJulia_parse_REPL_line(line)
    Base.parse_input_line("@SJulia.ex " * line)
    # Doing something with more information, as below, would
    # be nice, but some errors crash the REPL. Using
    # the macro @ex instead of function exfunc, makes a
    # much more stable REPL. There is probably a good
    # way to do it.

    # prev = syntax_deprecation_warnings(false)
    # try
    #     exfunc(Base.parse_input_line(line))
    # catch e
    #     print_with_color(:red, "SJulia: ",e)
    # finally
    #     syntax_deprecation_warnings(prev)
    # end
end

function Base.LineEdit.complete_line(c::SJuliaCompletionProvider, s)
    partial = Base.REPL.bytestring_beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = sjulia_completions(full, endof(partial))
    return ret, partial[range], should_complete
end

const sorted_builtins = Array(ByteString,0)
function populate_builtins()
    b = protectedsymbols_strings()
    for s in b
        push!(sorted_builtins,s)
    end
    nothing
end

function complete_sjulia_builtins(s::ByteString)
    r = searchsorted(sorted_builtins, s)
    i = first(r)
    n = length(sorted_builtins)
    while i <= n && startswith(sorted_builtins[i],s)
        r = first(r):i
        i += 1
    end
    sorted_builtins[r]
end

function sjulia_completions(string, pos)
    # First parse everything up to the current position
    partial = string[1:pos]
    inc_tag = Base.syntax_deprecation_warnings(false) do
        Base.incomplete_tag(parse(partial, raise=false))
    end
    if inc_tag in [:cmd, :string]
        m = match(r"[\t\n\r\"'`@\$><=;|&\{]| (?!\\)", reverse(partial))
        startpos = nextind(partial, reverseind(partial, m.offset))
        r = startpos:pos
        paths, r, success = complete_path(replace(string[r], r"\\ ", " "), pos)
        if inc_tag == :string &&
           length(paths) == 1 &&                              # Only close if there's a single choice,
           !isdir(expanduser(replace(string[startpos:start(r)-1] * paths[1], r"\\ ", " "))) &&  # except if it's a directory
           (length(string) <= pos || string[pos+1] != '"')    # or there's already a " at the cursor.
            paths[1] *= "\""
        end
        #Latex symbols can be completed for strings
        (success || inc_tag==:cmd) && return sort(paths), r, success
    end

    ok, ret = bslash_completions(string, pos)
    ok && return ret

    # Make sure that only bslash_completions is working on strings
    inc_tag==:string && return UTF8String[], 0:-1, false

     if inc_tag == :other && should_method_complete(partial)
        frange, method_name_end = find_start_brace(partial)
        ex = Base.syntax_deprecation_warnings(false) do
            parse(partial[frange] * ")", raise=false)
        end
        if isa(ex, Expr) && ex.head==:call
            return complete_methods(ex), start(frange):method_name_end, false
        end
    elseif inc_tag == :comment
        return UTF8String[], 0:-1, false
    end

    dotpos = rsearch(string, '.', pos)
    startpos = nextind(string, rsearch(string, non_identifier_chars, pos))

    ffunc = (mod,x)->true
    suggestions = UTF8String[]
    startpos == 0 && (pos = -1)
    dotpos <= startpos && (dotpos = startpos - 1)
    s = string[startpos:pos]
    append!(suggestions, complete_sjulia_builtins(s))
    return sort(unique(suggestions)), (dotpos+1):pos, true
end

# Code modified from Cxx.jl

global RunSJuliaREPL

# Handles BasicREPL
function RunSJuliaREPL(repl)
    sjulia_run_repl(repl)    
end

function RunSJuliaREPL(repl::LineEditREPL)

    # Completion list for SJulia
    populate_builtins()

    # Setup sjulia sjulia_prompt
    sjulia_prompt =
        LineEdit.Prompt("sjulia > ";
          # Copy colors from the prompt object
                        prompt_prefix=Base.text_colors[:blue],
                        complete = SJuliaCompletionProvider(repl),
                        on_enter = return_callback
#                        prompt_suffix = hascolor ?  hascolor not defined
#                        (repl.envcolors ? Base.input_color : repl.input_color) : ""
                        )
    sjulia_prompt.on_done =
        REPL.respond(SJulia_parse_REPL_line,
               repl, sjulia_prompt) # stay in symjulia

    main_mode = repl.interface.modes[1]

    push!(repl.interface.modes,sjulia_prompt)

    hp = main_mode.hist
    hp.mode_mapping[:sjulia] = sjulia_prompt
    sjulia_prompt.hist = hp

    const enter_sjulia_key = '='
    const sjulia_keymap = Dict{Any,Any}(
           enter_sjulia_key => function (s,args...)
            if isempty(s)
                if !haskey(s.mode_state,sjulia_prompt)
                    s.mode_state[sjulia_prompt] = LineEdit.init_state(repl.t,sjulia_prompt)
                end
                LineEdit.transition(s,sjulia_prompt)
            else
                LineEdit.edit_insert(s,enter_sjulia_key)
            end
           end
     )

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    mk = REPL.mode_keymap(main_mode)

    b = Dict{Any,Any}[skeymap, mk, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    sjulia_prompt.keymap_dict = LineEdit.keymap(b)

    main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, sjulia_keymap);
    nothing
end


import Base.REPL: AbstractREPL, start_repl_backend, run_frontend, REPLBackendRef,
   LineEditREPL, REPLDisplay, run_interface, setup_interface, LineEdit, REPL

function sjulia_run_repl(repl::AbstractREPL, consumer = x->nothing)
    repl_channel = Channel(1)
    response_channel = Channel(1)
    backend = start_repl_backend(repl_channel, response_channel)
    sjulia_run_frontend(repl, REPLBackendRef(repl_channel,response_channel))
    return backend
end

sjulia_run_repl(stream::IO) = run_repl(StreamREPL(stream))

function sjulia_run_frontend(repl::LineEditREPL, backend)
    d = REPLDisplay(repl)
    dopushdisplay = repl.specialdisplay === nothing && !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    if !isdefined(repl,:interface)
        interface = repl.interface = sjulia_setup_interface(repl)
        set_sjulia_prompt(1)  ## !! This could be handled in a hook. Make pull request of my hook code.
    else
        interface = repl.interface
    end
    repl.backendref = backend
#### Use this to add a mode to the stock repl
#    RunSJuliaREPL(repl)
    run_interface(repl.t, interface)
    dopushdisplay && popdisplay(d)
end

function sjulia_run_frontend(repl::BasicREPL, backend::REPLBackendRef)
    d = REPLDisplay(repl)
    dopushdisplay = !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    repl_channel, response_channel = backend.repl_channel, backend.response_channel
    hit_eof = false
    while true
        Base.reseteof(repl.terminal)
        write(repl.terminal, "sjulia> ")
        line = ""
        ast = nothing
        interrupted = false
        while true
            try
                line *= readline(repl.terminal)
            catch e
                if isa(e,InterruptException)
                    try # raise the debugger if present
                        ccall(:jl_raise_debugger, Int, ())
                    end
                    line = ""
                    interrupted = true
                    break
                elseif isa(e,EOFError)
                    hit_eof = true
                    break
                else
                    rethrow()
                end
            end
            ast = SJulia_parse_REPL_line(line)
            (isa(ast,Expr) && ast.head == :incomplete) || break
        end
        if !isempty(line)
            put!(repl_channel, (ast, 1))
            val, bt = take!(response_channel)
            if !ends_with_semicolon(line)
                print_response(repl, val, bt, true, false)
            end
        end
        write(repl.terminal, '\n')
        ((!interrupted && isempty(line)) || hit_eof) && break
    end
    # terminate backend
    put!(repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
end

function sjulia_run_frontend(repl::StreamREPL, backend::REPLBackendRef)
    have_color = Base.have_color
    banner(repl.stream, have_color)
    d = REPLDisplay(repl)
    dopushdisplay = !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    repl_channel, response_channel = backend.repl_channel, backend.response_channel
    while !eof(repl.stream)
        if have_color
            print(repl.stream,repl.prompt_color)
        end
        print(repl.stream, "sjulia> ")
        if have_color
            print(repl.stream, input_color(repl))
        end
        line = readline(repl.stream)
        if !isempty(line)
            ast = SJulia_parse_REPL_line(line)
            if have_color
                print(repl.stream, Base.color_normal)
            end
            put!(repl_channel, (ast, 1))
            val, bt = take!(response_channel)
            if !ends_with_semicolon(line)
                print_response(repl, val, bt, true, have_color)
            end
        end
    end
    # Terminate Backend
    put!(repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
end

function sjulia_setup_interface(repl::LineEditREPL; hascolor = repl.hascolor, extra_repl_keymap = Dict{Any,Any}[])
    ###
    #
    # This function returns the main interface that describes the REPL
    # functionality, it is called internally by functions that setup a
    # Terminal-based REPL frontend, but if you want to customize your REPL
    # or embed the REPL in another interface, you may call this function
    # directly and append it to your interface.
    #
    # Usage:
    #
    # repl_channel,response_channel = Channel(),Channel()
    # start_repl_backend(repl_channel, response_channel)
    # setup_interface(REPLDisplay(t),repl_channel,response_channel)
    #
    ###

    ###
    # We setup the interface in two stages.
    # First, we set up all components (prompt,rsearch,shell,help)
    # Second, we create keymaps with appropriate transitions between them
    #   and assign them to the components
    #
    ###

    ############################### Stage I ################################

    # This will provide completions for REPL and help mode
    replc = REPLCompletionProvider(repl)

    # Completions for SJulia
    sjulia_replc = SJuliaCompletionProvider(repl)

    # Set up the main Julia prompt
    julia_prompt = Prompt("julia> ";
        # Copy colors from the prompt object
        prompt_prefix = hascolor ? repl.prompt_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        keymap_func_data = repl,
        complete = replc,
        on_enter = return_callback)

    julia_prompt.on_done = respond(Base.parse_input_line, repl, julia_prompt)

    # Completion list for SJulia
    populate_builtins()

    # Setup sjulia sjulia_prompt
    sjulia_prompt =
        LineEdit.Prompt("sjulia > ";
                        prompt_prefix=Base.text_colors[:blue],
#                        keymap_func_data = repl,   what does this do ?
                        prompt_suffix = hascolor ?
                        (repl.envcolors ? Base.input_color : repl.input_color) : "",
                        complete = sjulia_replc,
                        on_enter = return_callback  # allows multiline input
                        )

    sjulia_prompt.on_done =
        REPL.respond(SJulia_parse_REPL_line,
                      repl, sjulia_prompt) # stay in symjulia

    # Setup help mode
    help_mode = Prompt("help?> ",
        prompt_prefix = hascolor ? repl.help_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        keymap_func_data = repl,
        complete = replc,
        # When we're done transform the entered line into a call to help("$line")
        on_done = respond(Docs.helpmode, repl, julia_prompt))

    # Set up shell mode
    shell_mode = Prompt("shell> ";
        prompt_prefix = hascolor ? repl.shell_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        keymap_func_data = repl,
        complete = ShellCompletionProvider(repl),
        # Transform "foo bar baz" into `foo bar baz` (shell quoting)
        # and pass into Base.repl_cmd for processing (handles `ls` and `cd`
        # special)
        on_done = respond(repl, julia_prompt) do line
            Expr(:call, :(Base.repl_cmd), macroexpand(Expr(:macrocall, symbol("@cmd"),line)), outstream(repl))
        end)


    ################################# Stage II #############################

    # Setup history
    # We will have a unified history for all REPL modes
    hp = REPLHistoryProvider(Dict{Symbol,Any}(:julia => julia_prompt,
                                              :sjulia => sjulia_prompt,
                                              :shell => shell_mode,
                                              :help  => help_mode))
    if repl.history_file
        try
            f = open(find_hist_file(), true, true, true, false, false)
            finalizer(replc, replc->close(f))
            hist_from_file(hp, f)
        catch e
            print_response(repl, e, catch_backtrace(), true, Base.have_color)
            println(outstream(repl))
            info("Disabling history file for this session.")
            repl.history_file = false
        end
    end
    history_reset_state(hp)
    julia_prompt.hist = hp
    sjulia_prompt.hist = hp
    shell_mode.hist = hp
    help_mode.hist = hp

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    search_prompt.complete = LatexCompletions()

    # Canonicalize user keymap input
    if isa(extra_repl_keymap, Dict)
        extra_repl_keymap = [extra_repl_keymap]
    end

    const enter_sjulia_key = '='

    const repl_keymap = Dict{Any,Any}(
         enter_sjulia_key => function (s,args...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                if !haskey(s.mode_state,sjulia_prompt)
                    s.mode_state[sjulia_prompt] = LineEdit.init_state(repl.t,sjulia_prompt)
                end
                buf = copy(LineEdit.buffer(s))   # allows us to edit a line of julia input
                if LineEdit.mode(s) == sjulia_prompt
                  LineEdit.transition(s, julia_prompt) do
                     LineEdit.state(s, julia_prompt).input_buffer = buf
                  end
                else
                    LineEdit.transition(s, sjulia_prompt) do
                    LineEdit.state(s, sjulia_prompt).input_buffer = buf
                    end
               end
            else
                LineEdit.edit_insert(s,enter_sjulia_key)
            end
           end,
         ';' => function (s,o...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                transition(s, shell_mode) do
                    LineEdit.state(s, shell_mode).input_buffer = buf
                end
            else
                edit_insert(s, ';')
            end
        end,
        '?' => function (s,o...)  # Disable this for sjulia mode, because we use '?' for sjulia help.
            if LineEdit.mode(s) != sjulia_prompt && (isempty(s) || position(LineEdit.buffer(s)) == 0)
                buf = copy(LineEdit.buffer(s))
                transition(s, help_mode) do
                    LineEdit.state(s, help_mode).input_buffer = buf
                end
            else
                edit_insert(s, '?')
            end
        end,

        # Bracketed Paste Mode
        "\e[200~" => (s,o...)->begin
            input = LineEdit.bracketed_paste(s) # read directly from s until reaching the end-bracketed-paste marker
            sbuffer = LineEdit.buffer(s)
            curspos = position(sbuffer)
            seek(sbuffer, 0)
            shouldeval = (nb_available(sbuffer) == curspos && search(sbuffer, UInt8('\n')) == 0)
            seek(sbuffer, curspos)
            if curspos == 0
                # if pasting at the beginning, strip leading whitespace
                input = lstrip(input)
            end
            if !shouldeval
                # when pasting in the middle of input, just paste in place
                # don't try to execute all the WIP, since that's rather confusing
                # and is often ill-defined how it should behave
                edit_insert(s, input)
                return
            end
            edit_insert(sbuffer, input)
            input = takebuf_string(sbuffer)
            oldpos = start(input)
            firstline = true
            while !done(input, oldpos) # loop until all lines have been executed
                ast, pos = Base.syntax_deprecation_warnings(false) do
                    Base.parse(input, oldpos, raise=false)
                end
                if (isa(ast, Expr) && (ast.head == :error || ast.head == :continue || ast.head == :incomplete)) ||
                        (done(input, pos) && !endswith(input, '\n'))
                    # remaining text is incomplete (an error, or parser ran to the end but didn't stop with a newline):
                    # Insert all the remaining text as one line (might be empty)
                    tail = input[oldpos:end]
                    if !firstline
                        # strip leading whitespace, but only if it was the result of executing something
                        # (avoids modifying the user's current leading wip line)
                        tail = lstrip(tail)
                    end
                    LineEdit.replace_line(s, tail)
                    LineEdit.refresh_line(s)
                    break
                end
                # get the line and strip leading and trailing whitespace
                line = strip(input[oldpos:prevind(input, pos)])
                if !isempty(line)
                    # put the line on the screen and history
                    LineEdit.replace_line(s, line)
                    LineEdit.commit_line(s)
                    # execute the statement
                    terminal = LineEdit.terminal(s) # This is slightly ugly but ok for now
                    raw!(terminal, false) && disable_bracketed_paste(terminal)
                    LineEdit.mode(s).on_done(s, LineEdit.buffer(s), true)
                    raw!(terminal, true) && enable_bracketed_paste(terminal)
                end
                oldpos = pos
                firstline = false
            end
        end,
    )

    prefix_prompt, prefix_keymap = LineEdit.setup_prefix_keymap(hp, julia_prompt)

    a = Dict{Any,Any}[skeymap, repl_keymap, prefix_keymap, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    prepend!(a, extra_repl_keymap)

#    sjulia_prompt.keymap_dict = LineEdit.keymap(a)
    julia_prompt.keymap_dict = LineEdit.keymap(a)

    mk = mode_keymap(julia_prompt)
#    mk = mode_keymap(sjulia_prompt)

    b = Dict{Any,Any}[skeymap, mk, prefix_keymap, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    prepend!(b, extra_repl_keymap)

    sjulia_prompt.keymap_dict = LineEdit.keymap(a)
    #sjulia_prompt.keymap_dict = shell_mode.keymap_dict = help_mode.keymap_dict = LineEdit.keymap(b)
    shell_mode.keymap_dict = help_mode.keymap_dict = LineEdit.keymap(b)

    ModalInterface([sjulia_prompt, julia_prompt, shell_mode, help_mode, search_prompt, prefix_prompt])
#    ModalInterface([sjulia_prompt, julia_prompt, shell_mode, help_mode, search_prompt, prefix_prompt])
end
