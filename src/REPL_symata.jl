import Base: REPL, Terminals
import Base.REPL: LineEdit, REPLCompletions

import Base.LineEdit: CompletionProvider, transition

import Base.REPL: LineEditREPL, BasicREPL, StreamREPL, ends_with_semicolon,
REPLCompletionProvider, return_callback, Prompt, respond, ShellCompletionProvider,
REPLHistoryProvider, find_hist_file, print_response, outstream, hist_from_file, hist_getline,
history_reset_state, LatexCompletions, edit_insert, mode_keymap, ModalInterface, reset, send_to_backend,
 backend, prepare_next, reset_state, ends_with_semicolon

import Base.REPL.REPLCompletions: bslash_completions, non_identifier_chars, should_method_complete,
find_start_brace, complete_path

import Base.REPL: AbstractREPL, start_repl_backend, run_frontend, REPLBackendRef,
   LineEditREPL, REPLDisplay, run_interface, setup_interface, LineEdit, REPL

import Base.REPL: display

# This prompt is used for the dumb terminal
const symataprompt = "symata > "

mutable struct SymataCompletionProvider <: CompletionProvider
    r::LineEditREPL
end

function symata_respond(f, repl, main; pass_empty = false)
    (s,buf,ok)->begin
        if !ok
            return transition(s, :abort)
        end
        line = String(take!(buf))
        if !isempty(line) || pass_empty
            reset(repl)
            val, bt = send_to_backend(f(line), backend(repl))
            if !ends_with_semicolon(line) || bt !== nothing
                wval = wrapout(val)
                REPL.print_response(repl, wval, bt, true, Base.have_color)

                # The following works more-or-less for ascii-art math. But, the LaTeX needs tweaking for this.
                # tex2mail interprets a negative thin space as several positive spaces
                # lstr = latex_string(wval)
                # str = readstring(pipeline(`echo $lstr`, `/home/lapeyre/.julia/v0.5/Nemo/local/bin/tex2mail --ignorefonts`))
                # print("\n\n", str)

            end
        end
        prepare_next(repl)
        reset_state(s)
        s.current_mode.sticky || transition(s, main)
    end
end

# This code only works in a dumb terminal. Also maybe when running tests ?
function symata_print_response(repl::AbstractREPL, val::Any, args...)
    newval = wrapout(val)
    REPL.print_response(repl, newval, args...)
end

## Don't follow this idea a.t.m.
# function symata_parse_julia_syntax_repl_line(line)
# end
# const SymataParsers = Dict{Any,Any}(
#                                     :juliasyntax => symata_parse_julia_syntax_repl_line
#                                     )

function Symata_parse_REPL_line(line)
    line = sjpreprocess_interactive(line)
# FIXME: upgrade syntax_deprecation_warnings for v0.7
    symata_syntax_deprecation_warnings(false) do
        Base.parse_input_line("@Symata.sym " * line)
    end
end

function Base.LineEdit.complete_line(c::SymataCompletionProvider, s)
    partial = symata_beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = symata_completions(full, endof(partial))
    return ret, partial[range], should_complete
end

# FIXME: this should not be global
const sorted_builtins = Array{Compat.String}(undef, 0)
function populate_builtins()
    b = protectedsymbols_strings()
    for s in b
        push!(sorted_builtins,s)
    end
    nothing
end

const _temporary_symbol_regex = r"#"
function add_completion_symbols(syms...)
    for sym in syms
        (match(_temporary_symbol_regex,string(sym)) == nothing) || continue
        if ! (sym in sorted_builtins)
            push!(sorted_builtins,sym)
        end
    end
    sort!(sorted_builtins)
    nothing
end

function remove_completion_symbols(syms...)
    for sym in syms
        deleteat!(sorted_builtins, findall((in)(sorted_builtins), (string(sym),)))
    end
    nothing
end

function complete_symata_builtins(s::AbstractString)
    r = searchsorted(sorted_builtins, s)
    i = first(r)
    n = length(sorted_builtins)
    while i <= n && startswith(sorted_builtins[i],s)
        r = first(r):i
        i += 1
    end
    sorted_builtins[r]
end

function symata_completions(string, pos)
    # First parse everything up to the current position
    partial = string[1:pos]
    inc_tag = symata_syntax_deprecation_warnings(false) do
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
    inc_tag==:string && return String[], 0:-1, false

     if inc_tag == :other && should_method_complete(partial)
        frange, method_name_end = find_start_brace(partial)
        ex = symata_syntax_deprecation_warnings(false) do
            parse(partial[frange] * ")", raise=false)
        end
        if isa(ex, Expr) && ex.head==:call
            return complete_methods(ex), start(frange):method_name_end, false
        end
    elseif inc_tag == :comment
        return String[], 0:-1, false
    end

    dotpos = rsearch(string, '.', pos)
    startpos = nextind(string, rsearch(string, non_identifier_chars, pos))

    ffunc = (mod,x)->true
    suggestions = String[]
    startpos == 0 && (pos = -1)
    dotpos <= startpos && (dotpos = startpos - 1)
    s = string[startpos:pos]
    append!(suggestions, complete_symata_builtins(s))
    return sort(unique(suggestions)), (dotpos+1):pos, true
end

# Code modified from Cxx.jl

global RunSymataREPL

# Handles BasicREPL
function RunSymataREPL(repl)
    symata_run_repl(repl)
end

function RunSymataREPL(repl::LineEditREPL)

    # Completion list for Symata
    populate_builtins()

    # Setup symata symata_prompt
    symata_prompt =
        LineEdit.Prompt(symataprompt;
          # Copy colors from the prompt object
                        prompt_prefix=Base.text_colors[:blue],
                        complete = SymataCompletionProvider(repl),
                        on_enter = return_callback
                        )
    symata_prompt.on_done =
        symata_respond(Symata_parse_REPL_line,
               repl, symata_prompt) # stay in symjulia

    main_mode = repl.interface.modes[1]

    push!(repl.interface.modes,symata_prompt)

    hp = main_mode.hist
    hp.mode_mapping[:symata] = symata_prompt
    symata_prompt.hist = hp

    enter_symata_key = '='
    symata_keymap = Dict{Any,Any}(
           enter_symata_key => function (s,args...)
            if isempty(s)
                if !haskey(s.mode_state,symata_prompt)
                    s.mode_state[symata_prompt] = LineEdit.init_state(repl.t,symata_prompt)
                end
                LineEdit.transition(s,symata_prompt)
            else
                LineEdit.edit_insert(s,enter_symata_key)
            end
           end
     )

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    mk = REPL.mode_keymap(main_mode)

    b = Dict{Any,Any}[skeymap, mk, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    symata_prompt.keymap_dict = LineEdit.keymap(b)

    main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, symata_keymap);
    nothing
end


import Base.REPL: AbstractREPL, start_repl_backend, run_frontend, REPLBackendRef,
   LineEditREPL, REPLDisplay, run_interface, setup_interface, LineEdit, REPL

function symata_run_repl(repl::AbstractREPL, consumer = x->nothing)
    repl_channel = Channel(1)
    response_channel = Channel(1)
    backend = start_repl_backend(repl_channel, response_channel)
    symata_run_frontend(repl, REPLBackendRef(repl_channel,response_channel))
    return backend
end

symata_run_repl(stream::IO) = run_repl(StreamREPL(stream))

function symata_run_frontend(repl::LineEditREPL, backend)
    d = REPLDisplay(repl)
    dopushdisplay = repl.specialdisplay === nothing && !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    if !isdefined(repl,:interface)
        interface = repl.interface = symata_setup_interface(repl)
        set_symata_prompt(1)  ## !! This could be handled in a hook. Make pull request of my hook code.
    else
        interface = repl.interface
    end
    repl.backendref = backend
    #### Use this to add a mode to the stock repl
    run_interface(repl.t, interface)
    dopushdisplay && popdisplay(d)
end

function symata_run_frontend(repl::BasicREPL, backend::REPLBackendRef)
    d = REPLDisplay(repl)
    dopushdisplay = !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    repl_channel, response_channel = backend.repl_channel, backend.response_channel
    hit_eof = false
    while true
        Base.reseteof(repl.terminal)
        write(repl.terminal, symataprompt)
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
                    catch
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
            ast = Symata_parse_REPL_line(line)
            (isa(ast,Expr) && ast.head == :incomplete) || break
        end
        if !isempty(line)
            put!(repl_channel, (ast, 1))
            val, bt = take!(response_channel)
            if !ends_with_semicolon(line)
                symata_print_response(repl, val, bt, true, false)
            end
        end
        write(repl.terminal, '\n')
        ((!interrupted && isempty(line)) || hit_eof) && break
    end
    # terminate backend
    put!(repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
end

function symata_run_frontend(repl::StreamREPL, backend::REPLBackendRef)
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
        print(repl.stream, symataprompt)
        if have_color
            print(repl.stream, input_color(repl))
        end
        line = readline(repl.stream)
        if !isempty(line)
            ast = Symata_parse_REPL_line(line)
            if have_color
                print(repl.stream, Base.color_normal)
            end
            put!(repl_channel, (ast, 1))
            val, bt = take!(response_channel)
            if !ends_with_semicolon(line)
                symata_print_response(repl, val, bt, true, have_color)
            end
        end
    end
    # Terminate Backend
    put!(repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
end

function symata_setup_interface(repl::LineEditREPL; hascolor = repl.hascolor, extra_repl_keymap = Dict{Any,Any}[])
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
    replc = REPLCompletionProvider()

    # Completions for Symata
    symata_replc = SymataCompletionProvider(repl)

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

    # Completion list for Symata
    populate_builtins()

    # Setup symata symata_prompt
    symata_prompt =
        LineEdit.Prompt(symataprompt;
                        prompt_prefix=Base.text_colors[:blue],
                        prompt_suffix = hascolor ?
                        (repl.envcolors ? Base.input_color : repl.input_color) : "",
                        complete = symata_replc,
                        on_enter = return_callback  # allows multiline input
                        )

    symata_prompt.on_done =
        symata_respond(Symata_parse_REPL_line,
                      repl, symata_prompt) # stay in symjulia

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
## FIXME: add branch, or accomodate v0.5
                        #       if VERSION > v"0.5" 
         complete = ShellCompletionProvider(),
       # else
       #   complete = ShellCompletionProvider(repl)
       # end,
        # Transform "foo bar baz" into `foo bar baz` (shell quoting)
        # and pass into Base.repl_cmd for processing (handles `ls` and `cd`
        # special)
        on_done = respond(repl, julia_prompt) do line
            Expr(:call, :(Base.repl_cmd), macroexpand(Expr(:macrocall, Symbol("@cmd"),line)), outstream(repl))
        end)


    ################################# Stage II #############################

    # Setup history
    # We will have a unified history for all REPL modes
    hp = REPLHistoryProvider(Dict{Symbol,Any}(:julia => julia_prompt,
                                              :symata => symata_prompt,
                                              :shell => shell_mode,
                                              :help  => help_mode))
    if repl.history_file
        try
            hist_path = find_hist_file()
            f = open(hist_path, true, true, true, false, false)
            finalizer(replc, replc->close(f))
            hist_from_file(hp, f, hist_path)
        catch e
            print_response(repl, e, catch_backtrace(), true, Base.have_color)
            println(outstream(repl))
            info("Disabling history file for this session.")
            repl.history_file = false
        end
    end
    history_reset_state(hp)
    julia_prompt.hist = hp
    symata_prompt.hist = hp
    shell_mode.hist = hp
    help_mode.hist = hp

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    search_prompt.complete = LatexCompletions()

    # Canonicalize user keymap input
    if isa(extra_repl_keymap, Dict)
        extra_repl_keymap = [extra_repl_keymap]
    end

    enter_symata_key = '='

    repl_keymap = Dict{Any,Any}(
         enter_symata_key => function (s,args...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                if !haskey(s.mode_state,symata_prompt)
                    s.mode_state[symata_prompt] = LineEdit.init_state(repl.t,symata_prompt)
                end
                buf = copy(LineEdit.buffer(s))   # allows us to edit a line of julia input
                if LineEdit.mode(s) == symata_prompt
                  LineEdit.transition(s, julia_prompt) do
                     LineEdit.state(s, julia_prompt).input_buffer = buf
                  end
                else
                    LineEdit.transition(s, symata_prompt) do
                    LineEdit.state(s, symata_prompt).input_buffer = buf
                    end
               end
            else
                LineEdit.edit_insert(s,enter_symata_key)
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
        '?' => function (s,o...)  # Disable this for symata mode, because we use '?' for symata help.
            if LineEdit.mode(s) != symata_prompt && (isempty(s) || position(LineEdit.buffer(s)) == 0)
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
            input = String(take!(sbuffer))
            oldpos = start(input)
            firstline = true
            while !done(input, oldpos) # loop until all lines have been executed
                ast, pos = symata_syntax_deprecation_warnings(false) do
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

    julia_prompt.keymap_dict = LineEdit.keymap(a)

    mk = mode_keymap(julia_prompt)

    b = Dict{Any,Any}[skeymap, mk, prefix_keymap, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    prepend!(b, extra_repl_keymap)

    symata_prompt.keymap_dict = LineEdit.keymap(a)
    shell_mode.keymap_dict = help_mode.keymap_dict = LineEdit.keymap(b)

    ModalInterface([symata_prompt, julia_prompt, shell_mode, help_mode, search_prompt, prefix_prompt])
end
