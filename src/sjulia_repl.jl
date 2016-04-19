import Base: LineEdit, REPL, Terminals

import Base.LineEdit: CompletionProvider
import Base.REPL: LineEditREPL, BasicREPL, StreamREPL, ends_with_semicolon, print_response
import Base.REPLCompletions: bslash_completions, non_identifier_chars, should_method_complete,
find_start_brace, complete_path

import Base: TTY

import Base.Terminals
import Base.REPL

import Base: active_repl

import Base: text_colors, possible_formatting_symbols, available_text_colors, available_text_colors_docstring,
have_color, default_color_warn, default_color_info, default_color_input,  default_color_answer,
color_normal, repl_color, warn_color, info_color, input_color, answer_color, repl_cmd,
display_error, eval_user_input, syntax_deprecation_warnings, parse_input_line, incomplete_tag,
try_include, process_options, load_machine_file, repl_hooks,
atreplinit, _atreplinit

##################################################

function SJulia_start()
    opts = Base.JLOptions()    
    startup               = (opts.startupfile != 2)
    history_file          = (opts.historyfile != 0)
    quiet                 = (opts.quiet != 0)

    color_set             = (opts.color != 0)
    global have_color     = (opts.color == 1)

    local term
    global active_repl
    global active_repl_backend

#    repl = true
    
    if !isa(STDIN,TTY)
        global is_interactive |= !isa(STDIN,Union{File,IOStream})
        color_set || (global have_color = false)
    else
        term = Terminals.TTYTerminal(get(ENV,"TERM",@windows? "" : "dumb"),STDIN,STDOUT,STDERR)
        global is_interactive = true
        color_set || (global have_color = Terminals.hascolor(term))
        quiet || REPL.banner(term,term)
        if term.term_type == "dumb"
            active_repl = REPL.BasicREPL(term)
            quiet || warn("Terminal not fully functional")
        else
            active_repl = REPL.LineEditREPL(term, true)
            active_repl.history_file = history_file
            active_repl.hascolor = have_color
        end
        # Make sure any displays pushed in .juliarc.jl ends up above the
                # REPLDisplay
        pushdisplay(REPL.REPLDisplay(active_repl))
    end

    if !isa(STDIN,TTY)
        # note: currently IOStream is used for file STDIN
        if isa(STDIN,File) || isa(STDIN,IOStream)
            # reading from a file, behave like include
            eval(Main,parse_input_line(readstring(STDIN)))
        else
            # otherwise behave repl-like
            while !eof(STDIN)
                eval_user_input(parse_input_line(STDIN), true)
            end
        end
    else
        _atreplinit(active_repl)
        sjulia_run_repl(active_repl, backend->(global active_repl_backend = backend))
        exit
    end
    exit
end
    
##################################################################


type SJuliaCompletionProvider <: CompletionProvider
    r::LineEditREPL
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
function RunSJuliaREPL(repl)

    # Completion list for SJulia
    populate_builtins()

    # Setup sjulia sjulia_prompt
    sjulia_prompt =
        LineEdit.Prompt("sjulia > ";
          # Copy colors from the prompt object
                        prompt_prefix=Base.text_colors[:blue],
                        complete = SJuliaCompletionProvider(repl)
#                        prompt_suffix = hascolor ?  hascolor not defined
#                        (repl.envcolors ? Base.input_color : repl.input_color) : ""
                        )
    sjulia_prompt.on_done =
        REPL.respond(
              (line)->(Base.parse_input_line("@SJulia.ex $line")),
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
        interface = repl.interface = setup_interface(repl)
    else
        interface = repl.interface
    end
    repl.backendref = backend
    RunSJuliaREPL(repl)
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
            ast = Base.parse_input_line("@SJulia.ex $line")
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
            ast = Base.parse_input_line("@SJulia.ex $line")            
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

