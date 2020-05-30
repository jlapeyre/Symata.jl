import REPL
import REPL: LineEdit, REPLCompletions

mutable struct SymataCompletionProvider <: REPL.CompletionProvider
    r::REPL.LineEditREPL
end

function symata_respond(f, repl, main; pass_empty = false)
    return function do_respond(s, buf, ok)
        if !ok
            return transition(s, :abort)
        end
        line = String(take!(buf))
        if !isempty(line) || pass_empty
            REPL.reset(repl)
            local response
            try
                ast = Base.invokelatest(f, line)
                response0 = REPL.eval_with_backend(ast, REPL.backend(repl))
                response = (wrapout(response0[1]), response0[2])
            catch
                response = (catch_stack(), true)
            end
            REPL.print_response(repl, response, !REPL.ends_with_semicolon(line), Base.have_color)
            # The following works more-or-less for ascii-art math. But, the LaTeX needs tweaking for this.
                # tex2mail interprets a negative thin space as several positive spaces
                # lstr = latex_string(wval)
                # str = readstring(pipeline(`echo $lstr`, `/home/lapeyre/.julia/v0.5/Nemo/local/bin/tex2mail --ignorefonts`))
                # print("\n\n", str)
        end
        REPL.prepare_next(repl)
        REPL.reset_state(s)
        return s.current_mode.sticky ? true : LineEdit.transition(s, main)
    end
end

# This code only works in a dumb terminal. Also maybe when running tests ?
function symata_print_response(repl::REPL.AbstractREPL, val::Any, args...)
    REPL.print_response(repl, wrapout(val), args...)
end

function iscomment(s)
    m = match(r"^\s*#", s) # for v0.7, which returns more LineNumberNode's
    return m == nothing ? false : true
end

function Symata_parse_REPL_line(linein)
    iscomment(linein) && return nothing
    Base.parse_input_line("@Symata.symfull " * sjpreprocess_interactive(linein))
end

function REPL.LineEdit.complete_line(c::SymataCompletionProvider, s)
    partial = symata_beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = symata_completions(full, lastindex(partial))
    return ret, partial[range], should_complete
end

const sorted_builtins = Array{String}(undef, 0)
function populate_builtins()
    b = protectedsymbols_strings()
    for s in b
        push!(sorted_builtins,s)
    end
    return nothing
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

# New in v0.7, we don't implement all of this yet
# const Completions = Tuple{Vector{Completion}, UnitRange{Int64}, Bool}

function symata_completions(string, pos, context_module=Main)
    # First parse everything up to the current position
    partial = string[1:pos]
    inc_tag = Base.incomplete_tag(Meta.parse(partial, raise=false, depwarn=false))
    if inc_tag in [:cmd, :string]
        m = match(r"[\t\n\r\"'`@\$><=;|&\{]| (?!\\)", reverse(partial))
#       m = match(r"[\t\n\r\"`><=*?|]| (?!\\)", reverse(partial)) line from v0.7 stdlib
        startpos = nextind(partial, reverseind(partial, m.offset))
        r = startpos:pos

        paths, r, success = REPLCompletions.complete_path(replace(string[r], r"\\ " => " "), pos)

        # if inc_tag == :string &&
        #    length(paths) == 1 &&  # Only close if there's a single choice,
        #    !isdir(expanduser(replace(string[startpos:start(r)-1] * paths[1], r"\\ " => " "))) &&  # except if it's a directory
        #    (length(string) <= pos || string[pos+1] != '"')    # or there's already a " at the cursor.
        #     paths[1] *= "\""
        # end
        if inc_tag == :string &&
           length(paths) == 1 &&  # Only close if there's a single choice,
           !isdir(expanduser(replace(string[startpos:prevind(string, first(r))] * paths[1].path,
                                     r"\\ " => " "))) &&  # except if it's a directory
           (length(string) <= pos ||
            string[nextind(string,pos)] != '"')  # or there's already a " at the cursor.
            paths[1] = PathCompletion(paths[1].path * "\"")
        end

        #Latex symbols can be completed for strings
        (success || inc_tag==:cmd) && return sort!(paths, by=p->p.path), r, success
    end

    ok, ret = REPL.REPLCompletions.bslash_completions(string, pos)
    ok && return ret

    # Make sure that only bslash_completions is working on strings
    inc_tag==:string && return String[], 0:-1, false

     if inc_tag == :other && REPLCompletions.should_method_complete(partial)
        frange, method_name_end = REPLCompletions.find_start_brace(partial)
        ex = symata_syntax_deprecation_warnings(false) do
            Meta.parse(partial[frange] * ")", raise=false)
        end
        if isa(ex, Expr) && ex.head==:call
            return complete_methods(ex), start(frange):method_name_end, false
        end
    elseif inc_tag == :comment
        return String[], 0:-1, false
    end

    dotpos = something(findprev(isequal('.'), string, pos), 0)
    startpos = nextind(string, something(findprev(in(REPLCompletions.non_identifier_chars), string, pos), 0))

    ffunc = (mod,x)->true
    suggestions = String[]
    startpos == 0 && (pos = -1)
    dotpos <= startpos && (dotpos = startpos - 1)
    s = string[startpos:pos]
    append!(suggestions, complete_symata_builtins(s))
#    return sort!(unique(suggestions), by=completion_text), (dotpos+1):pos, true  # v0.7  we don't define completion_text
    return sort(unique(suggestions)), (dotpos+1):pos, true
end

# Code modified from Cxx.jl
global RunSymataREPL

const initial_symata_REPL_prompt = "symata 1> "
# Handle the featureful REPL
function RunSymataREPL(repl::REPL.LineEditREPL)

    # Completion list for Symata
    populate_builtins()

    # Setup symata symata_prompt
    symata_REPL_mode =
        LineEdit.Prompt(initial_symata_REPL_prompt;
          # Copy colors from the prompt object
                        prompt_prefix=Base.text_colors[:blue],
                        complete = SymataCompletionProvider(repl),
                        on_enter = REPL.return_callback
                        )
    symata_REPL_mode.on_done =
        symata_respond(Symata_parse_REPL_line,
               repl, symata_REPL_mode) # stay in symjulia

    main_mode = repl.interface.modes[1]

    push!(repl.interface.modes, symata_REPL_mode)

    hp = main_mode.hist
    hp.mode_mapping[:symata] = symata_REPL_mode
    symata_REPL_mode.hist = hp

    enter_symata_key = '='
    symata_keymap = Dict{Any,Any}(
        enter_symata_key => function (s,args...)
        if isempty(s)
          if !haskey(s.mode_state,symata_REPL_mode)
             s.mode_state[symata_REPL_mode] = LineEdit.init_state(repl.t,symata_REPL_mode)
          end
          LineEdit.transition(s,symata_REPL_mode)
       else
           LineEdit.edit_insert(s,enter_symata_key)
      end
    end
    )

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    mk = REPL.mode_keymap(main_mode)

    b = Dict{Any,Any}[skeymap, mk, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    symata_REPL_mode.keymap_dict = LineEdit.keymap(b)

    main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, symata_keymap);
    return nothing
end

# Handles BasicREPL and StreamREPL
function RunSymataREPL(repl)
    symata_run_repl(repl)
end

function symata_run_repl(repl::REPL.AbstractREPL, consumer = x -> nothing)
    repl_channel = Channel(1)
    response_channel = Channel(1)
    backend = REPL.start_repl_backend(repl_channel, response_channel)
    symata_run_frontend(repl, REPL.REPLBackendRef(repl_channel,response_channel))
    return backend
end

symata_run_repl(stream::IO) = run_repl(REPL.StreamREPL(stream))

# For BasicREPL and StreamREPL only
const symataprompt = "symata > "
function symata_run_frontend(repl::REPL.BasicREPL, backend::REPL.REPLBackendRef)
    d = REPL.REPLDisplay(repl)
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
            if ! REPL.ends_with_semicolon(line)
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

function symata_run_frontend(repl::REPL.StreamREPL, backend::REPL.REPLBackendRef)
    have_color = Base.have_color
    banner(repl.stream, have_color)
    d = REPL.REPLDisplay(repl)
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
            if ! REPL.ends_with_semicolon(line)
                symata_print_response(repl, val, bt, true, have_color)
            end
        end
    end
    # Terminate Backend
    put!(repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
end

function transition_to_symata()
    mistate = Base.active_repl.mistate
    symata_prompt = Base.active_repl.interface.modes[end]
    symata_prompt.sticky = true
    REPL.LineEdit.transition(mistate, symata_prompt) # prevent 42 appearing on input line
end
