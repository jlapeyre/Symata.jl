import Base: LineEdit, REPL

import Base.LineEdit: CompletionProvider
import Base.REPL: LineEditREPL
import Base.REPLCompletions: bslash_completions, non_identifier_chars, should_method_complete,
find_start_brace, complete_path


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
#    comp_keywords = true
    # deleted julia module stuff
    startpos == 0 && (pos = -1)
    dotpos <= startpos && (dotpos = startpos - 1)
    s = string[startpos:pos]
#    comp_keywords && append!(suggestions, complete_keyword(s))
    append!(suggestions, complete_sjulia_builtins(s))
#    append!(suggestions, complete_symbol(s, ffunc))
    return sort(unique(suggestions)), (dotpos+1):pos, true
end


# Code modified from Cxx.jl

global RunSJuliaREPL
function RunSJuliaREPL()

    populate_builtins()

    repl = Base.active_repl

    # Setup sjulia sjulia_prompt
    sjulia_prompt =
        LineEdit.Prompt("sjulia > ";
          # Copy colors from the prompt object
                        prompt_prefix=Base.text_colors[:blue],
                        complete = SJuliaCompletionProvider(repl)
                        )
    #         prompt_prefix="\e[38;5;166m",  # brownish
    #                        prompt_suffix=Base.text_colors[:black]) # input text color, but we don't need to chane it

    sjulia_prompt.on_done =
        REPL.respond(
              (line)->(Base.parse_input_line("@SJulia.ex $line")),
               repl, sjulia_prompt) # stay in symjulia

#        sjulia_prompt.on_done = REPL.respond(repl,sjulia_prompt) do line
#        SJulia.process_sjulia_string(string(line,"\n;"), isTopLevelExpression(C,line), :REPL, 1, 1;
#                compiler = C)
#        end

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
