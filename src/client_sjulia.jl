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

# See base/client.jl

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

    Base.eval(parse("global have_color = true"))  # get colors for warn and error

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

