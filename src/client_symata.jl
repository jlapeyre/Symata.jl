import Base: LineEdit, REPL, Terminals

import Base.LineEdit: CompletionProvider
import Base.REPL: LineEditREPL, BasicREPL, StreamREPL, ends_with_semicolon, print_response
import Base.REPLCompletions: bslash_completions, non_identifier_chars, should_method_complete,
find_start_brace, complete_path

import Base: TTY

import Base.Terminals
import Base.REPL

#import Base: active_repl, have_color

import Base: text_colors, possible_formatting_symbols, available_text_colors, available_text_colors_docstring,
default_color_warn, default_color_info, default_color_input,  default_color_answer,
color_normal, repl_color, warn_color, info_color, input_color, answer_color, repl_cmd,
display_error, eval_user_input, syntax_deprecation_warnings, parse_input_line, incomplete_tag,
try_include, process_options, load_machine_file, repl_hooks,
atreplinit, _atreplinit

##################################################

# See base/client.jl

function Symata_start()
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
#        term = Terminals.TTYTerminal(get(ENV,"TERM",@windows? "" : "dumb"),STDIN,STDOUT,STDERR)
        term = Terminals.TTYTerminal(get(ENV,"TERM", @compat @static is_windows() ? "" : "dumb"),STDIN,STDOUT,STDERR)
        global is_interactive = true
        color_set || (global have_color = Terminals.hascolor(term))
        Base.eval(parse("global have_color = " * string(have_color)))  # get colors for warn and error
        quiet || REPL.banner(term,term)
        if term.term_type == "dumb"
            active_repl = BasicREPL(term)
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

    if isdefined(:STDOUT)
        setsymval(:STDOUT, STDOUT)
    else
        warn("**** CANT FIND STDOUT")
    end
    if isdefined(:STDERR)
        setsymval(:STDERR, STDERR)
    else
        warn("**** CANT FIND STDERR")
    end
    if isdefined(:DevNull)
        setsymval(:DevNull, DevNull)
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
        symata_run_repl(active_repl, backend->(global active_repl_backend = backend))
        exit
    end
    exit
end

# This is another fine mess you've gotten us into.
function symata_have_dumb_terminal()
    (! isdefined_base_active_repl()) && (! isdefined_symata_active_repl()) && return true
    (isdefined_base_active_repl() && typeof(Base.active_repl) == Base.REPL.BasicREPL) ||
    (isdefined_symata_active_repl() && typeof(Symata.active_repl) == Base.REPL.BasicREPL)
end

isdefined_base_active_repl() = isdefined(Base, :active_repl)
isdefined_symata_active_repl() = isdefined(Symata, :active_repl)

# For now, julia and symata share history. So, either mode 1 or 2 is ok
function symata_repl_mode()
    if  isdefined_symata_active_repl()
        symata_repl().interface.modes[1]
    elseif isdefined_base_active_repl()
        Base.active_repl.interface.modes[6]
    else
        error("Unable to start interactive Symata session. Can't find active REPL mode.")
    end
end

symata_repl_history() = symata_repl_mode().hist

function  set_symata_prompt{T<:AbstractString}(s::T)
    symata_have_dumb_terminal() && return # not implemented yet
    (Symata.symata_repl_mode().prompt = s)
end

#set_symata_prompt(n::Int) = set_symata_prompt("symata " * string(n) * "> ")
set_symata_prompt(n::Int) = set_symata_prompt("symata " * string(n) * "> ")
get_symata_prompt(s::AbstractString) = Symata.symata_repl_mode().prompt

function symata_repl()
    isdefined_symata_active_repl() && return Symata.active_repl
    isdefined_base_active_repl() && return Base.active_repl
    error("Can't find the active REPL.")
end
