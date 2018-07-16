import REPL

# import Base: text_colors,  default_color_input,  default_color_answer,
#        color_normal, input_color, answer_color, repl_cmd,
#        display_error, eval_user_input, parse_input_line, incomplete_tag,
#        repl_hooks, atreplinit, _atreplinit

##################################################

# See base/client.jl

# This function is only meant to be used via
# julia -i -e "using Symata"
# Otherwise it is never called.
#
# This code appears to be too old. Better to copy what
# is done in recent client.jl
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

    if !isa(stdin, Base.TTY)
        global is_interactive |= !isa(stdin, Union{File,IOStream})
        color_set || (global have_color = false)
    else
        term = REPL.Terminals.TTYTerminal(get(ENV,"TERM", @static Sys.iswindows() ? "" : "dumb"), stdin, stdout, stderr)
        global is_interactive = true
        color_set || (global have_color = REPL.Terminals.hascolor(term))
        Base.eval(Meta.parse("global have_color = " * string(have_color))) # get colors for warn and error
        quiet || REPL.banner(term,term)
        if term.term_type == "dumb"
            active_repl = BasicREPL(term)
            quiet || @warn("Terminal not fully functional")
        else
            active_repl = REPL.LineEditREPL(term, true)
            active_repl.history_file = history_file
            active_repl.hascolor = have_color
        end
        # Make sure any displays pushed in .juliarc.jl ends up above the REPLDisplay
        pushdisplay(REPL.REPLDisplay(active_repl))
    end
    if @isdefined(stdout)
        setsymval(:STDOUT, stdout)
    else
        @warn("**** CANT FIND STDOUT")
    end
    if @isdefined(stderr)
        setsymval(:STDERR, stderr)
    else
        @warn("**** CANT FIND STDERR")
    end
    if @isdefined(devnull)
        setsymval(:DevNull, devnull)
    end
    if !isa(stdin, Base.TTY)
        # note: currently IOStream is used for file STDIN
        if isa(stdin,File) || isa(stdin,IOStream)
            # reading from a file, behave like include
            Core.eval(Main,parse_input_line(readstring(stdin)))
        else
            # otherwise behave repl-like
            while !eof(stdin)
                eval_user_input(parse_input_line(stdin), true)
            end
        end
    else
        Base._atreplinit(active_repl)
        symata_run_repl(active_repl, backend->(global active_repl_backend = backend))
        exit
    end
    exit
end

# This is another fine mess you've gotten us into.
function symata_have_dumb_terminal()
    (! isdefined_base_active_repl()) && (! isdefined_symata_active_repl()) && return true
    (isdefined_base_active_repl() && typeof(Base.active_repl) == REPL.BasicREPL) ||
    (isdefined_symata_active_repl() && typeof(Symata.active_repl) == REPL.BasicREPL)
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

function  set_symata_prompt(s::T) where T<:AbstractString
    symata_have_dumb_terminal() && return # not implemented yet
    (Symata.symata_repl_mode().prompt = s)
end

set_symata_prompt(n::Int) = set_symata_prompt("symata " * string(n) * "> ")
get_symata_prompt(s::AbstractString) = Symata.symata_repl_mode().prompt

function symata_repl()
    isdefined_symata_active_repl() && return Symata.active_repl
    isdefined_base_active_repl() && return Base.active_repl
    error("Can't find the active REPL.")
end
