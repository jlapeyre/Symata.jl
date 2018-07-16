import REPL

function set_symata_prompt(s::T) where T<:AbstractString
    symata_have_dumb_terminal() && return # not implemented yet
    (Symata.symata_repl_mode().prompt = s)
end

# This is another fine mess you've gotten us into.
function symata_have_dumb_terminal()
    (! isdefined_base_active_repl()) && (! isdefined_symata_active_repl()) && return true
    (isdefined_base_active_repl() && typeof(Base.active_repl) == REPL.BasicREPL) ||
    (isdefined_symata_active_repl() && typeof(Symata.active_repl) == REPL.BasicREPL)
end

isdefined_base_active_repl() = isdefined(Base, :active_repl)
isdefined_symata_active_repl() = isdefined(Symata, :active_repl)

function symata_repl()
    isdefined_symata_active_repl() && return Symata.active_repl
    isdefined_base_active_repl() && return Base.active_repl
    error("Can't find the active REPL.")
end

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
set_symata_prompt(n::Int) = set_symata_prompt("symata " * string(n) * "> ")
get_symata_prompt(s::AbstractString) = Symata.symata_repl_mode().prompt
