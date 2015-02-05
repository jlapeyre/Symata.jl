## Documentation and Examples

# Documentation is entered in files like this:
# @sjdoc SomeHead "SomeHead does this."

# @sjseealso( Sym, Sym1, Sym2, etc.)
# Sets Seealso's for Sym to Sym1, ...

# @sjseealso_group(Sym1,Sym2,...) set's each symbols see alsos to
# all of the others.

# Examples are entered like this. All these pairs together constitute and example
# @sjexamp( SomeHead,
#    "first input", "first output",
#    "second ....
# Examples are printed along with the document.

# Repeating @sjexamp pushes more examples onto an array for SomeHead

## do_example evaluates the examples (only input strings) and
# prints input and output, and pushes input strings onto the history
# so the user can play with them.

# Single doc string for a symbol. Retrieved by:
# sjulia>?, SomeHead
# NOTE THE COMMA. it is just there so that Julia will parse the command.
const SJDOCS = Dict{Symbol,String}()
const SJEXAMPLES = Dict{Symbol,Array{Any,1}}()
const SJSEEALSO = Dict{Symbol,Array{Any,1}}()

macro sjdoc(sym,str)
    SJDOCS[sym] = str
    nothing
end

# Called from the macro @ex which parses cli input to see if we have a help query
# or an expression.
function check_help_query(ex)
    if is_type(ex,Expr) && ex.head == :tuple && ex.args[1] == :?
        q = ex.args[2]
        if haskey(SJDOCS,q)
            print(SJDOCS[q])
            format_see_alsos(q)
            format_sjexamples(q)
        else
            println("No documentation for '", string(q),"'.")
        end
        return true
    else
        return false
    end
end

# use in source files to define examples for a symbol (representing a Head)
# maybe this should be a function
macro sjexamp(sym,strs...)
    if ! haskey(SJEXAMPLES,sym)
        SJEXAMPLES[sym] = Array(Any,0)
    end
    exs = SJEXAMPLES[sym]
    ar = Array(Any,0)
    for i in 1:length(strs)
        push!(ar,eval(strs[i]))
    end
    push!(exs,ar)
end

# set seealsos. clobbers existing
macro sjseealso(source_sym, targ_syms...)
    SJSEEALSO[source_sym] = targ_syms
end

# each sym seealso's all others
# This does not clobber existing seealsos
macro sjseealso_group(syms...)
    local na
    for src in syms
        if ! haskey(SJSEEALSO,src)
            na = Array(Any,0)
            SJSEEALSO[src] = na
        else
            na = SJSEEALSO[src]
        end
        for targ in syms
            if src == targ continue end
            push!(na,targ)
        end
    end
end

## Print see alsos for sym
function format_see_alsos(sym)
    ! haskey(SJSEEALSO,sym) && return
    sas = SJSEEALSO[sym]
    length(sas) == 0 && return
    print("See also ")
    len = length(sas)
    if len == 1
        println(string(sas[1]),".")
        return
    elseif len == 2
        println(string(sas[1]), " and ", string(sas[2]),".")
        return
    else
        for i in 1:len-1
            print(string(sas[i]),", ")
        end
        println("and ", string(sas[end]),".")
    end
end

#  For printing along with doc string.
function format_sjexamples(sym)
    if haskey(SJEXAMPLES,sym)
        exs = SJEXAMPLES[sym]
        println("Examples")        
        for strs in exs
            println()
            format_sjexample(strs)
        end
    end
end

# Format just one example for printing with docs. Do not evaluate.
# Each items in example is either an explanatory string,
# or a 2-tuple of input and output.
function format_sjexample(lines)
    local ins, outs, expl
    for i in 1:length(lines)
        ln = lines[i]
        if is_type_less(ln,AbstractString)
            println(ln)
        else
            expl = ""
            if length(ln) == 2
                (ins,outs) = ln
            else
                (ins,outs,expl) = ln
            end
            if length(expl) > 0
                println("# ",expl)
            end
            println("sjulia> ", ins)
            if length(outs) > 0 println(outs) end
            if i < length(lines)
                println()
            end
        end
    end
end

# evaluate example code, and push corresponding input strings onto
# the history, so the user can modify the example.
function do_example(lines)
    len = length(lines)
    local ins, outs, expl    
    for i in 1:len
        ln = lines[i]
        if is_type_less(ln,AbstractString)
            println(ln)
        else
            expl = ""
            if length(ln) == 2
                (ins,outs) = ln
            else
                (ins,outs,expl) = ln
            end
            if length(expl) > 0
                println("# ",expl)
            end            
            println("sjulia> ", ins)
            ex = parse(ins)
            res = (eval(Expr(:macrocall,symbol("@ex"), ex)))
            if res != nothing println(res) end
            sj_add_history(ins)
            if i < len
                println()
            end
        end
    end
end

## evaluate the nth example for symbol sym
function do_example_n(sym,n)
    if haskey(SJEXAMPLES,sym)
        exs = SJEXAMPLES[sym]
        if length(exs) < n
            println("There is no example $n for ", string(sym), ".")
            return
        else
            do_example(exs[n])
        end
    else
        println("There are no examples for ", string(sym), ".")
    end
end

## evaluate all examples for symbol sym
function do_examples(sym)
    if haskey(SJEXAMPLES,sym)
        exs = SJEXAMPLES[sym]
        for strs in exs
            do_example(strs)
        end
    end
end

## Information on hierarchy of fields:

# julia> Base.active_repl.interface.modes[2]
# "Prompt(\"sjulia> \",...)"

# julia> typeof(Base.active_repl.interface.modes[2])
# Base.LineEdit.Prompt

# julia> typeof(Base.active_repl.interface.modes[2].hist)
# Base.REPL.REPLHistoryProvider

# Push a string onto history in sjulia (symjulia) mode
# modified from add_history in REPL.jl
function sj_add_history(s::String)
    sj_add_history(Base.active_repl.interface.modes[2].hist,s)
end

function sj_add_history(hist::Base.REPL.REPLHistoryProvider, s::String)
#    str = rstrip(bytestring(s.input_buffer))  # original line
    str = rstrip(bytestring(s))
    isempty(strip(str)) && return
    mode = :symjulia
    length(hist.history) > 0 &&  # we could be more clever and not push the same example sequence twice
    mode == hist.modes[end] && str == hist.history[end] && return
    push!(hist.modes, mode)
    push!(hist.history, str)
    hist.history_file == nothing && return
    entry = """
    # time: $(strftime("%Y-%m-%d %H:%M:%S %Z", time()))
    # mode: $mode
    $(Base.replace(str, r"^"ms, "\t"))
    """
    # TODO: write-lock history file
    seekend(hist.history_file)
    print(hist.history_file, entry)
    flush(hist.history_file)
end
