const SJDOCS = Dict{Symbol,String}()

const SJEXAMPLES = Dict{Symbol,Array{Any,1}}()

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
            format_sjexamples(q)
        else
            println("No documentation for '", string(q),"'.")
        end
        return true
    else
        return false
    end
end

macro sjexamp(sym,strs...)
    if ! haskey(SJEXAMPLES,sym)
        SJEXAMPLES[sym] = Array(Any,0)
    end
    exs = SJEXAMPLES[sym]
    push!(exs,strs)
end

function format_sjexamples(sym)
    if haskey(SJEXAMPLES,sym)
        exs = SJEXAMPLES[sym]
        for strs in exs
            println()
            println("Example")            
            format_sjexample(strs)
        end
    end
end

function format_sjexample(strs)
    for i in 1:2:length(strs)-1
        println("sjulia> ", strs[i])
        if length(strs[i+1]) > 0 println("",strs[i+1]) end
        if i < length(strs) - 2
            println()
        end
    end        
end

# evaluate example code, and push corresponding strings onto
# the history, so the user can modify the example.
function do_example(strs)
    println("Example:")
    for i in 1:2:length(strs)-1
        println("sjulia> ", strs[i])
        ex = parse(strs[i])
        println(eval(Expr(:macrocall,symbol("@ex"), ex)))
        sj_add_history(strs[i])
        if i < length(strs) - 2
            println()
        end        
    end
end

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
#    str = rstrip(bytestring(s.input_buffer))
    str = rstrip(bytestring(s))
    isempty(strip(str)) && return
    mode = :symjulia
    length(hist.history) > 0 &&
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
