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

# do_example evaluates the examples (only input strings) and
# prints input and output, and pushes input strings onto the history
# so the user can play with them.

# Single doc string for a symbol. Retrieved by:
# symata>? SomeHead

const SJEXAMPLES = Dict{Symbol,Array{Any,1}}()
const SJSEEALSO = Dict{Symbol,Array{Any,1}}()

## Following moved to mxpr.jl
#const SJDOCS = Dict{Symbol,Any}()
# function sjdocfun(sym,str)
#     SJDOCS[sym] = Markdown.parse(str)
#     nothing
# end
# macro sjdoc(sym,str)
#     SJDOCS[sym] = Markdown.parse(str)
#     nothing
# end

# Called from the macro @sym which parses cli input to see if we have a help query
# or an expression.
# no io stream specified when printing here
function check_doc_query(ex)
    if isa(ex,Expr) && ex.head == :tuple && ex.args[1] == MAGIC_HELP_QUERY_SYMBOL
        if length(ex.args) == 1
            println("Documentation is available for these symbols.")
            print(wrapout(list_documented_symbols()))
            return true
        end
        q = ex.args[2]
        print_doc(q)
        return true
    else
        return false
    end
end

print_doc(s::AbstractString) = print_doc(Symbol(s))

function print_doc(qs...)
    length(qs) == 0 && (println("Try Help(sym) for these symbols."); return list_documented_symbols())
    for q in qs
        check_autoload(q)        
        if haskey(SJDOCS,q)
#            display(Markdown.parse(SJDOCS[q]))
            display(SJDOCS[q])
            println()
            format_see_alsos(q)
            format_sjexamples(q)
        elseif ! write_definition_no_attributes(q)
            println("No documentation for '", string(q),"'.")
        end
        as = get_attributes(q)
        if length(as) > 0
            symprintln("\n Attributes(", string(q), ") = ", mxpr(:List,as...))
        end
        if getkerneloptions(:show_sympy_docs) print_sympy_doc(q) end
    end
    Null
end

# FIXME. We need to have all doc printing functions return strings.
# Then, we either print them or pass them to a front end.
# Commented out parts remain TODO
function retrieve_doc(qs...)
#    length(qs) == 0 && (println("Try Help(sym) for these symbols."); return list_documented_symbols())
    for iq in qs
        q = Symbol(iq)
        check_autoload(q)
        if haskey(SJDOCS,q)
            return(SJDOCS[q])
#            format_see_alsos(q)
#            format_sjexamples(q)
        else
            return("No documentation for '" * string(q) * "'.")
        end
    end
end


function print_sympy_doc(sjsymin::SymString)
    sjsym = Symbol(sjsymin)
    if have_pyfunc_symbol(sjsym)
        println("\nSymPy documentation")
        pysym = lookup_pyfunc_symbol(sjsym)
        spysym = string(pysym)
        printcom = "println(sympy[:$spysym][:__doc__])"
        try eval(parse(printcom))
        catch
            Null
        end
    end
    Null
end

function print_matching_topics(r::Regex)
    i = 0
    lastt = :none
    for (t,doc) in SJDOCS
        if ismatch(r,string(doc))
            i += 1
            println(t)
            lastt = t
        end
    end
    if i == 1
        println(SJDOCS[r])
    end
end

function print_all_docs()
    syms = sort!(collect(keys(SJDOCS)))
    for x in syms
        println("##### ", x)
        print(SJDOCS[x])
        println()
        format_see_alsos(x)
        println()
        if haskey(SJEXAMPLES,x)
            exs = SJEXAMPLES[x]
            println("Examples")
            for strs in exs
                println()
                println("```")
                format_sjexample(strs)
                println("```")
            end
            println()
        end
        println()
    end
    Null
end

function list_documented_symbols()
    syms = sort!(collect(keys(SJDOCS)))
    len = length(syms)
    args = newargs()
    for i in 1:len
        if syms[i] == :ans continue end
        push!(args,syms[i])
    end
    mxpr(:List,args)
end

#### See Alsos

# set seealsos. clobbers existing
macro sjseealso(source_sym, targ_syms...)
    SJSEEALSO[source_sym] = Any[targ_syms...]
end

# each sym seealso's all others
# This does not clobber existing seealsos
# FIXME: Do not add symbols more than once.
macro sjseealso_group(syms...)
    local na
    for src in syms
        if ! haskey(SJSEEALSO,src)
            na = Array{Any}(undef, 0)
            SJSEEALSO[src] = na
        else
            na = SJSEEALSO[src]
        end
        for targ in syms
            if src == targ continue end
            if ! in(targ,na) push!(na,targ) end
        end
    end
end

## Print see alsos for sym
function format_see_alsos(sym)
    ! haskey(SJSEEALSO,sym) && return
    sas = copy(SJSEEALSO[sym])
    length(sas) == 0 && return
    sort!(sas)
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

#### Examples

"""
    macro sjexamp(sym,strs...)

define examples for a symbol (representing a Head). This is used
in source files to define examples for a symbol (representing a Head).
Maybe this should be a function
"""
macro sjexamp(sym,strs...)
    if ! haskey(SJEXAMPLES,sym)
        SJEXAMPLES[sym] = Array{Any}(undef, 0)
    end
    exs = SJEXAMPLES[sym]
    ar = Array{Any}(undef, 0)
    for i in 1:length(strs)
        push!(ar,eval(strs[i]))
    end
    push!(exs,ar)
end


"""
    format_sjexamples(sym)

Print Symata examples associated with a symbol.
"""
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


"""
    format_sjexample(lines)

Format just one example for printing with docs. Do not evaluate.
Each item in example is either an explanatory string, or a 2-tuple of input and output.
"""
function format_sjexample(lines)
    local ins, outs, expl
    for i in 1:length(lines)
        ln = lines[i]
        if isa(ln,AbstractString)
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
            println("symata> ", ins)
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
        if isa(ln,AbstractString)
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
            println("symata> ", ins)
            ex = parse(ins)
            res = (eval(Expr(:macrocall,Symbol("@sym"), ex)))
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
            println()
            println("--------------------------")
            println()
        end
    end
end

## Information on hierarchy of fields:

# julia> Base.active_repl.interface.modes[2]
# "Prompt(\"symata> \",...)"

# julia> typeof(Base.active_repl.interface.modes[2])
# Base.LineEdit.Prompt

# julia> typeof(Base.active_repl.interface.modes[2].hist)
# Base.REPL.REPLHistoryProvider

# Push a string onto history in symata (symjulia) mode
# modified from add_history in REPL.jl
function sj_add_history(s::AbstractString)
    sj_add_history(symata_repl_history(), s)
end

function sj_add_history(hist::REPL.REPLHistoryProvider, s::AbstractString)
    str = rstrip(Compat.String(s))
    isempty(strip(str)) && return
    mode = :symata
    length(hist.history) > 0 &&  # we could be more clever and not push the same example sequence twice
    mode == hist.modes[end] && str == hist.history[end] && return
    push!(hist.modes, mode)
    push!(hist.history, str)
    hist.history_file == nothing && return
    entry = """
    # time: $(Libc.strftime("%Y-%m-%d %H:%M:%S %Z", time()))
    # mode: $mode
    $(Base.replace(str, r"^"ms => "\t"))
    """
    # TODO: write-lock history file
    seekend(hist.history_file)
    print(hist.history_file, entry)
    flush(hist.history_file)
end

### Help

@sjdoc Help """
    Help(sym), Help(\"sym\"), or ? topic 

print documentation for the symbol `sym`. For example `Help(Expand)`.
Due to parsing restrictions at the repl, for some topics, the input must be a string.

    h"topic" gives a case-insensitive regular expression search.

In the REPL, or Jupyter notebook, hit TAB to see all the available completions.

    Help(All => True)

print all of the documentation.

    Help(regex)

prints a list of topics whose documentation text matches the
regular expression regex. For example `Help(r\"Set\"i)` lists all topics that
match "Set" case-independently.
"""

@mkapprule Help  nodefault => true

@doap Help() = print_doc("Help")

function do_Help(mx::Mxpr{:Help}, r::Mxpr{:Rule})
    if r[1] == :All && r[2] == true
        print_all_docs()
    end
    Null
end

do_Help(mx::Mxpr{:Help},args...) =  print_doc(args...)

function do_Help(mx::Mxpr{:Help},r::T) where T<:Regex
    print_matching_topics(r)
end

### Example

@sjdoc Example """
    Example(s)

run (evaluates) all examples for the symbol `s`, typically a function or variable.
Input, output, and comments are displayed. Input strings
for the example are pushed onto the terminal history so they can be retrieved and
edited and re-evaluated.

    Example(s,n)

run the `n`th example for symbol `s`. When viewing documentation strings via `?`
the examples are printed along with the documentation string, but are not evaluated.

    Example()

Returns a list of all example topics.
"""

@mkapprule Example  :nargs => 0:2

@doap Example() = mxprcf(:List,Any[sort(collect(keys(SJEXAMPLES)))...])
@doap Example(topic) = do_examples(mx[1])  ## <-- mx[1] is topic ?
@doap Example(topic, n::Int) = do_example_n(mx[1],n)
