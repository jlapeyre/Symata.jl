### This is Symata level IO. The code for formatting and printing Mxpr, etc. is in output.jl

function Symata_module_path()
    joinpath(dirname(@__FILE__), "..")
end

### Println

@sjdoc Println """
    Println(expr1,expr2,...)

print the expressions and a newline.
"""

apprules(mx::Mxpr{:Println}) = (symprintln(margs(mx)...) ; Null)

### Print

@sjdoc Print """
    Print(expr1,expr2,...)

prints the expressions.
"""

apprules(mx::Mxpr{:Print}) = (symprint(margs(mx)...); Null)

# We are using read_Symata_file instead of this.
# We should probably delete this at some point.
# Read Symata expressions from a string and evaluate them, one by one.
function Symata_eval_string(s)
    s = sjpreprocess_string(s)
    i = 1
    local sjretval
    while !done(s,i)
        Base.syntax_deprecation_warnings(false) do
            expr, i = parse(s,i)
        end
        sjretval =
            try
                Symata.symataevaluate(expr)
            catch e
                symprintln("Reading file: got error ", e)
            end
    end
    sjretval
end


Symata_eval_file(fname) = fname |> readstring |> Symata_eval_string

# TODO. Read more than one expression from a line.
# TODO. Use a new exception type
# TODO. use EvaluateJuliaSyntaxSimple() I think it is simpler than the try catch stuff below

"""
    read_Symata_file(f::AbstractString, test::Symata_Test = Symata_NullTest() )

Read and evaluate Symata expressions from file `f`.
Return the last returned value. If `test` is given, interpret the contents
of the file as Symata tests.
"""
function read_Symata_file(f::AbstractString, test::Symata_Test = Symata_NullTest() )
    eline = ""
    local sjretval
    reading_test::Bool = false
    incomplete_flag::Bool = false
    incomplete_message = ""
    local line_number
    for (line_number,line) = enumerate(eachline(f))
        pline = sjpreprocess_string(line)
        if typeof(test) != Symata_NullTest && length(pline) > 1 && pline[1:2] == "T "
            if isa(test, SymataPlainPrintTest) println(pline) end
            reading_test = true
            if length(pline) > 2
                eline = eline * pline[3:end]
            end
        else
            eline = eline * pline   # is it worth using takebuf here ?
        end
        expr =
            try
                Base.syntax_deprecation_warnings(false) do
                    parse(eline)
                end
            catch
                error("Syntax error in file $f, line $line_number: '", chomp(pline), "'")
            end
        if typeof(expr) ==  Expr && expr.head == :incomplete
            incomplete_flag = true
            incomplete_message = expr.args[1]
            continue
        end
        sjretval =
            try
                incomplete_flag = false
                res = Symata.symataevaluate(expr, EvaluateJuliaSyntaxSimple())
                if typeof(test) != Symata_NullTest && reading_test
                    reading_test = false
                    record_SJTest(test, f, line_number, res)
                end
                res
            catch e
                warn("SJ Error reading file $f,  line $line_number\n")
                rethrow(e)
            finally
                eline = ""
            end
    end
    if incomplete_flag
        error(incomplete_message, " in file $f line $line_number")
    end
    sjretval
end


### Get

@mkapprule Get

@sjdoc Get """
    Get(filename)

read and evaluate Symata expressions from file `filename`.

!!! note
    Try putting empty lines between expressions if you get errors on reading.
"""

do_Get{T<:AbstractString}(mx::Mxpr{:Get}, fname::T) =  read_Symata_file(fname)

### ReadString

@mkapprule ReadString  :nargs => 1

@sjdoc ReadString  """
    ReadString(filename)

reads `filename` and returns the contents as a `String`.
"""

do_ReadString{T<:AbstractString}(mx::Mxpr{:ReadString}, fname::T) = readstring(fname)

# Write Symata expression as strings that can be used as input to define
# objects and properties.

# string to set attributes of sym
function attributes_set_string(sym::SymString)
    a = get_attributes(sym)
    a = map(string,a)
    sym = string(sym)
    if length(a) == 1
        "SetAttributes(" * sym * ", " * a[1] * ")"
    elseif length(a) > 1
        "SetAttributes(" * sym * ", [" * join(a, ", ")  * "])"
    else
        ""
    end
end

### Definition

@mkapprule Definition

@sjdoc Definition """
    Definition(sym)

prints definitions associated with `Symbol` `sym`.
"""

function maybeprint(io::IO,s)
    if length(s)>0 symprintln(io,s)
        return true
    end
    false
end

## FIXME: The correct way is not this way. Instead  Definition(f) does not evaluate to anything.
## rather displaying the form Definition(f) prints the definitions
function do_Definition(mx::Mxpr{:Definition}, sym::SymString)
    write_definition(STDOUT,sym)
    Null
end

write_definition(sym) = write_definition(STDOUT,sym)

function write_definition(io::IO, sym)
    gotdef = write_definition_no_attributes(io,sym)
    gotdef = maybeprint(io,attributes_set_string(sym)) || gotdef
    if gotdef
        println(io)
    end
    return gotdef
end

write_definition_no_attributes(sym) = write_definition_no_attributes(STDOUT,sym)
function write_definition_no_attributes(io::IO, sym)
    gotdef = maybeprint(io,getdefinition(Symbol(sym)))
    dvdefs = jlistdownvaluedefs(sym)
    if length(dvdefs) > 0
        gotdef = true
        for def in dvdefs
            symprintln(io,def)
        end
    end
    uvdefs = jlistupvaluedefs(sym)
    if length(uvdefs) > 0
        gotdef = true
        for def in uvdefs
            symprintln(io,def)
        end
    end
#    gotdef = maybeprint(io,attributes_set_string(sym)) || gotdef
    if gotdef
        println(io)
    end
    return gotdef
end


### Save

@mkapprule Save

@sjdoc Save """
    Save(filename, obj)

append definitions associated with `obj` to file `filename`.

    Save(filename, list)

append definitions associated with the objects in `list` to `filename`.

`Save` writes defnitions given by `Definition`.
"""

do_Save{T<:AbstractString}(mx::Mxpr{:Save}, fname::T, sym::SJSym) = save_definition(fname, [sym])
do_Save{T<:AbstractString}(mx::Mxpr{:Save}, fname::T, syms::Mxpr{:List}) = save_definition(fname, margs(syms))

function save_definition(fname, syms)
    io = open(fname, "a")  # check stuff !
    for sym in syms
        write_definition(io,sym)
    end
    close(io)
end

@sjseealso_group(Get, Save, Definition)

### Open

# we should fold this behavior into mkapprule
@mkapprule Open  :nodefault => true

@sjdoc Open """
    Open(args...)

opens a file, runs a command, etc. The arguments are passed to Julia `open`.
The Julia methods that take array arguments are not implemented.
"""

do_Open(mx::Mxpr{:Open}, args...) = open(args...)

### Close

@mkapprule Close

@sjdoc Close """
    Close(str)

closes the `IO` stream `str`.
"""

do_Close{T<:IO}(mx::Mxpr{:Close}, str::T) = (close(str); Null)


### STDOUT, STDERR.  These two symbols are set it client_symata.jl after the REPL is created.

@sjdoc STDOUT """
    STDOUT

the standard output stream.
"""

@sjdoc STDERR """
    STDERR

the standard output stream.
"""

@sjdoc DevNull """
    DevNull

is bound to Julia output stream `DevNull`.
"""

@mkapprule Out

@sjdoc Out """
    Out(n)

return the `n`th line of output. The value returned is evaluated in the current environment.

`O`

return the last output.

`OO`

returns the second to last output.

`OO..O`

`k` times gives the `k`th previous output, for `k <= 10`.

`Clear(Out)` clears all the saved output.

!!! note
    `O`, `OO`, etc. do not work in `Jupyter`.
"""

@sjseealso(Out, Clear)

function do_Out(mx::Mxpr{:Out}, n::Integer)
    if isymata_mode()
        doeval(Main.IJulia.Out[n])
    else
        doeval(get_output_by_line(n))
    end
end

@doap Out() = doeval(get_output_by_line(-1))

do_Out(mx::Mxpr{:Out}, x) = :Null

### In

@mkapprule In :nargs => 1

@sjdoc In """
    In(n)

returns the `n`th input cell. `In` only works in `Jupyter/IPython`.
"""
@doap function In(n::Integer)
    if isymata_mode()
        doeval(eval(Expr(:macrocall,Symbol("@sym"), parse(Main.IJulia.In[n]))))
    else
        :Null
    end
end


# We should probably explicitly return Null
# from functions. But,this might catch all
# of the nothings from causing printing
# ...No. this did not help
apprules(expr::Void) = :Null

### TempName

@mkapprule TempName :nargs => 0

@sjdoc TempName """
    TempName()

generate a unique temporary file path.
"""
@doap TempName() = tempname()


### DeleteFile

@mkapprule DeleteFile :nargs => 1

@sjdoc DeleteFile """
    DelteFile(filename)

delete `filename`.
"""
@doap DeleteFile(file::AbstractString) = rm(file)

### Format

## Note: This is *not* Mma's format

@sjdoc Format """
    Format(n, options)

formats `n` using the Julia `format` function from `Formatting.jl`.
Optional arguments to `format` are given in Symata format.
For example `precision=3` becomes `Precision => 3`.

Options are `Precision`, `Commas`, `Stripzeros`, `Conversion`,
`Width`, `Zeropadding`,  `Alternative`, `Mixedfraction`, `Tryden`,
`Parens`,`Autoscale`, `Suffix`.
"""

@mkapprule Format :nodefault => true

Formatting.format(ws::SymataIO.AbstractWO; kws...) = Formatting.format(ws.x; kws...)

function do_Format(mx, args...)
    (nargs,kws) = separate_rules1(args...; keylowercase=true)
    Formatting.format(map(wrapout,nargs)...; kws...)
end

### FullForm

@sjdoc FullForm """
    FullForm(expr)

print the internal representation of `expr` and all sub-expressions as
`Head(arg1,arg2,...)`. Normal output may use infix notation instead. But, expressions
may always be entered in `FullForm`.
"""

@sjexamp( FullForm,
          ("Clear(a,b)",""),
          ("a+b","a+b"),
          ("FullForm(a+b)","Plus(a,b)"))

@sjexamp( FullForm,
          ("Sqrt(2)", "Power(2,1//2)", "Internally, square roots are represented as powers."),
          ("Sqrt(2)[2]", "1//2", "The second part of the expression is '1//2'"))

# FullForm is implemented in output.jl

### Echo

@mkapprule Echo :nargs => 1:3

@doap Echo(x) = (symprintln(x); x)
@doap Echo(x,label::String) = (symprintln(label," ", x); x)

@doap function Echo(x,label::String, f)
    res = doeval(mxpr(f,x))
    symprintln(label," ", res)
    x
end
    


### Julia

## Julia() should be in some other file... which one ?

@sjdoc Julia """
    Julia()

exit Symata mode and returns to Julia mode from within Jupyter.
Use `isymata()` from Julia to enter Symata mode again.
"""

@mkapprule Julia :nargs => 0

@doap function Julia()
    isymata_mode(false)
    set_jupyter_input_prompt_color("green")
end
