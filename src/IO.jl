### This is Symata level IO. The code for formatting and printing Mxpr, etc. is in output.jl

function Symata_module_path()
    joinpath(dirname(@__FILE__), "..")
end

#### Println

@sjdoc Println "
Println(expr1,expr2,...) prints the expressions and a newline.
"
apprules(mx::Mxpr{:Println}) = (symprintln(margs(mx)...) ; Null)


#### Print

@sjdoc Print "
Print(expr1,expr2,...) prints the expressions.
"
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
                Symata.exfunc(expr)
            catch e
                symprintln("Reading file: got error ", e)
            end
    end
    sjretval
end


function Symata_eval_file(fname)
    fname |> readstring |> Symata_eval_string
end


# TODO. Read more than one expression from a line.
# TODO. Use a new exception type
# Read and evaluate Symata expressions from a file.
# Return the last returned value.
function read_Symata_file(f::AbstractString, test::Symata_Test = Symata_NullTest() )
    oldval = set_issjinteractive(false)
    eline = ""
    local sjretval
    reading_test::Bool = false
    incomplete_flag::Bool = false
    incomplete_message = ""
    local line_number
    for (line_number,line) = enumerate(eachline(f))
        pline = sjpreprocess_string(line)
        if typeof(test) != Symata_NullTest && length(pline) > 1 && pline[1:2] == "T "
            reading_test = true
            if length(pline) > 2
                eline = eline * pline[3:end]
            end
        else
            eline = eline * pline
        end
        expr =
            try
                Base.syntax_deprecation_warnings(false) do
                    parse(eline)
                end
            catch
                set_issjinteractive(oldval)
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
                res = Symata.exfunc(expr)
                if typeof(test) != Symata_NullTest && reading_test
                    reading_test = false
                    record_SJTest(test, f, line_number, res)
                end
                res
            catch e
                set_issjinteractive(oldval)
                warn("SJ Error reading file $f,  line $line_number\n")
                rethrow(e)
            finally
                eline = ""
            end
    end
    set_issjinteractive(oldval)
    if incomplete_flag
        error(incomplete_message, " in file $f line $line_number")
    end
    sjretval
end


#### Get

@mkapprule Get

@sjdoc Get "
Get(\"filename\") reads and evaluates Symata expressions from file \"filename\".
Try putting empty lines between expressions if you get errors on reading.
"

do_Get{T<:AbstractString}(mx::Mxpr{:Get}, fname::T) =  read_Symata_file(fname)

#### ReadString

@mkapprule ReadString  :nargs => 1
@sjdoc ReadString  "
ReadString(\"filename\") reads \"filename\" and returns the contents as a string.
"
do_ReadString{T<:AbstractString}(mx::Mxpr{:ReadString}, fname::T) = readstring(fname)

# Write Symata expression as strings that can be used as input to define
# objects and properties.

# string to set attributes of sym
function attributes_set_string{T<:Union{AbstractString, SJSym}}(sym::T)
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

#### Definition

@mkapprule Definition

@sjdoc Definition "
Definition(sym) prints definitions associated with symbol sym.
"

function maybeprint(io::IO,s)
    if length(s)>0 symprintln(io,s) end
end

function do_Definition{T<:Union{AbstractString, SJSym}}(mx::Mxpr{:Definition}, sym::T)
    write_definition(STDOUT,sym)
end

function write_definition(io::IO, sym)
    maybeprint(io,getdefinition(Symbol(sym)))
    dvdefs = jlistdownvaluedefs(sym)
    if length(dvdefs) > 0
        for def in dvdefs
            symprintln(io,def)
        end
    end
    uvdefs = jlistupvaluedefs(sym)
    if length(uvdefs) > 0
        for def in uvdefs
            symprintln(io,def)
        end
    end
    maybeprint(io,attributes_set_string(sym))
    println(io)
end

#### Save

@mkapprule Save

@sjdoc Save "
Save(\"file\", obj) appends definitions associated with obj to \"file\".
Save(\"file\", list) appends definitions associated with the objects in list to \"file\".
Save uses Definition.
"

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

#### Open

# we should fold this behavior into mkapprule
@mkapprule Open  :nodefault => true

@sjdoc Open "
Open(args...) opens a file, runs a command, etc. The arguments are passed to julia `open'.
The julia methods that take array arguments are not implemented.
"

do_Open(mx::Mxpr{:Open}, args...) = open(args...)

#### Close

@mkapprule Close

@sjdoc Close "
Close(str) closes the IO stream str.
"

do_Close{T<:IO}(mx::Mxpr{:Close}, str::T) = (close(str); Null)


### STDOUT, STDERR.  These two symbols are set it client_symata.jl after the REPL is created.

@sjdoc STDOUT "
STDOUT is the standard output stream.
"

@sjdoc STDERR "
STDERR is the standard output stream.
"

@sjdoc DevNull "
DevNull is bound to Julia output stream DevNull.
"

@mkapprule Out

@sjdoc Out "
Out(n) returns the nth line of output. The value returned is evaluated in the current environment.

O returns the last output.

OO returns the second to last output.

OO..O k times gives the kth previous output, for k <= 10.

Clear(Out) clears all the saved output.
"

@sjseealso(Out, Clear)

do_Out(mx::Mxpr{:Out}, n::Integer) = doeval(get_output_by_line(n))
do_Out(mx::Mxpr{:Out}, x) = :Null

# We should probably explicitly return Null
# from functions. But,this might catch all
# of the nothings from causing printing
# ...No. this did not help
apprules(expr::Void) = :Null

##### TempName

@mkapprule TempName :nargs => 0

@sjdoc TempName "
TempName() generates a unique temporary file path.
"

@doap TempName() = tempname()


##### DeleteFile

@mkapprule DeleteFile :nargs => 1

@sjdoc DeleteFile "
DelteFile(file) deletes file
"

@doap DeleteFile(file::AbstractString) = rm(file)
