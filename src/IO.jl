### This is SJulia level IO. The code for printing Mxpr, etc. is in output.jl

#### Println

@sjdoc Println "
Println(expr1,expr2,...) prints the expressions and a newline.
"
apprules(mx::Mxpr{:Println}) = (println(margs(mx)...) ; Null)


#### Print

@sjdoc Print "
Print(expr1,expr2,...) prints the expressions.
"
apprules(mx::Mxpr{:Print}) = (print(margs(mx)...); Null)


# Read SJulia expressions from a string and evaluate them, one by one.
function SJulia_eval_string(s)
    i = 1
    local sjretval
    while !done(s,i)
        expr, i = parse(s,i)   # probably need to trap errors here as well. And count, in order to give up
        sjretval =
        try
            SJulia.exfunc(expr) # we should use the macro here, I think.
        catch e
            println("Reading file: got error ", e)
#            println(e)
        end
    end
    sjretval
end

function SJulia_eval_file(fname)
    fname |> readstring |> SJulia_eval_string
end

#### Get

@mkapprule Get

@sjdoc Get "
Get(\"filename\") reads and evaluates SJulia expressions from file \"filename\".
Try putting empty lines between expressions if you get errors on reading.
"

do_Get{T<:AbstractString}(mx::Mxpr{:Get}, fname::T) =  SJulia_eval_file(fname)

#### ReadString

@mkapprule ReadString  :nargs => 1
@sjdoc ReadString  "
ReadString(\"filename\") reads \"filename\" and returns the contents as a string.
"
do_ReadString{T<:AbstractString}(mx::Mxpr{:ReadString}, fname::T) = readstring(fname)

# Write SJulia expression as strings that can be used as input to define
# objects and properties.

# string to set attributes of sym
function attributes_set_string{T<:Union{AbstractString, Symbol}}(sym::T)
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
    if length(s)>0 println(io,s) end
end

function do_Definition{T<:Union{AbstractString, Symbol}}(mx::Mxpr{:Definition}, sym::T)
    write_definition(STDOUT,sym)
end

function write_definition(io::IO, sym)
    maybeprint(io,getdefinition(symbol(sym)))
    dvdefs = jlistdownvaluedefs(sym)
    if length(dvdefs) > 0
        for def in dvdefs
            println(io,def)
        end
    end
    uvdefs = jlistupvaluedefs(sym)
    if length(uvdefs) > 0
        for def in uvdefs
            println(io,def)
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


### STDOUT, STDERR.  These two symbols are set it client_sjulia.jl after the REPL is created.

@sjdoc STDOUT "
STDOUT is the standard output stream.
"

@sjdoc STDERR "
STDERR is the standard output stream.
"

@sjdoc DevNull "
DevNull is bound to julia stream DevNull.
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
