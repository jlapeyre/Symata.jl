# These Symata functions (symbols) translate between Symata and Julia data and expressions

#### JVar

@sjdoc JVar "
JVar(x) returns the Julia value of the Symbol that x evaluates to. For example,
if a = 1 in Julia and b = a in Symata, then JVar(b) evaluates to 1.
"

@sjseealso_group(Jxpr,JVar)
apprules(mx::Mxpr{:JVar}) = eval(Main,symname(mx[1]))

#### SetJ

@sjdoc SetJ "
SetJ(x,val) sets the Julia symbol x to val. Variables and functions in Symata
are separate from those in Julia, ie, their table of bindings to symbols are separate.
"

# This can also be done with a Jxpr
# Bind a Julia symbol to the rhs
function apprules(mx::Mxpr{:SetJ})
    lhs = mx[1]
    rhs = mx[2]
    if typeof(rhs) == Symbol
        rhs = QuoteNode(rhs)
    end
    Main.eval(Expr(:(=),symname(lhs),rhs))
end

#### Jxpr

@sjdoc Jxpr "
Jxpr allows embedding Julia expressions.
A Jxpr is entered like this :( expr ) . expr is interpreted as a Julia expression and
it is wrapped expression with head Jxpr, which is then evaluated when
Jxpr is evaluated. You never see the head Jxpr. For example
 m = :( [1:10] )  creates a Julia array and binds it to the Symata symbol m
"

@sjexamp( Jxpr,
         "This creates a Julia Array{Int,1} and \"binds\" it to the Symata symbol m.",
         ("m = :( collect(1:3) )",
          "3-element Array{Int64,1}:\n 1\n 2\n 3"))

@sjexamp( Jxpr,
         "Call a Julia function",
         ("tf = :( time )",""),
         ("tf()","1.424287593897437e9"))

# quote, i.e. :( expr ) is parsed as a Julia expression and is wrapped as
# Mxpr with head Jxpr. It is evaluated here.
# Eg.  m = :( [1:10] )  creates a Julia array and assigns to Symata symbol m
function apprules(mx::Mxpr{:Jxpr})
    do_jxpr(mx,mx[1])
end

function do_jxpr{T<:Union{Expr,Symbol}}(mx::Mxpr{:Jxpr}, ex::T)
    return eval(ex)
end

function do_jxpr(mx::Mxpr{:Jxpr}, x)
    symerror("Jxpr: Can't execute Julia code of type ", typeof(x))
end

#### Unpack

@sjdoc Unpack "
Unpack(a) unpacks a Julia typed array into an Symata List expression.
Only 1-d is supported. If a is a Julia Dict, then a list of lists of
key,value pairs is returned.
"

@sjexamp( Unpack,
         "This creates a List of three random Float64's.",
         ("Unpack( :(rand(3)) )", "[0.5548766917324894,0.034964001133465095,0.9122052258982192]"))

function apprules(mx::Mxpr{:Unpack})
    obj = mx[1]
    mx = unpack_to_List(obj)
    setfixed(mx)
    setcanon(mx)
    return mx
end


function unpack_to_List(obj)
    args = do_unpack(obj)
    return mxpr(:List,args)
end

function do_unpack(obj)
    args = newargs(length(obj))
    @inbounds for i in 1:length(obj)
        args[i] = obj[i]
    end
    return args
end


function do_unpack(dict::Dict)
    args = newargs(length(dict))
    i = 0
    for (k,v) in dict
        i += 1
        args[i] = mxpr(:List,k,v)
    end
    return args
end

#### Pack

@sjdoc Pack "
Pack(expr) packs the arguments of the Symata expression expr into a Julia array,
whose element type is the minimum required to holds the arguments of expr.
"

@sjexamp( Pack,
         "This returns a Julia array of element type Int [1,2,3].",
         ("ClearAll(f)",""),
         ("Pack(f(1,2,3))","3-element Array{Int64,1}: [1,2,3]"))

@sjseealso_group(Pack,Unpack)

"""
    typejoin_array(a::Array)

return the typejoin of all elements in `a`.
I saw this in Base somewher. Probably more efficient there.
"""
function typejoin_array(a)
    length(a) == 0 && return Any
    T = typeof(a[1])
@inbounds  for i in 2:length(a)
        T = typejoin(T, typeof(a[i]))
    end
    T
end

# 1-d unpack
function apprules(mx::Mxpr{:Pack})
    a = margs(margs(mx)[1])
    T = typejoin_array(a)
    args = do_pack(T,a)
    return args
end

function do_pack(T,sjobj)
    args = Array(T,length(sjobj))
    @inbounds for i in 1:length(sjobj)
        args[i] = sjobj[i]
    end
    return args
end

#### Translate Symata to Julia
# This stuff is partly implemented, but some of it works pretty well

# Wrap Expr to prevent Symata from evaluating it.

mxpr_to_expr(x) = x

function mxpr_to_expr(s::Symbol)
    Symbol(lowercase(string(s)))
end

function mxpr_to_expr(mx::Mxpr)
    h = mtojsym(mhead(mx))
    head = Symbol(lowercase(string(h)))
    if length(mx) == 0
        return :(  $(head)() )
    end
    a = margs(mx)
    a1 = mxpr_to_expr(a[1])
    ex = :( $(head)($(a1)) )
    if length(mx) == 1
        return ex
    end
    for i in 2:length(a)
        push!(ex.args, mxpr_to_expr(a[i]))
    end
    ex
end

#### Compile

function freesyms(x)
    syms = Dict()
    freesyms(x,syms)
    sort(collect(keys(syms)))
end

function freesyms(ex::Expr, syms)
    a = ex.head == :call ?  @view((ex.args)[2:end]) : ex.args
    foreach( x -> freesyms(x,syms), a)
end

freesyms(s::Symbol, syms) = (if ! isdefined(s) syms[s] = 1 end)

freesyms(x,syms) = nothing

@mkapprule Compile

@sjdoc Compile "
f = Compile(expr)

convert `expr` to a compiled function.

f = Compile( [x,y,...], expr) 

create an anonymous Julia function `(x,y,...) -> expr` from Symata expression `expr` and binds
the result to f. The translation of `expr` to Julia is crude, but works in many cases.

symata> f = Compile(x^2 + y^2)
symata> f(3,4)
        25

Since compile is under development, it simply lowercases symbols. So the Symata function ArcSin, will not work. You have
to use a Julia symbol `asin` or `ASin`. For instance,

 ReplaceAll(expr,  ArcSin => asin )

Symbols that do *not* need to be translated this was include `Cos`, `E`, `Exp`, `Pi`, `EulerGamma`.

Compile works by creating an anonymous Julia function from the Symata expression `expr`. The function
signature is the sorted list of free symbols found in `expr`. free symbols are all symbols found excluding
function names in the translated expression and symbols that are bound in the Symata module scope. This
excludes `e` for instance.
"

@doap Compile(a::Mxpr{:List}, body ) = eval(Expr(:function, Expr(:tuple, [mxpr_to_expr(x) for x in margs(a)]...) , mxpr_to_expr(body)))

@doap function Compile(body)
    jbody = mxpr_to_expr(body)
    eval(Expr(:function, Expr(:tuple, freesyms(jbody)...), jbody))
end

####  ToJulia

type Jexpr
    ex::Expr
end

@mkapprule ToJulia

@doap ToJulia(x) = Jexpr(mxpr_to_expr(x))
