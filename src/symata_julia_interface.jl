# These Symata functions (symbols) translate between Symata and Julia data and expressions

#### JVar

@sjdoc JVar """
    JVar(x)

return the Julia value of the Symbol that `x` evaluates to.

For example, if `a = 1` in Julia and `b = a` in Symata, then `JVar(b)` evaluates to `1`.
"""

@sjseealso_group(Jxpr, JVar)
apprules(mx::Mxpr{:JVar}) = Core.eval(Main,symname(mx[1]))

### SetJ

@mkapprule SetJ nargs => 2  """
    SetJ(x,val)

sets the Julia symbol `x` to `val`.

Variables and functions in Symata are separate from those in Julia, ie, their table of bindings to symbols are separate.
"""

exportj(lhs::SymString) = exportj(lhs,doeval(lhs))
## FIXME: find out what we want on the next line and do it properly.
exportj(lhs::SymString, rhs) = Main.eval(Expr(:(=),symname(Symbol(lhs)),rhs))
exportj(lhs::SymString, rhs::Symbol) = exportj(lhs,QuoteNode(rhs))
@doap SetJ(lhs::SymString,rhs) = exportj(lhs,rhs)

### ExportJ

@mkapprule ExportJ """
    ExportJ(x,y,...)

sets the Julia symbols `x`,`y`, etc. to the Symata-value of `x`,`y`, etc.
`ExportJ(x)` is equivalent to `SetJ(x,x)`.
"""

@doap ExportJ(vars::SymString...) = foreach(exportj, vars)

### Jxpr

@sjdoc Jxpr """
    Jxpr

allows embedding Julia expressions. A Jxpr is entered like this `J( expr )`.

`expr` is interpreted as a Julia expression and it is wrapped expression with head `Jxpr`, which is then evaluated when
`Jxpr` is evaluated. You never see the head `Jxpr`. For example,
 `m = J( collect(1:10) )`  creates a Julia array and binds it to the Symata symbol `m`.
"""

@sjexamp( Jxpr,
         "This creates a Julia Array{Int,1} and \"binds\" it to the Symata symbol m.",
         ("m = J( collect(1:3) )",
          "3-element Array{Int64,1}:\n 1\n 2\n 3"))

@sjexamp( Jxpr,
         "Call a Julia function",
         ("tf = J( time )",""),
         ("tf()","1.424287593897437e9"))

# quote, i.e. :( expr ) is parsed as a Julia expression and is wrapped as  < -- we will remove this
# Mxpr with head Jxpr. It is evaluated here.
@mkapprule Jxpr :nodefault => true

@doap function Jxpr(args...)
    local res
    for x in args
        res = eval(x)
    end
    res
end

@sjdoc J """
    J(expr1,expr2,...)

Evaluate Julia expressions `expr1`, `expr2`, ... in Julia and return the final result.
"""

@sjseealso_group(J, Jexpr, Compile, SymataCall)

# @doap Jxpr{T<:Union{Expr,Symbol,Number}}(ex::T) = eval(ex)
# @doap Jxpr(x) = symerror("Jxpr: Can't execute Julia code of type ", typeof(x))

#### Unpack

@sjdoc Unpack """
    Unpack(a)

unpack a Julia typed `AbstractArray` into an Symata `List` expression.

Only `1`-d is supported. If `a` is a Julia `Dict`, then a `List` of `Lists` of
key,value pairs is returned.
"""

@sjexamp( Unpack,
         "This creates a List of three random Float64's.",
         ("Unpack( J(rand(3)) )", "[0.5548766917324894,0.034964001133465095,0.9122052258982192]"))


@mkapprule Unpack :nargs => 1

@doap Unpack(a::AbstractArray) = unpacktoList(a)

"""
    unpacktoList(obj::AbstractArray{T,1})

Converts a Julia array to a Symata list of type `ListT`. Only one-dimensional
arrays are supported.
"""
function unpacktoList(obj)
    mx = mxpr(:List, do_unpack(obj))
    setfixed(mx)
    setcanon(mx)
    return mx
end
do_unpack(obj) = copyto!(newargs(length(obj)),obj)

## We copy this and change it a bit to tolistoflists in mxpr_utils
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

@sjdoc Pack """
    Pack(expr)

pack the arguments of the Symata expression `expr` into a Julia array,
whose element type is the minimum required to holds the arguments of `expr`.
"""

@sjexamp( Pack,
         "This returns a Julia array of element type Int [1,2,3].",
         ("ClearAll(f)",""),
         ("Pack(f(1,2,3))","3-element Array{Int64,1}: [1,2,3]"))

@sjseealso_group(Pack,Unpack)

"""
    typejoin_array(a::Array)

return the typejoin of all elements in `a`.
I saw this in Base somewhere. Probably more efficient there.
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
    do_pack(T,a)
end

do_pack(T,sjobj) = copyto!(Array{T}(undef, length(sjobj)), sjobj)

#### Translate Symata to Julia

##
# Wrap Expr to prevent Symata from evaluating it.

abstract type AbstractMtoE end

mutable struct MtoECompile <: AbstractMtoE
end

mutable struct MtoEPlain <: AbstractMtoE
end

mxpr_to_expr(x, aux::AbstractMtoE) = x

# Don't really want to do this with everything !
function mxpr_to_expr(s::Symbol, aux::AbstractMtoE)
    s
#    Symbol(lowercase(string(s)))
end

function mxpr_to_expr(s::Symbol,aux::MtoEPlain)
    Symbol(lowercase(string(s)))  # only lowercase some
end


const MTOJSYM_COMPILE = Dict(
    :Times => :mmul,
    :Plus => :mplus,
    :Power => :mpow,
    :Abs => :mabs,
)

mtojsym_compile(s::Symbol) =  get(MTOJSYM_COMPILE, s, mtojsym(s))

const MTOJSYM_PLAIN = Dict(
                             :Times => :(*),
                             :Plus => :(+),
                             :Power => :(^),
                             :Abs => :abs,
                             :E => :e
                             )

function mtojsym_plain(s::Symbol)
    Symbol(lowercase(string(get(MTOJSYM_PLAIN, s, mtojsym(s)))))
end

function mxpr_to_expr(mx::Mxpr,aux::MtoECompile)
    head = mtojsym_compile(mhead(mx))
    mxpr_to_expr_body(head,mx,aux)
end

# convert List to Julia array
function mxpr_to_expr(mx::Mxpr{:List},aux::MtoECompile)
    Expr(:vect, [mxpr_to_expr(x,aux) for x in margs(mx)]...)
end

function mxpr_to_expr(mx::Mxpr,aux::MtoEPlain)
    head = mtojsym_plain(mhead(mx))
    mxpr_to_expr_body(head,mx,aux)
end

function mxpr_to_expr_body(head,mx::Mxpr,aux::AbstractMtoE)
    isempty(mx) && return :( $(head)() )
    a = margs(mx)
    a1 = mxpr_to_expr(a[1],aux)
    ex = :( $(head)($(a1)) )
    length(mx) == 1 && return ex
    for i in 2:length(a)
        push!(ex.args, mxpr_to_expr(a[i],aux))
    end
    ex
end

#### Compile


@mkapprule Compile """
    f = Compile(expr)

convert `expr` to a compiled function.

    f = Compile( [x,y,...], expr)

create an anonymous Julia function `(x,y,...) -> expr` from Symata expression `expr` and bind
the result to `f`.

```
symata> f = Compile(x^2 + y^2)
symata> f(3,4)
        25
```

Compile works by creating an anonymous Julia function from the Symata expression `expr`. This anonymous
function is just-in-time compiled for the arguments that it is called with.
If no arguments are specified, then the function signature is the sorted list of free symbols found in `expr`. Free symbols are all symbols
that are not bound in the Julia. More precisely, in the Symata module scope. This
excludes `e`, for instance. Alternatively, the arguments may be specified, for example `Compile( [x,y], expr)`.

By default `expr` is not evaluated. This works for literal expressions, such as `x^2`. If the expression must be evaluated, wrap it
in `Evaluate`. For example

```
symata 1> expr = x^2 + y^3
Out(1) = x^2 + y^3

symata 2> f = Compile(Evaluate(expr))
Out(2) = (::#1) (generic function with 1 method)

symata 3> f(2,3)
Out(3) = 31
```
"""
@doap function Compile(a::Mxpr{:List}, body )
    aux = MtoECompile()
    Core.eval(Main, Expr(:function, Expr(:tuple, [mxpr_to_expr(x,aux) for x in margs(a)]...) , mxpr_to_expr(body,aux)))
end

# This is not reliable. It is not worth the complexity of fixing it, I think.
# We require an explicit list of variables, instead.
# We could look at Attributes, Protected, etc. and just use plain variables.
# @doap function Compile(body)
#     aux = MtoECompile()
#     jbody = mxpr_to_expr(body,aux)
#     Core.eval(Main, Expr(:function, Expr(:tuple, freesyms(jbody)...), jbody))
# end

## NOTE: freesyms is unrelated to getfreesyms and setfreesyms
"""
    freesyms(ex::Expr)

return a list of Symata-unbound symbols at any depth in expression `ex`, by
walking through the expression. This may not be useful anymore.
"""
function freesyms(x)
    syms = Dict()
    freesyms!(x, syms)
    sort(collect(keys(syms)))
end

function freesyms!(ex::Expr, syms::Dict)
    a = ( ex.head == :call ?  view(ex.args,2:length(ex.args)) : ex.args )
    foreach(x -> freesyms!(x,syms), a)
end

# We have been using isdefined for a long time,
# but this is incorrect. We want isbound, which
# checks for Symata binding.
# Neither is correct. For instance, E is not Symata bound.
# E is bound in Julia Main. (we did it). So, the fact
# that this was worked for several examples, is fortuitous.
function freesyms!(s::Symbol, syms)
#    if ! isdefined(Symata,s)
    if ! isbound(s)
        syms[s] = 1
    end
end

freesyms!(x, syms) = nothing

####  ToJuliaExpression

# type Jexpr
#      ex::Expr
# end

@mkapprule ToJuliaExpression

@doap function ToJuliaExpression(x)
#    Jexpr(mxpr_to_expr(x, MtoECompile()))
#    mxpr(:JuliaExpression, mxpr_to_expr(x, MtoECompile()))
    mxpr(:JuliaExpression, mxpr_to_expr(x, MtoEPlain()))
end

"""
    @symExpr(expr)

Symata-evaluates `expr`, translates the result to a Julia expression, and inserts it into the surrounding code. This works
just like any Julia macro for inserting code, except that the code is generated by evaluating a Symata expression.

The following evaluates Symata code, translates the result to Julia and uses it as the body of a Julia function.
```
julia> f1(z) = @symExpr Together(PolyLog(-1,z) + (1-z))
```
"""
macro symExpr(expr)
    mx = symataevaluate(expr, EvaluateJuliaSyntaxSimple())
    mx = doeval(mx)
    local ex
    if isa(mx,Mxpr{:JuliaExpression})
       ex = mx[1]
    else
       ex = mxpr_to_expr(mx, MtoECompile())
    end
    :(($(esc(ex))))  ## We need to escape this so that the code is evaluated in the correct module
end

### ToJuliaString

@sjdoc ToJuliaString """
    ToJuliaString(expr)

translate `expr` to Julia, returning a string.
If the option `NoSymata => False` is given, then translated code requires `using Symata`.
Currently, only arithemetic, trigonometric functions, and a few other things are translated.
"""
@mkapprule ToJuliaString :nodefault => true  :options => Dict( :NoSymata => true )

protect(:NoSymata)

function do_ToJuliaString(mx, x; NoSymata=true)
    aux = NoSymata ? MtoEPlain() : MtoECompile()
    string(mxpr_to_expr(x, aux))
end

@mkapprule SymataCall  """
    SymataCall(var, body)
    SymataCall(varlist, body)

return a Julia function that substitues `var` or variable in `varlist` into `body` and
submits the result to the Symata evaluation sequence.

The variables are localized before writing the Julia function.
"""
@doap SymataCall(var::Symbol, ex::Mxpr) = wrap_symata(doeval(ex),var)
@doap SymataCall(vars::Mxpr{:List}, ex::Mxpr) = wrap_symata(doeval(ex),vars...)

"""
    f = wrap_symata(expr::Mxpr,var)

return a Julia function that Symata-evaluates an expression when called.

The function corresponds to `f(var) = expr`.
This wraps `expr` in a single-argument Julia function.

First creates a lexical scope, that is a new gensym symbol `nvar` is generated and is substituted
everywhere in `expr` for `var`. Calling `f(x)`  Symata-binds `x` to `nvar` and `expr` is
Symata-evaluated and the result is returned.

`wrap_symata` is used by `NIntegrate` and `Plot`.
"""
function wrap_symata(var0,expr0::Mxpr)   # Why are arguments reversed here ? Is this ever called ?
    (var,expr) = localize_variable(var0,expr0)
#    deepunsetfixed(expr)
    function (x)
        setsymval(var,x)
        # deepunsetfixed(expr)
        # this seems wasteful. When called from NIntegrate, it is not needed. Don't know why.
        # because we needed HoldAll
        doeval(expr)
    end
end

function wrap_symata(expr0::Mxpr,vars0...)
    vars = Array{Symbol}(undef, length(vars0))
    expr = expr0
    for i in 1:length(vars0)
        (vars[i],expr) = localize_variable(vars0[i],expr)
    end
    deepunsetfixed(expr)
    function (args...)
        for i in 1:length(args)
            setsymval(vars[i],args[i])
        end
        deepunsetfixed(expr)
        doeval(expr)
    end
end

function wrap_symata(expr0::SJSym,var0)
    var0 == expr0 && return (x) -> x
    sym = get_localized_symbol(var0)
    return (sym,expr0)   # this does not appear to be useful
end


#### CodeNative

# This does not yet work. Symata evaluates the symbol `f` to
# the function object, so we lose its name. We probably need
# a robust way to associate the symbol with the function.
# ... Julia numbers anonymous functions and names others.
# we can get this information somehow
# ok, it is string(func::Function). Then we can keep a
# hash table of Symata symbols bound to functions.
# ... a PITA.
# Better to define a Julia type symfunction or something
# that stores the function and the name.
# we can make it callable and it passes the call to its function
# can also do this at the Symata level.

# FIXME. implement functions according to the description above.

# @mkapprule CodeNative
# @doap function CodeNative(f::Symbol, arg)
#     thetypes = (margs(args)...)
#     code_native(eval(f), thetypes)
# end
