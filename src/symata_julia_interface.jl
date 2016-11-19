# These Symata functions (symbols) translate between Symata and Julia data and expressions

#### JVar

@sjdoc JVar """
    JVar(x)

return the Julia value of the Symbol that `x` evaluates to.

For example, if `a = 1` in Julia and `b = a` in Symata, then `JVar(b)` evaluates to `1`.
"""

@sjseealso_group(Jxpr,JVar)
apprules(mx::Mxpr{:JVar}) = eval(Main,symname(mx[1]))

#### SetJ

@sjdoc SetJ """
    SetJ(x,val)

sets the Julia symbol `x` to `val`.

Variables and functions in Symata are separate from those in Julia, ie, their table of bindings to symbols are separate.
"""

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
@mkapprule Jxpr

@doap Jxpr{T<:Union{Expr,Symbol}}(ex::T) = eval(ex)
@doap Jxpr(x) = symerror("Jxpr: Can't execute Julia code of type ", typeof(x))

#apprules(mx::Mxpr{:Jxpr}) = do_jxpr(mx,mx[1])
#do_jxpr{T<:Union{Expr,Symbol}}(mx::Mxpr{:Jxpr}, ex::T) = eval(ex)
#do_jxpr(mx::Mxpr{:Jxpr}, x) = symerror("Jxpr: Can't execute Julia code of type ", typeof(x))

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

function apprules(mx::Mxpr{:Unpack})
    obj = mx[1]
    mx = unpack_to_List(obj)
    setfixed(mx)
    setcanon(mx)
    return mx
end

unpack_to_List(obj) = mxpr(:List, do_unpack(obj))
do_unpack(obj) = copy!(newargs(length(obj)),obj)

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
    do_pack(T,a)
end

do_pack(T,sjobj) = copy!(Array(T,length(sjobj)), sjobj)

#### Translate Symata to Julia

# Wrap Expr to prevent Symata from evaluating it.

mxpr_to_expr(x) = x

# Don't really want to do this with everything !
function mxpr_to_expr(s::Symbol)
    Symbol(lowercase(string(s)))
end

# This is (no longer not( necessary for Times. Maybe for other things.
# Also, if we can remove unwanted methods for *, we will need this.
const MTOJSYM_COMPILE = Dict(
                             :Times => :mmul,
                             :Plus => :mplus,
                             :Power => :mpow,
                             :Abs => :mabs
                             )

function mtojsym_compile(s::Symbol)
    if haskey(MTOJSYM_COMPILE, s) return MTOJSYM_COMPILE[s] end
    mtojsym(s)
end

function mxpr_to_expr(mx::Mxpr)
    head = mtojsym_compile(mhead(mx))
    length(mx) == 0 && return :(  $(head)() )
    a = margs(mx)
    a1 = mxpr_to_expr(a[1])
    ex = :( $(head)($(a1)) )
    length(mx) == 1 && return ex
    for i in 2:length(a)
        push!(ex.args, mxpr_to_expr(a[i]))
    end
    ex
end

#### Compile

"""
    freesyms(ex::Expr)

return a list of unbound symbols at any depth in expression `ex`.
"""
function freesyms(x)
    syms = Dict()
    freesyms!(x,syms)
    sort(collect(keys(syms)))
end

function freesyms!(ex::Expr, syms)
    a = ( ex.head == :call ?  view(ex.args,2:length(ex.args)) : ex.args )
    foreach( x -> freesyms!(x,syms), a)
end

freesyms!(s::Symbol, syms) = (if ! isdefined(s) syms[s] = 1 end)
freesyms!(x,syms) = nothing

@mkapprule Compile

@sjdoc Compile """
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
@doap Compile(a::Mxpr{:List}, body ) = eval(Main, Expr(:function, Expr(:tuple, [mxpr_to_expr(x) for x in margs(a)]...) , mxpr_to_expr(body)))

@doap function Compile(body)
    jbody = mxpr_to_expr(body)
    eval(Main, Expr(:function, Expr(:tuple, freesyms(jbody)...), jbody))
end

####  ToJulia

type Jexpr
     ex::Expr
end

@mkapprule ToJuliaExpression
@doap ToJuliaExpression(x) = Jexpr(mxpr_to_expr(x))

## Better name for this ?
@mkapprule SymataCall
@doap SymataCall(ex::Mxpr, var::Symbol) = wrap_symata(doeval(ex),var)
@doap SymataCall(ex::Mxpr, vars...) = wrap_symata(doeval(ex),vars...)

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
function wrap_symata(var0,expr0::Mxpr)
    (var,expr) = localize_variable(var0,expr0)
    deepunsetfixed(expr)
    function (x)
        setsymval(var,x)
        deepunsetfixed(expr) # this seems wasteful. When called from NIntegrate, it is not needed. Don't know why.
        doeval(expr)
    end
end

function wrap_symata(expr0::Mxpr,vars0...)
    vars = Array(Symbol,length(vars0))
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
    sym = get_localized_symbol(var0)    
    var0 == expr0 && return (sym,sym)
    return (sym,expr0)
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
