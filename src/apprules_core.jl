# mkapprule defines a builtin "function" corresponding to a Head. Eg, Cos, While, Table, ...
# @mkapprule Headname  actually defines builtin evaluation rules for expressions with head Headname
#
# You can supply information about the number of args etc. by passing a dict to @mkapprules like this:
#   @mkapprules Headname  :key1 => val1  :key2 => val2 ....
#
# @mkapprules writes a default rule that returns the input expression.
# Eg. if you don't write a function (described below) to handle an expression with head Headname(a,b,c)
# Headname(a,b,c) is returned
#
# nargs => n              The expression requires exactly n arguments.
# nargs =>  ur::UnitRange  The number of arguments must lie within the range.
# nargs => n:Inf    n or more arguments
# options => Dict( :opt1 => default1, ... )  The expression takes keyword arguments, "Options". These are translated
#                                            to rules, which is how we implement keyword argumentss.
# nodefault => true    Don't write the default rule for any number of arguments of any type.
# A string is assumed to be the Symata docstring and is passed to @sjdoc.
# 

## TODO use this syntax :options = (:opt1 => default1, ...)
## TODO: assert type or ranges for keyword arguments (and positional arguments)

# """
#     OrMore(start)

# Unit range representing `start` and no upper bound.
# """
# immutable OrMore{T<:Real} <: AbstractUnitRange
#     start::T
# end

function parse_nargs(ex)
    if isa(ex,Expr) && ex.head == :(:) && ex.args[2] == :Inf
        return UnitRangeInf(ex.args[1])
    end
    return eval(ex)
end

## v0.6.0 bug.
## FIXME: Some things are being sent here to evaluate, eg 1:Inf
## that should not be evaluated. 1:Inf should be parsed above, not evaluated
eval_app_directive(ex) = eval(x)

function eval_app_directive(ex::Expr)
    if ex.head == :( => ) &&
        isa(ex.args[1],Symbol)
        return ex.args[1] => eval(ex.args[2])
    end
    eval(ex)
end


## FIXME: broken in an update to v0.6.0-rc2
## Now only one of he following kindof works:  @mkapprule nargs => 1:Inf, or  @mkapprule :nargs => 1:Inf,
## Depending on what we do here.
## There are several possible sources of errors
## !!!!!!!!!!!!   The AST for  :( a => b ) differs between v0.5 and v0.6,
## This is the main source of bugs here.
"""
 get_arg_dict(args)
constructs a Dict from the macro aguments.
"""
function get_arg_dict(args)
    d = Dict{Symbol,Any}()
    local pe
    for p in args
        if iscall(p, :(=>), 3)  # Convert v0.6 Pair to v0.5 Pair.
            p = Expr(:(=>), p.args[2], p.args[3])
        end
        # println("  ***** get_arg_dict: doing p = $p")
        # println("  Length is ", length(p.args))
        # println("  isaExpr: ", isa(p,Expr), ", type is ", typeof(p.args[1]), ", val is ", p.args[1])
#        if isa(p,Expr) && p.args[1] == :(=>) && p.args[2] == :nargs
        if isa(p,Expr) && p.args[1] == :nargs            
#            println("***** get_arg_dict: p is Expr and nargs")
            pres = parse_nargs(p.args[2])
            pe = :nargs => pres
        else
#            println("***** get_arg_dict: p is NOT Expr and nargs")
            pe = eval_app_directive(p)
        end
        d[pe[1]] = pe[2]
    end
    d
end

function get_arg_dict_options(args)
    d = Dict{Symbol,Any}()
    for p in args
        pe = eval(p)
        d[pe[1]] = pe[2]
    end
    d
end

# FIXME: we are relying on a bug that has been fixed
#  Change in macro hygiene on 0.6? #19587
#  Also, https://discourse.julialang.org/t/macro-problem-with-v0-6/1581

macro mkapprule(inargs...)
    head = inargs[1]   ## head is the symbol for which we are writing rules    
    args = [inargs...]
    n = length(args)
    headstr = string(head)
    fns = Symbol("do_" * headstr)
    mxarg = parse("mx::Mxpr{:" * headstr * "}") # something easier worked too, but I did not realize it
    local apprulecall
    apprulecall = :(apprules($mxarg) = $fns(mx, margs(mx)...))
    defmx = :( mx )
    specs = Dict()
    local rargs
    if n > 1  ## there are some directives
        docstringind = findfirst(x -> isa(x,String), args)
        if docstringind > 0
            sjdocfun(head,args[docstringind])
            deleteat!(args,docstringind)
        end
        n = length(args)
    end
    if n > 1
        rargs = args[2:end]
    else
        rargs = Any[]
    end
#    println("********  Getting args $rargs")
    specs = get_arg_dict(rargs)
#    println("********  Success Got specs")
    if haskey(specs, :options)
        optd = get_arg_dict_options(specs[:options])
        if haskey(specs, :nargs)
            nargcode = checkargscode(:newargs,head,specs[:nargs])
        else
            nargcode = :(nothing)
        end
        apprulecall = :(function apprules($mxarg)
                        kws = $optd
                        newargs = separate_known_rules(mx,kws)
                        $nargcode
                        if length(kws) == 0
                        $fns(mx,margs(mx)...)
                        else
                        $fns(mx,newargs...; kws...)
                        end
                        end )
    elseif haskey(specs, :nargs)
        nargcode = checkargscode(:mx,head,specs[:nargs])
        if isa(specs[:nargs],Int) || isa(specs[:nargs],AbstractUnitRange)
            nargcode = checkargscode(:mx,head,specs[:nargs])
            apprulecall = :(function apprules($mxarg)
                            $nargcode
                            $fns(mx,margs(mx)...)
                            end)
        end
    else
        nothing
    end
    if haskey(specs, :nodefault)
        defaultmethod = ""
    else
        defaultmethod = :($fns($mxarg, args...) = $defmx)
    end
    esc(quote
        set_sysattributes($headstr)
        $apprulecall
        $defaultmethod
        end)
end

# The macro mkapprule writes this function:  apprules(mx::Mxpr{:Headname}) = do_Headname(mx,margs(mx)...)
# We have to write functions do_Headname that handle various argument numbers and types via Julia's multiple dispatch.
# doap macro writes a do_Headname rule (a function)
# For example:
#    do_Headname(mx::Mxpr{:Headname},x,y)
# handles expressions Headname(x,y)
# To write this rule, you call doap like this
# @doap function Headname(x,y)
#                ...
#       end
# or
# @doap  function Headname(x,y) = ...
#
# or
# @doap function Headname{T<:SomeType}(x,y::T) = ...
# etc.

"""
    @doap  method definition for function Headname

Write a method defining a Symata rule for the symbol `Headname`.

You first write a macro call  `@mkapprule Headname`, which writes the function  `apprules(mx::Mxpr{:Headname}) = do_Headname(mx,margs(mx)...)`
You then write functions `do_Headname` that handle various argument numbers and types via Julia's multiple dispatch.

The macro @doap writes a method for the function `do_Headname`.


Examples of rules that handle Symata expressions of the form `Headname(x,y)` are

```
@doap function Headname(x,y)
               ...
end

@doap  function Headname(x,y) = ...

@doap function Headname{T<:SomeType}(x,y::T) = ...
```

These macros write methods that look like this: `do_Headname(mx::Mxpr{:Headname},x,y) ...` 
Note that this means you must not make a conflicting definition or use of `mx` in the body of @doap.
"""
macro doap(func)
    prototype = func.args[1].args                      # eg.  Headname{T,V}(x::T,y::V) or Headname(x,y::Int), etc.
    sj_func_name0 = prototype[1]                       #      Headname{T,V}, or Headname
    if isa(sj_func_name0, Expr)                        # got  Headname{T,V}
        if ( sj_func_name0.head == :curly )
            sj_func_name = sj_func_name0.args[1]       #   get Headname
            new_func_name = Symbol("do_",sj_func_name) #  Headname --> do_Headname
            sj_func_name0.args[1] = new_func_name      #  replace Headname{T,V} -> do_Headname{T,V}
        else
            symerror("Can't interpret ", sj_func_name)
        end
    else                                                       # got Headname(x,y)
        sj_func_name = sj_func_name0
        new_func_name = Symbol("do_",sj_func_name)             # do_Headname
        prototype[1] = new_func_name                           # replace Headname --> do_Headname
    end
    local quote_sj_func_name
    if isa(sj_func_name,Symbol)
        quote_sj_func_name = QuoteNode(sj_func_name)         # :Headname from prototype like Headname(args...)
    elseif isa(sj_func_name,Expr)
        quote_sj_func_name = QuoteNode(sj_func_name.args[1]) # :Headname from prototype like Headname{T,V}(args...)
    else
        symerror("doap: Can't interpret ", sj_func_name)
    end
    mxarg =  :( mx::Mxpr{$quote_sj_func_name}  )             # mx::Mxpr{:Headname}
    nind = 2 ## kws appear last in the argument list in Julia syntax. But, they are moved to the front in the AST
    if length(prototype) > 1   ## So, we have to detect this.
        if isa(prototype[2],Expr) && prototype[2].head == :parameters
            nind = 3
        end
    end
    insert!(prototype,nind, mxarg)                              # insert mx::Mxpr{:Headname} as the first argument in prototype
    :(($(esc(func))))                                        # return the rewritten function
end

#### Default apprule
##  This is for a head that is (almost always) not a "system" symbol, but rather user defined

apprules(x) = x

#### Currying

@doc """
For example

```
symata 1> op = Map(f)
Out(1) = Map(f)
symata 2> op([x,y,z])
Out(2) = [f(x),f(y),f(z)]
```

"Currying" was added to Mma in 2014
""" currying

apprules(mx::Mxpr{GenHead}) = do_GenHead(mx, mhead(mx))
do_GenHead(mx,h) = mx

# Head is a Julia function. Apply it to the arguments
do_GenHead(mx,f::Function) = f(margs(mx)...)


# Assume operator version of an Symata "function". Eg, Map
# Map(q)([1,2,3])
# But, not all functions use the first operator. Eg for MatchQ it is the second.

# function do_GenHead(mx,head::Mxpr)
#     mxpr(mhead(head),margs(head)...,copy(margs(mx))...)
# end

macro curry_first(fname)
    doname = Symbol("do_", string(fname))
    qname = QuoteNode(fname)
    esc(quote
        function ($(doname))(mx,arg1)
            mx
        end
        function do_GenHead(mx,head::Mxpr{$(qname)})
            mxpr(mhead(head),margs(head)...,copy(margs(mx))...)
        end
    end)
end

macro curry_second(fname)
    doname = Symbol("do_", string(fname))
    qname = QuoteNode(fname)
    esc(quote
        function ($(doname))(mx,arg1)
            mx
        end
        function do_GenHead(mx,head::Mxpr{$(qname)})
            args = copy(margs(mx))
            mxpr(mhead(head),args[1],margs(head)...,args[2:end]...)
        end
    end)
end

macro curry_last(fname)
    doname = Symbol("do_", string(fname))
    qname = QuoteNode(fname)
    esc(quote
        function ($(doname))(mx,arg1)
            mx
        end
        function do_GenHead(mx,head::Mxpr{$(qname)})
            mxpr(mhead(head),copy(margs(mx))...,margs(head)...)
        end
    end)
end

macro curry_split(fname)
    doname = Symbol("do_", string(fname))
    qname = QuoteNode(fname)
    esc(quote
        function ($(doname))(mx,arg1)
            mx
        end
        function do_GenHead(mx,head::Mxpr{$(qname)})
            mxpr(mhead(head),margs(head)[1],copy(margs(mx))...,margs(head)[2])
        end
    end)
end
