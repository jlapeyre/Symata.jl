## TODO: Reorganize this. Migrate most of the Heads handled here elsewhere.
## Which heads should be here ?


# this breaks pre-compilation only with OSX and Windows on nightliles starting about Oct 18, 2016

# No, this appears to save nightly builds on al least linux. otherwise
# nightlies fail on every platform
if VERSION >= v"0.5-"
    import Combinatorics: permutations
end

# So we return to loadinb Combinatorics unconditionally and tolerating warnings in v0.4
#using Combinatorics


### Apply

@sjdoc Apply """
    Apply(f,expr)   f .% expr

replace the `Head` of `expr` with `f`.

`Apply` also works with some
Julia objects. For example `Apply(Plus, :( [1:10] ))` returns `55`. `Apply` can be used
in operator form. For example

```
m = Apply(Plus)
m(f(a,b,c))
```
"""
@mkapprule Apply :nodefault => true

@curry_first Apply

@doap Apply(f,g) = mx

@doap  function Apply(head::SJSym,mxa::Mxpr)
    head == :Plus && return _apply_plus(mxa)
    if head == :Times # 4 or 5 times faster for plus on numbers, don't evaluate
#        mx = mxpr(head,copy(margs(mxa))) # we may find that we need to copy
        mx = mxpra(head,margs(mxa))
        mx = canonexpr!(mx)            # this is ok
        setcanon(mx)
    else
        mx = mxpra(head,margs(mxa))
    end
    isa(mx,Mxpr) && isempty(mx) && return 0   # do this instead. fixes bug Apply(Times, [DirectedInfinity(),0]) --> 0
    mx
end

function _apply_plus(mxa::Mxpr)
    mx = mxpra(:Plus,margs(mxa))
    mx = canonexpr!(mx)
    setcanon(mx)
    return isa(mx,Mxpr) && isempty(mx) ? 0 : mx
end


@doap Apply(h,mxa::Mxpr) = mxpra(h,margs(mxa))

@doap function Apply{T<:Number}(h::SJSym,arr::AbstractArray{T})
    h == :Plus && return convert(T,sum(arr))
    h == :Times && return convert(T,prod(arr))
    mx
end

### Hash

@mkapprule Hash :nargs => 1
@doap Hash(x) = hash(x)

### Head

@sjdoc Head """
    Head(expr)

return the `Head` of `expr`.

`expr` may be a Symata expression or object of any Julia type.

!!! note
    The `Head` of a Julia expression is `Expr`, for instance,
    `Head( :( :( a = 1) ))` returns `Expr`. Note we have to quote twice, because one level of
    a quoted Julia expression is evaluated so that we can embed Julia code.
"""

@mkapprule Head  :nargs =>  1

@doap Head(mx1::Mxpr) = mhead(mx1)
@doap Head(s::SJSym) = getsym(:Symbol)  # or just :Symbol ? This is the ancient inteface
@doap Head(ex) = typeof(ex)

## `Function` is the head of Symata pure functions. So we use a different head for Julia functions
# Unclear what to do here.
# typeof( (x) -> x) in
# Julia v0.4 : Function
# Julia v0.5 and later : a hash key or something

@sjdoc CompiledFunction """
    CompiledFunction

is the head of compiled functions.

Compiled functions can be written directly in the host language, Julia.
```
f = :( x -> x^2 )
```

They may also be compiled from Symata expressions

```
f = Compile([x], x^2)
```
"""
@doap Head{T<:Function}(f::T) = :CompiledFunction

### Isa

@sjdoc Isa """
    Isa(x,type)

return `True` if `x` is of type `type`.
"""
@mkapprule Isa

@doap Isa(x,T::DataType) = isa(x,T)
@doap Isa(x,T::Symbol) = isa(x,eval(T))
@doap Isa(x,T) = mx
    
#const Float = AbstractFloat

### ReleaseHold

#typealias Holds Union{Mxpr{:Hold}, Mxpr{:HoldForm}, Mxpr{:HoldPattern}, Mxpr{:HoldComplete}}

@mkapprule ReleaseHold :nargs => 1

@sjdoc ReleaseHold """
    ReleaseHold(expr)

removes the outer layer of `Hold`, `HoldForm`, `HoldPattern`, and `HoldComplete` from `expr`.
"""

@doap function ReleaseHold(mxa::Holds)
#    length(margs(mxa)) == 0 && return mxpr(:Sequence)  ## delete theses lines after a while
    isempty(mxa) && return mxpr(:Sequence)    
    length(mxa) > 1 && return  mxpra(:Sequence,margs(mxa))
#    length(margs(mxa)) > 1 && return  mxpra(:Sequence,margs(mxa))    
    return mxa[1]
end

@doap ReleaseHold(ex) = ex

### Reverse

function Base.reverse(mx::Mxpr)
    mx1 = copy(mx)
    Base.reverse!(margs(mx1))
    return mx1
end

@sjdoc Reverse """
    Reverse(expr)

reverse the order of the arguments in `expr`.
"""

@mkapprule Reverse :nargs => 1
@doap function Reverse(ex::Mxpr)
    get_attribute(ex,:Orderless) && return ex
    setfixed(mxpra(mhead(ex),reverse(margs(ex))))
end
@doap Reverse(ex::AbstractArray) = reverse(ex)

@mkapprule Reverse! :nargs => 1
@doap function Reverse!(ex::Mxpr)
    get_attribute(ex,:Orderless) && return ex    
    reverse!(margs(ex))
    ex
    end
@doap Reverse!(ex::AbstractArray) = reverse!(ex)

### Permutations

@sjdoc Permutations """
    Permutations(expr)

give a list of all permutations of elements in `expr`.
"""

function apprules(mx::Mxpr{:Permutations})
    perms = collect(permutations(margs(mx[1])))
    h = mhead(mx[1])
    len = length(perms)
    nargs = newargs(len)
    @inbounds for i in 1:len
        nargs[i] = setfixed(mxpr(:List,perms[i]))
    end
    setfixed(mxpra(:List,nargs))
end

@sjdoc FactorInteger """
    FactorInteger(n)

give a list of prime factors of `n` and their multiplicities.
"""

apprules(mx::Mxpr{:FactorInteger}) = setfixed(mxpra(:List,do_unpack(factor(mx[1]))))

### Level


### Map

@sjdoc Map """
    Map(f,expr)   f % expr

return `f` applied to each element in a `expr`.

    Map(f,expr,levelspec)

map at levels specified by `levelspec`.

`levelspec` is a standard level specification.

- `n`       levels `0` through `n`.
- `[n]`     level `n` only.
- `[n1,n2]` levels `n1` through `n2`

Negative indices count backwards from the deepest level.
`Infinity` specifies the deepest level.

`expr` is copied first. `f` can be a Symata object or a Julia function.

`Map` can be used in an operator form. For example `Map(f)(expr)`.
"""

@mkapprule Map

@doap function Map(f::Function,expr::Mxpr)
    args = margs(expr)
    nargs = newargs(args)
    @inbounds for i in 1:length(args)
        nargs[i] = f(args[i]) # Probably need more evaluation
    end
    mxpra(mhead(expr),nargs)
end

# Should we return an Array or a List ? We choose List now.
@doap function Map(f::Function, a::AbstractArray)
    nargs = newargs(length(a))
    @inbounds for i in 1:length(a)
        nargs[i] = f(a[i])
    end
    mxpra(:List,nargs)
end

# We create one Mxpr outside the loop. Old
# code (commented out) created Mxpr every time.
# This saves 30 percent of time and allocation in some tests.
@doap function Map(f,expr::Mxpr)
    args = margs(expr)
    nargs = newargs(args)
    mx1 = mxpr(f,0) # reserve one argument
    @inbounds for i in 1:length(args)
        mx1.args[1] = args[i]  # map f of one argument
        nargs[i] = doeval(mx1)
    end
    mxpra(mhead(expr),nargs)
end

type MapData
    action
end

## A bug in meaning of level spec of `2` is not caught because we check if p is Null below.
## level spec 2 should mean  from 1 to 2, but the level spec code interpreted it as 0 through 2.
@doap function Map(f,expr::Mxpr, inlevspec)
    levelspec = make_level_specification(expr, inlevspec)
    ex = recursive_copy(expr)
    data = MapData(nothing)
    action = LevelAction(data,
                         function (data,expr)
                         p = data.action.parent
                           if p != Null
                             p[data.action.subind] = mxpr(f,expr)
                           end
                         end)
    data.action = action
    traverse_levels!(action, levelspec, ex)
    if has_level_zero(levelspec)
        ex = mxpr(f,ex)
    end
    ex
end

@curry_first Map

### ToExpression

apprules(mx::Mxpr{:ToExpression}) = do_ToExpression(mx,margs(mx)...)

@sjdoc ToExpression """
    ToExpression(str)

convert string `str` to an expression.
"""

do_ToExpression{T<:AbstractString}(mx,s::T) = symataevaluate(parse(s), EvaluateJuliaSyntaxSimple())
do_ToExpression(mx,s) = s
do_ToExpression(mx,args...) = mx
set_pattributes("ToExpression")

### Count

@sjdoc Count """
    Count(expr,pattern)

return the number of arguments in `expr` than match `pattern`.

Only matching on one level is supported. `Count(pattern)` can be used as the head of an expression,
as an operator. For instance, `cop = Count(_^2)` defines a function that counts
the number of arguments that have the form of a square.

`Count` also works when `expr` is a Julia `Dict`.
"""

@sjexamp( Count,
         ("Count(Range(10), _Integer)", "10"),
         ("Count(_Integer)(Range(10))", "10"),
         ("Count(Range(10), 2)", "1"))

set_pattributes("Count")
function apprules(mx::Mxpr{:Count})
    do_Count(mx,margs(mx)...)
end


# Allocating outside loop and sending Dict as arg is 3x faster in one test
function do_Count(mx,expr,pat)
    args = margs(expr)
    c = 0
    jp = patterntoBlank(pat)
    capt = capturealloc()
    @inbounds for i in 1:length(args)
        (gotmatch,capt) = match_and_capt(args[i],jp,capt)
        gotmatch ? c += 1 : nothing
    end
    return c
end

@curry_last Count

### Cases

@sjdoc Cases """
    Cases(expr,pattern)

return the elements in `expr` that match `pattern`.

    Cases(expr,pattern,levelspec)

return the elements in `expr` on levels specified by `levelspec` that match `pattern`.

The head of the returned object is the same as that of `expr`.

`Cases(pattern)` can be used as the `Head` of an expression, as an operator.
For example, `getints = Cases(_Integer)`.
"""

@sjexamp( Cases,
         ("Cases([1,2.0,3,\"dog\"], _Integer)", "[1,3]"))

@mkapprule Cases

# Allocating outside loop and sending Dict as arg is 3x faster in one test
# @doap function Casesold(expr,pat)
#     args = margs(expr)
#     nargs = newargs()
#     jp = patterntoBlank(pat)
#     capt = capturealloc()
#     @inbounds for i in 1:length(args)
#         ex = args[i]
#         (gotmatch,capt) = match_and_capt(ex,jp,capt)
#         gotmatch ? push!(nargs,sjcopy(ex)) : nothing
#     end
#     mxpr(:List,nargs)
# end

# Allocating outside loop and sending Dict as arg is 3x faster in one test

type CasesData
    new_args
    jp
    capt
end

# We have no level spec
@doap function Cases(expr,pat)
    new_args = newargs()
    jp = patterntoBlank(pat)
    capt = capturealloc()
    data = CasesData(new_args,jp,capt)
    local action
    if isa(pat,RuleT)
        action = LevelAction(data, function (data, expr)
                             (gotmatch,res) = replace(expr,data.jp)
                             gotmatch ? push!(data.new_args,res) : nothing
                             end)
    else
        action = LevelAction(data, function (data, expr)
                             (gotmatch,capt) = match_and_capt(expr,data.jp,data.capt)
                             gotmatch ? push!(data.new_args,sjcopy(expr)) : nothing
                             end)
    end
    traverse_levels!(action,LevelSpecAtDepth(1),expr)
    mxpra(:List,new_args)
end

function _doCases(levelspec::LevelSpec, expr ,pat)
    new_args = newargs()
    jp = patterntoBlank(pat)
    capt = capturealloc()
    data = CasesData(new_args,jp,capt)
    # action = LevelAction(data, function (data, expr)
    #                         (gotmatch,capt) = match_and_capt(expr,data.jp,data.capt)
    #                         gotmatch ? push!(data.new_args,sjcopy(expr)) : nothing
    #                      end)
    if isa(pat,RuleT)
        action = LevelAction(data, function (data, expr)
                             (gotmatch,res) = replace(expr,data.jp)
                             gotmatch ? push!(data.new_args,res) : nothing
                             end)
    else
        action = LevelAction(data, function (data, expr)
                             (gotmatch,capt) = match_and_capt(expr,data.jp,data.capt)
                             gotmatch ? push!(data.new_args,sjcopy(expr)) : nothing
                             end)
    end
    traverse_levels!(action,levelspec,expr)
    mxpra(:List,new_args)
end

@doap function Cases(expr,pat,inlevelspec)
    levelspec = make_level_specification(expr, inlevelspec)
    _doCases(levelspec,expr,pat)
end


@curry_last Cases

### DeleteCases

@sjdoc DeleteCases """
    DeleteCases(expr,pattern)

deletes the elements in `expr` that match `pattern`.

The head of the returned object is the same as that of expr. Only matching on one level is supported.

`DelteCases(pattern)` can be used as the head of an expression, as an operator.
For example `noints = DeleteCases(_Integer)`.
"""

@sjexamp( DeleteCases,
         ("DeleteCases([1,2.0,3,\"dog\"], _Integer)", "[2.0,\"dog\"]"))


@mkapprule DeleteCases

# Allocating outside loop and sending Dict as arg is 3x faster in one test
@doap function DeleteCases(expr,pat)
    args = margs(expr)
    new_args = newargs()
    jp = patterntoBlank(pat)
    capt = capturealloc()
    @inbounds for i in 1:length(args)
        (gotmatch,capt) = match_and_capt(args[i],jp,capt)
        gotmatch ? nothing : push!(new_args,sjcopy(args[i])) # The difference from Cases
    end
    rmx = mxpra(mhead(expr),new_args)
    return rmx
end

@curry_last DeleteCases


### Push!

@sjdoc Push! """
    Push!(a,val)

pushes `val` onto the expression that `Symbol` `a` evaluates to.

!!!  note
   Typically, computer algebra systems follow the principle that expressions should be immutable.
   Rather than modify an expression, a copy is modified. `Push!` violates this principle. It could
   interact poorly with other parts of Symata.
"""

@sjexamp( Push!,
         ("ClearAll(a,b)",""),
         ("a = []",""),
         ("For(i=1, i < 1000, Increment(i), Push!(a,Symbol(\"b\$i\")))",""))

set_pattributes(["Push!"],[:HoldFirst])
apprules(mx::Mxpr{:Push!}) = do_Push(mx,margs(mx)...)
do_Push(mx,args...) = mx
do_Push(mx,x::SJSym,val) = do_Push1(mx,symval(x),val)
do_Push1(mx,x,val) = mx
do_Push1(mx,x::Mxpr,val) = (push!(x.args,val); x)

### Pop!

@sjdoc Pop! """
    Pop!(expr)

pops a value from the arguments of `expr`.

`Pop!` mutates `expr`.
"""

@mkapprule Pop!

@doap function Pop!(mx1::Mxpr)
    length(mx1) < 1 && return mx1 # and warn or error
    pop!(mx1.args)
end

@doap Pop!(x) = x

@sjseealso_group(Pop!, Push!)

## This is not how this works
### Composition
# @sjdoc Composition """
#     Composition([f,g,...],arg)
# returns f(g(...(arg)))
# """
# @mkapprule Composition
# function do_GenHead(mx,head::Mxpr{:Composition})
#     args = copy(margs(mx))
#     mxpr(mhead(head),args[1],margs(head)...,args[2:end]...)
# end

### ComposeList

@sjdoc ComposeList """
    ComposeList([f1,f2,...],x)

returns `[f1(x),f2(f1(x)),...]`.
"""
@mkapprule ComposeList :nargs => 2

@doap function ComposeList(list::ListT,x)
    ops = reverse(margs(list))
    mout = mxpr(ops[1],x)
    nargs = newargs(1)
    nargs[1] = mout
    for i in 2:length(ops)
        mout = mxpr(ops[i],mout)
        push!(nargs,mout)
    end
    mxpra(:List,nargs)
end

evalifdelayed(r::Mxpr{:Rule}) = rhs(r)
evalifdelayed(r::Mxpr{:RuleDelayed}) = doeval(rhs(r))

### ReplacePart

@mkapprule ReplacePart """
    ReplacePart(expr, i => repl)

returns a copy of `expr` with the `i`th part replaced by `repl`
"""  :nargs => 1:2

@doap ReplacePart{T<:Union{RulesT,ListT}}(expr::Mxpr,arg::T) = replacepart1(mx,expr,arg)

@curry_second ReplacePart

function replacepart1(mx,expr,arg)
    nexpr = deepunsetfixed(recursive_copy(expr)) ## Mma must use some kind of lazy copying
    replacepart(mx,nexpr,arg)
end

function replacepart(mx,expr,rule::RulesT)
    _rhs = evalifdelayed(rule)
    if listofpredq(lhs(rule), integerq)
       expr = replaceonepart(mx,expr,lhs(rule),evalifdelayed(rule))
    elseif listq(lhs(rule)) # lhs is a List
        foreach( x -> (expr = replaceonepart(mx,expr,x,_rhs)), lhs(rule))
    else
       expr = replaceonepart(mx,expr,lhs(rule),_rhs)
    end
    expr
end

function replacepart(mx,expr,arg::ListT)
    if listofpredq(arg,ruleq)
        foreach( r -> (expr = replaceonepart(mx,expr,lhs(r),evalifdelayed(r))), arg)
    end
    expr
end

function replaceonepart(mx,expr,_lhs::Integer,_rhs)
    if _lhs == 0
        expr = mxprnewhead(expr,_rhs)
    else
        expr[posnegi(expr,_lhs)] = _rhs
    end
    expr
end

function replaceonepart(mx,expr,_lhs::ListT,_rhs)
    if ! listofpredq(_lhs,integerq)
        symwarn("$_lhs is not a part specification")
        return mx
    end
    setpart2!(expr,_rhs,margs(_lhs)...)
#    expr[margs(_lhs)...] = _rhs
    expr
end

### Level

@mkapprule Level :nargs => 2:3

@sjdoc Level """
    Level(expr,levelspec)

returns a list of all parts at `levelspec`.

    Level(expr,levelspec,f)

applies `f` to each part in the returned list.

`Level` traverses the expression breadth first. Negative indices count from the
depth of the entire expression, rather than from each leaf.
"""


type LevelData
    levellist
    action
end



@doap function Level(expr, inlevelspec)
    levelspec = make_level_specification(expr,inlevelspec)
    nargs = newargs()
    data = LevelData(nargs,nothing)
    action = LevelAction(data,
                         function(data,expr)
#                         act = data.action
#                         println(act.levelind, " ", act.subind)
                           push!(data.levellist,expr)
                         end)
    data.action = action
    traverse_levels!(action, levelspec,expr)
    ## todo level zero
    mxpra(:List,nargs)
end

@doap function Level(expr, inlevelspec, f)
    levelspec = make_level_specification(expr,inlevelspec)
    nargs = newargs()
    data = LevelData(nargs,nothing)
    action = LevelAction(data,
                         function(data,expr)
                           push!(data.levellist,mxpr(f,expr))
                         end)
    data.action = action
    traverse_levels!(action, levelspec,expr)
    ## todo level zero
    mxpra(:List,nargs)
end

### ExpandA, only a bit is implemented. Sympy Expand is more capable.

@sjdoc ExpandA """
    ExpandA(expr)

expand products in `expr`. This is only partially implemented,
mostly to test the efficiency of evaluation and evaluation control. The
Sympy version `Expand()` is more capable, but slower.
"""

apprules(mx::Mxpr{:ExpandA}) = _doexpand(mx[1])
