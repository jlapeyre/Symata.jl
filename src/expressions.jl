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
    Apply(f,expr)

replace the `Head` of `expr` with `f`.

`Apply` also works with some
Julia objects. For example `Apply(Plus, :( [1:10] ))` returns `55`. `Apply` can be used
in operator form. For example

```
m = Apply(Plus)
m(f(a,b,c))
```
"""

# Why mkapprule does not work ?
apprules(mx::Mxpr{:Apply}) = do_Apply(mx,margs(mx)...)

@curry_first Apply

# This allows things like:  Apply(f)([a,b,c])
# do_Apply(mx,f) = mx
# do_Apply(mx,x,y) = mx

function do_Apply(mx::Mxpr,head::SJSym,mxa::Mxpr)
    if (head == :Plus || head == :Times ) # 4 or 5 times faster for plus on numbers, don't evaluate
#        mx = mxpr(head,copy(margs(mxa))) # we may find that we need to copy
        mx = mxpr(head,margs(mxa))
        mx = canonexpr!(mx)            # this is ok
        setcanon(mx)
    else
        mx = mxpr(head,margs(mxa))
    end
    is_Mxpr(mx) && length(mx) == 0 && return 0   # do this instead. fixes bug Apply(Times, [DirectedInfinity(),0]) --> 0
    mx
end

do_Apply(mx::Mxpr,h,mxa::Mxpr) = mxpr(h,margs(mxa))

# Apply operation to a typed numeric array.
# We can build these functions with a macro and
# mapping from  :Times -> mmul
# :Cos -> cos, etc.
function do_Apply{T<:Number}(mx::Mxpr,h::SJSym,arr::Array{T})
    if h == :Plus
        s = zero(T)
        for i in 1:length(arr)
            s += arr[i]
        end
        return s
    end
    return mx
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

@doap function Isa(x,T::DataType)
    isa(x,T)
end

const Float = AbstractFloat

@doap function Isa(x,T::Symbol)
    t = eval(T)
    isa(x,t)
end

@doap Isa(x,T) = mx

### ReleaseHold

#typealias Holds Union{Mxpr{:Hold}, Mxpr{:HoldForm}, Mxpr{:HoldPattern}, Mxpr{:HoldComplete}}

@mkapprule ReleaseHold :nargs => 1

@sjdoc ReleaseHold """
    ReleaseHold(expr)

removes the outer layer of `Hold`, `HoldForm`, `HoldPattern`, and `HoldComplete` from `expr`.
"""

@doap function ReleaseHold(mxa::Holds)
    length(margs(mxa)) == 0 && return mxpr(:Sequence)
    length(margs(mxa)) > 1 && return  mxpr(:Sequence,margs(mxa)...)
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

function apprules(mx::Mxpr{:Reverse})
    do_reverse(mx[1])
end

# Builtin Orderless
# they would only be resorted
do_reverse(mx::Orderless) = mx

function do_reverse(mx::Mxpr)
    if get_attribute(mx,:Orderless)
        return mx
    end
    setfixed(mxpr(mhead(mx),reverse(margs(mx))))
end

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
    setfixed(mxpr(:List,nargs))
end

@sjdoc FactorInteger """
    FactorInteger(n)

give a list of prime factors of `n` and their multiplicities.
"""

apprules(mx::Mxpr{:FactorInteger}) = setfixed(mxpr(:List,do_unpack(factor(mx[1]))))

### Level




### Map

@sjdoc Map """
    Map(f,expr)

return `f` applied to each element in a `expr`.

`expr` is copied first. `f` can be a Symata object or a Julia function.

`Map` can be used in an operator form. For example `Map(f)(expr)`.
"""

@mkapprule Map

function do_Map(mx::Mxpr{:Map},f::Function,expr::Mxpr)
    args = margs(expr)
    nargs = newargs(args)
    @inbounds for i in 1:length(args)
        nargs[i] = f(args[i]) # Probably need more evaluation
    end
    mxpr(mhead(expr),nargs)
end

# Should we return an Array or a List ? We choose List now.
function do_Map(mx::Mxpr{:Map},f::Function, a::AbstractArray)
    nargs = newargs(length(a))
    @inbounds for i in 1:length(a)
        nargs[i] = f(a[i])
    end
    mxpr(:List,nargs)
end

# We create one Mxpr outside the loop. Old
# code (commented out) created Mxpr every time.
# This saves 30 percent of time and allocation in some tests.
function do_Map(mx::Mxpr{:Map},f,expr::Mxpr)
    args = margs(expr)
    nargs = newargs(args)
    mx = mxpr(f,0) # reserve one argument
    @inbounds for i in 1:length(args)
        mx.args[1] = args[i]  # map f of one argument
        nargs[i] = doeval(mx)
    end
    mxpr(mhead(expr),nargs)
end

@curry_first Map

### ToExpression

apprules(mx::Mxpr{:ToExpression}) = do_ToExpression(mx,margs(mx)...)

@sjdoc ToExpression """
    ToExpression(str)

convert string `str` to an expression.
"""
do_ToExpression{T<:AbstractString}(mx,s::T) = exfunc(parse(s), SimpleExFuncOptions)
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

function sjcopy{T<:Union{AbstractString,Symbol}}(s::T)
    identity(s)
end

function sjcopy(x)
    copy(x)
end

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
    if is_Mxpr(pat,:Rule)
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
    mxpr(:List,new_args)
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
    if is_Mxpr(pat,:Rule)
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
    mxpr(:List,new_args)
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
    rmx = mxpr(mhead(expr),new_args)
    return rmx
end

@curry_last DeleteCases

### FreeQ

# don't know how to put this in the macro string
# const levelstring = """
# - `n`       levels `0` through `n`.
# - `[n]`     level `n` only.
# - `[n1,n2]` levels `n1` through `n2`
# """

@mkapprule FreeQ :nargs => 1:3

@doap FreeQ(expr, pattern) = freeq(LevelSpecAll(),expr,pattern)

@doap FreeQ(expr, pattern, inlevelspec)  = freeq(make_level_specification(expr, inlevelspec), expr, pattern)

@curry_second FreeQ

@sjdoc FreeQ """
    FreeQ(expr, pattern)

return `True` if no subexpression of `expr` matches `pattern`.

    FreeQ(expr, pattern, levelspec)

return `True` if `expr` does not match `pattern` on levels specified by `levelspec`.

- `n`       levels `0` through `n`.
- `[n]`     level `n` only.
- `[n1,n2]` levels `n1` through `n2`

    FreeQ(pattern)

operator form.
"""

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
