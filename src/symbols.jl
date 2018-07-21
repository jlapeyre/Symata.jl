## FIXME: implement SubValues

# NB. attributes for builtin symbols are mostly set in protected_symbols.jl

# Check if we are trying to add to a symbol bound to itself. If
# so, we warn and return the unevaluated expression. This avoids
# an infinite evaluation loop. This is what Mma does.
macro checkunbound(mx,x,sv)
    esc( quote
           $sv = symval($x)   # could replace with isbound($x)
           if $sv == $x
             @warn("The symbol " * string($x) * " does not have a value, so it's value cannot be changed")
             setfixed($mx)
             return $mx
          end
    end)
end

protectstr(s) = "Symbol '" * string(s) * "' is proctected. Did you mean `==` ?"

function checkprotect(s::Qsym)
    get_attribute(s,:Protected) &&
    symerror(protectstr(s))
end

function checkprotect(s::SJSym)
    isProtected(s) && symerror(protectstr(symname(s)))
end

checkprotect(mx::Mxpr) = checkprotect(mhead(mx))

function warncheckprotect(s::SJSym)
    if isProtected(s)
        symwarn(protectstr(symname(s)))
        return false
    else
        return true
    end
end
warncheckprotect(mx::Mxpr) = warncheckprotect(mhead(mx))

### Attributes

@mkapprule Attributes :nargs => 1  """
    Attributes(s)

returns attributes associated with symbol `s`. Builtin symbols have
the attribute `Protected`, and may have others, including `HoldFirst`, `SequenceHold`,
`HoldRest`, `HoldAll`, `ReadProtected`, `Constant`, `Locked`, `Flat`, `Listable`,
`NumericFunction`, `OneIdentity`.
"""

@doap Attributes(s::SymString) = get_attributesList(Symbol(s))

get_attributesList(sj::SJSymbol) = tolistfixed(get_attributes(sj))

### SetAttributes

@mkapprule SetAttributes :nargs => 2  """
    SetAttributes(sym,attr)

add `attr` to the list of attributes for `sym`.

    SetAttributes(list,attr)

add `attr` to the list of attributes for each symbol in `list`.

    SetAttributes(sym,list)

add each attribute in `list`  to the list of attributes for `sym`.
"""

function check_set_attributes(sym::SJSymbol, attr::SJSym)
    checkprotect(sym)
    set_attribute(sym,attr)
    Null
end

set_attributes(sym::SJSymbol, attr::SJSym) = check_set_attributes(sym,attr)

function set_attributes(sym::SJSymbol, attrs::T) where T<:Array
    foreach(a -> check_set_attributes(sym,a), attrs)
end

function set_attributes(syms::T, attr::SJSym) where T<:Array
    foreach(sym -> check_set_attributes(sym,attr), syms)
end

@doap SetAttributes(sym::SJSymbol, attr::SJSym) = set_attributes(sym,attr)
@doap SetAttributes(syms::Mxpr{:List}, attr::SJSym) = set_attributes(margs(syms),attr)
@doap SetAttributes(sym::SJSymbol, attrs::Mxpr{:List}) = set_attributes(sym,margs(attrs))

### ClearAttributes

@sjdoc ClearAttributes """
    ClearAttributes(sym,attr)

removes `attr` from the list of attributes of symbol `sym`.
"""
@mkapprule ClearAttributes :nargs => 2

@doap ClearAttributes(sym,attr) = (unset_attribute(sym,attr); Null)

### Unprotect

###  Wolfram does this: Protect[a,b,c,....] returns a list of symbol names (strings) of those symbols which were not
###  protected before, but now are. Non-symbols are silently ignored. Symbols which were already protected are
###  silently ignored.

@sjdoc Unprotect """
    Unprotect(z1,z2,...)

removes the `Protected` attribute from the symbols `z1, z2, ...`.
"""

function apprules(mx::Mxpr{:Unprotect})
    nargs = newargs()
    for i in 1:length(mx)
        ret = do_unprotect(mx,mx[i])
        if ret != false
            push!(nargs, ret)
        end
    end
    mxprcf(:List, nargs)
end

function do_unprotect(mx,a::SJSym)
    if isProtected(a)
        unprotect(a)
        return string(a)
    end
    return false
end

do_unprotect(mx,a) = false

### Protect

@sjdoc Protect """
    Protect(z1,z2,...)

add the `Protected` attribute to the lists of attributes for the symbols `z1, z2, ...`.
"""

# FIXME: Following broke after updating to v0.6.0-rc2 from an older v0.6.0
#@mkapprule Protect :nargs => 1:Inf  :nodefault => true
@mkapprule Protect :nodefault => true

@doap function Protect(args...)
    nargs = newargs()
    for x in args
        ret = do_protect(x)
        if ret != false
            push!(nargs, ret)
        end
    end
    mxprcf(:List, nargs)
end

function do_protect(a::SJSym)
    isProtected(a) && return false
    protect(a)
    string(a)
end

### Set and SetDelayed

@sjdoc Set """
    Set(a,b), a = b

Sets the value of `a` to `b`. `b` is evaluated only once, when `a=b` is evaluated.

    obj[i,j,...] = val

set a part of `obj` to `val`. `obj` can be a Symata expression
or a Julia object, such as an `Array` or `Dict`.
"""

@sjseealso_group( Set, SetDelayed, UpSet, DownValues, UpValues )

@sjexamp( Set,
         ("Clear(a,b,c)",""),
         ("b = a", "a"),
         ("a = 1", "1"),
         ("c = a", "1"),
         ("a = 2", "2"),
         ("b", "2"),
         ("c", "1"))

@sjdoc SetDelayed """
    SetDelayed(a,b), a := b

Whenever `a` is evaluated, `b` is evaluated and the result is assigned to `a`.

Note that `a` is not set to the value of `b` at the time `a := b` is evaluated, but
rather to the current value of `b` every time `a` is evaluated.
"""

# Set SJSym value.
# Set has HoldFirst, SetDelayed has HoldAll.

@mkapprule Set nargs => 1:Inf

# This is what Mma does.
# When is this used ?
function do_Set(mx::Mxpr{:Set}, lhs::SJSym)
    checkprotect(lhs)
    rhs = mxprcf(:Sequence)
    setsymval(lhs,rhs)
    setdefinition(lhs, mx) # This is correct. Definition includes `Set`.
    rhs
end

## Destructured. *Can* do [x,y] = [y,x]
@doap function Set(lhs::Mxpr{:List},rhs::Mxpr{:List})
    length(lhs) != length(rhs) && return mx
    for i in 1:length(lhs)
        doeval(mxpr(:Set,lhs[i],rhs[i]))
    end
    rhs
end

@mkapprule SetDelayed nargs => 1:Inf  nodefault => true

@doap SetDelayed(lhs,rhs) = setdelayed(mx,lhs,rhs)

@doap SetDelayed(lhs, rhs...) = mxpr(:SetDelayed, lhs, mxpr(:Sequence,rhs...))

@doap SetDelayed() = mx

# getsym(symname(lhs)) is because a copy of symbol is being made somewhere
# so we look up the original in the table
# SetDelayed is not correct. rhs is also evaluated because
# lhs is evaluated twice in order to find fixed point.
# we have treat this specially somehow, not ordinary evaluation.
#
# Note added (Apr 2016): This is working AFAICT
# For evaluation, we can use setsymval for both set and setdelayed
# The only difference is whether we return the rhs.
# But, to save the definitions to a file, we need to record whether we had set or setdelayed,
# so we use setdelayedval
function setdelayed(mx::Mxpr{:SetDelayed},lhs::SJSymbol, rhs)
    checkprotect(lhs)
    setsymval(lhs,rhs)   # Using this works just as well. But, for printing, we don't whether Set or SetDelayed
    setdefinition(lhs, mx)
    add_completion_symbols(string(lhs))
    Null
end

# TODO: Use OwnValues here rather than binding.
function do_Set(mx::Mxpr{:Set},lhs::SJSymbol, rhs)
    checkprotect(lhs)
    setsymval(lhs,rhs)
    setdefinition(lhs, mx)
    add_completion_symbols(string(lhs))
    rhs
end

# Create DownValue. "function" definition
# eg f(x_) := x  defines a DownValue for the SJSym f
function setdelayed(mx,lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    h = mhead(lhs)
    set_downvalue(mx,h,rule) # push DownValue
    if isa(h,Symbol) add_completion_symbols(string(h)) end
    rule
    Null
end

function do_Set(mx::Mxpr{:Set},lhs::Mxpr{:Part}, rhs::Mxpr{:Module})
    symerror("$mx is not implemented")
end

# Mma is not clear but seems to evaluate the first arg to the lhs (the expression
# whose part we want) exactly once. We should document what we do.
# We check is_Number several times, because we may have a Dict.
#function do_Set(mx::Mxpr{:Set},lhs::Mxpr{:Part}, rhs)
@doap function Set(lhs::Mxpr{:Part}, rhs)
    ex0 = meval(expr(lhs))  # evaluate once, eg, to get expr from symbol.
    tinds = indices(lhs)
    if isa(tinds[1],Mxpr{:Sequence}) ## is this the correct place to unwrap the Sequence ?
        tinds[1] = margs(tinds[1])
    end
    inds = map(doeval,tinds)
    val = doeval(rhs)
    ex = ex0
    exlast = ex0
    for j in 1:length(inds)-1
        exlast = ex
        ex = get_part_one_ind(ex,inds[j])
    end
    ind = inds[end] # doeval(tinds[end])
    ind = posnegi(ex,ind)
    if isa(ind,Number) && ind == 0
        exlast[tinds[length(inds)-1]] = mxprnewhead(ex,val)
    else
        ex[ind] = val
    end
    unsetfixed(ex0)
    val
end

# Optimize a bit. Localize variables once, not every time pattern is evaluated
setdelayed(mx,lhs::Mxpr, rhs::Mxpr{:Module}) = setdelayed(mx,lhs,localize_module!(rhs))

@doap Set(lhs::Mxpr, rhs::Mxpr{:Module}) = do_Set(mx,lhs,localize_module!(rhs))

### Increment

@sjdoc Increment """
    Increment(n)

increments the value of `n` by `1` and returns the old value.
"""

@mkapprule Increment :nargs => 1

@doap function Increment(x::SJSym)
    @checkunbound(mx,x,xval)
    do_increment1(mx,x,xval)
end

function do_increment1(mx,x,xval::T) where T<:Number
    setsymval(x,mplus(xval,1))  # maybe + is ok here.
    return xval
end

function do_increment1(mx,x,val)
    setsymval(x,doeval(mxpr(:Plus,val,1)))
    return val
end

### Decrement

@sjdoc Decrement """
    Decrement(n)

decrements the value of `n` by `1` and returns the old value.
"""

@mkapprule Decrement :nargs => 1

#function do_Decrement(mx, x::SJSym)
@doap function Decrement(x::SJSym)
    @checkunbound(mx,x,xval)
    do_decrement1(mx,x,xval)
end

function do_decrement1(mx,x,xval::Number)
    setsymval(x,mplus(xval,-1))  # maybe + is ok here.
    return xval
end

function do_decrement1(mx,x,val)
    setsymval(x,doeval(mxpr(:Plus,val,-1)))
    return val
end

### TimesBy

@sjdoc TimesBy """
    TimesBy(a,b), or a *= b

set `a` to `a * b` and returns the new value. This is currently
faster than `a = a * b` for numbers.
"""

@mkapprule TimesBy :nargs => 2

function do_TimesBy(mx::Mxpr{:TimesBy}, x::SJSym,val)
    @checkunbound(mx,x,xval)
    do_TimesBy1(mx,x,xval,val)
end

function do_TimesBy1(mx,x,xval::T, val::V) where {T<:Number,V<:Number}
    r = mmul(xval,val)
    setsymval(x,r)
    return r
end
function do_TimesBy1(mx,x,xval,val)
    setsymval(x,doeval(mxpr(:Set,x, mxpr(:Times,xval,val))))
    return symval(x)
end


#### AddTo

@sjdoc AddTo """
    AddTo(a,b), or a += b,

sets `a` to `a + b` and returns the new value. This is currently
faster than `a = a + b` for numbers.
"""

@mkapprule AddTo :nargs => 2

function do_AddTo(mx::Mxpr{:AddTo},x::SJSym,val)
    @checkunbound(mx,x,xval)
    do_AddTo1(mx,x,xval,val)
end

function do_AddTo1(mx,x,xval::T, val::V) where {T<:Number,V<:Number}
    r = mplus(xval,val)
    setsymval(x,r)
    return r
end
function do_AddTo1(mx,x,xval,val)
    setsymval(x,doeval(mxpr(:Set,x, mxpr(:Plus,xval,val))))
    return symval(x)
end


#### Dump and DumpHold

@sjdoc Dump """
    Dump(expr)

print an internal representation of `expr`.

`Dump` is similar to Julia `dump`.
"""

@sjdoc DumpHold """
    DumpHold(expr)

print an internal representation of `expr`.

`DumpHold` is similar to Julia `dump`. In contrast to `Dump`, `expr` is not evaluated before its internal
representation is printed.
"""

@sjseealso_group(Dump,DumpHold)

# DumpHold does not evaluate args before dumping
apprules(mx::T) where {T<:Union{Mxpr{:Dump},Mxpr{:DumpHold}}} = for a in margs(mx) is_SJSym(a) ? dump(getssym(a)) : dump(a) end

## TODO: completely redesign this
### Contexts

@mkapprule Contexts  :nargs => 0:1

@sjdoc Contexts """
    Contexts()

return a `List` of all contexts.
"""

@doap Contexts() = tolist(getcontexts())
#     mxpr(:List, getcontexts()...)
# end


@mkapprule ContextSymbols  :nargs => 1

@sjdoc ContextsSymbols """
    Contexts(context)

return a `List` of all symbols in `context`.
"""

@doap ContextSymbols(s) = tolist(getsymbolsincontext(s))
#     mxpr(:List, getsymbolsincontext(s)...)
# end

### UpSet

@sjdoc UpSet """
    UpSet(a(g(x_)),b),  a(g(x_)) ^= b

associate the transformation rule with `g`.
"""

@mkapprule UpSet nargs => 1:Inf
@doap UpSet(lhs,rhs) = upset(mx,lhs,rhs)

## Note, mx is needed here because the entire expression is recorded for the definition of lhs
function upset(mx,lhs::Mxpr, rhs)
    _upset(mx,lhs,rhs)
    return rhs
end

function _upset_one(mx,m::SJSym,rule)
    warncheckprotect(m)
    set_upvalue(mx, m,rule)
end

_upset_one(mx,m::Mxpr,rule) = _upset_one(mx,mhead(m),rule)

_upset_one(args...) = nothing

function _upset(mx,lhs::Mxpr, rhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    foreach( m -> _upset_one(mx,m,rule), lhs)
end

@mkapprule UpSetDelayed nargs => 1:Inf
@doap UpSetDelayed(lhs,rhs) = upsetdelayed(mx,lhs,rhs)

## I think the only difference with UpSet again, is we don't return the rhs
function upsetdelayed(mx,lhs::Mxpr, rhs)
    _upset(mx,lhs,rhs)
    return Null
end

function do_Set(mx::Mxpr{:Set},lhs::Mxpr, rhs)
    checkprotect(lhs)
    unsetfixed(rhs)  # This allows f(3) to return 9, rather than 3^2 with f(x_) = x^2
                     # Not sure if this is the correct solution
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    set_downvalue(mx, mhead(lhs),rule) # push DownValue
    rhs # Set always returns the rhs. This is checked
end

### Symbol

@sjdoc Symbol """
    Symbol(str)

converts the string `str` to a symbol. For example if `a` is `1`,
then Symbol("a") returns `1`.
"""

@mkapprule Symbol :nargs => 1

# function apprules(mx::Mxpr{:Symbol})
#     dosymbol(mx,mx[1])
# end

@doap Symbol(s::String) = getsym(Symbol(s))
@doap Symbol(x) = (symwarn("Symbol: expected a string"); mx)

#### Clear

@sjdoc Clear """
    Clear(x,y,z,...)

remove the values associated with `x,y,z,...`. It does not remove
their `DownValues`.

    Clear(Out)

delete all of the saved Output lines. It actually replaces them with
the value `Null`.
"""

@sjseealso_group(Clear, ClearAll)

# 'Clear' a value. ie. set symbol's value to its name
## NOTE: Mma 1) allows 0 or more args with no message.
##           2) complains about non-symbols, but does not abort, ie. will clear remaining symbols
##           3) the symbol can be given as a string
##           4) always returns Null

@mkapprule Clear  nodefault => true

@doap Clear(args...) = (foreach(_clear, args); Null)

_clear(x) = Null

function _clear(ins::SymString)
    s = Symbol(ins)
    if s == :Out
        clear_all_output()
        return
    end
    checkprotect(s)
    clear_downvalues(s)
    clear_upvalues(s)
    clear_ownvalue_definition(s)
    setsymval(s,s)        # May 2016. This works ?
end

### ClearAll

@sjdoc ClearAll """
    ClearAll(x,y,z,...)

remove all values and `DownValues` associated with `x,y,z`.

The symbols are removed from the symbol table and will not appear in the list returned
by `UserSyms()`.

`Apply(ClearAll, UserSyms())` clears all user symbols.
"""

# FIXME. remove SymPy properties from symbol
# Remove all values associate with SJSym. values and DownValues
function apprules(mx::Mxpr{:ClearAll})  # already threaded
    for a in margs(mx)
        b = Symbol(a)
        checkprotect(b)
        delete_sym(b)
        remove_completion_symbols(b)
    end
end

### Unique

@sjdoc Unique """
    Unique()

creates a unique symbol.
"""

@mkapprule Unique :nargs => 0:1

@doap Unique() = (s = gensym(); setsymval(s,s); s)

### SymbolName

@sjdoc SymbolName """
    SymbolName(symbol)

returns the name of `symbol` as a string.
"""
@mkapprule SymbolName :nargs => 1
@doap SymbolName(x::SJSym) = string(x)

### DownValues

@sjdoc DownValues """
    DownValues(s)

return a `List` of `DownValues` associated with symbol `s`. These are values
that are typically set with the declarative "function definition".

For example `f(x_) := x` sets a `DownValue` for `f`.
"""

@sjexamp( DownValues,
         ("ClearAll(f)",""),
         ("f(x_) := x^2",""),
         ("DownValues(f)", "[HoldPattern(f(x_))->(x^2)]"))

## TODO. implement options
@mkapprule DownValues :nargs => 1
@doap DownValues(s::SJSymbol) = mxpr(:List,downvalues(s)...)

### UpValues

@sjdoc UpValues """
    UpValues(s)

returns a List of UpValues associated with symbol `s`. These are values
that are typically set with `UpSet`.
"""
@mkapprule UpValues
@doap UpValues(s::SJSymbol) = mxpr(:List,upvalues(s)...)

#### HAge, FixedQ and UnFix

@sjdoc HAge """
    HAge(s)

return the timestamp for the expression or symbol `s`.
Using this timestamp to avoid unnecessary evaluation is a partially
implemented feature.
"""

@sjseealso_group(HAge,Age,FixedQ,Syms,DirtyQ,Unfix)
# Get the last-altered timestamp of an expression or symbol
apprules(mx::Mxpr{:HAge}) = hdo_getage(mx,mx[1])
hdo_getage(mx,s::SJSym) = Int(symage(s))
hdo_getage(mx,s::Mxpr) = Int(getage(s))
hdo_getage(mx,x) = mx

apprules(mx::Mxpr{:Age}) = do_getage(mx,mx[1])
do_getage(mx,s::SJSym) = Int(symage(s))
do_getage(mx,s::Mxpr) = Int(getage(s))
do_getage(mx,x) = mx

@sjdoc FixedQ """
    FixedQ(expr)

returns the status of the fixed point bit, which tells whether `expr`
is expected to evaluate to itself in the current environment. This is partially
implemented.
"""

# Get fixed-point bit. Idea is to set it if expr evaluates to itself.
apprules(mx::Mxpr{:FixedQ}) = is_fixed(mx[1])

@sjdoc Unfix """
    Unfix(expr)

unsets the fixed flag on `expr`, causing it to be evaluated.
This is a workaround for bugs that cause an expression to be marked fixed
before it is completely evaluated.
"""
@mkapprule Unfix  nodefault => true  options => Dict( :Deep => false )

function do_Unfix(mx,expr::Mxpr; Deep=false)
#    (deep,val) = kws[1]
    Deep ? deepunsetfixed(expr) : unsetfixed(expr)
    unsetfixed(expr)
    expr
end

#do_Unfix(mx,args...) = mx
do_Unfix(mx,args...;kws...) = mx

# function apprules(mx::Mxpr{:Unfix})
#     unsetfixed(mx[1])
#     mx[1]
# end

### Syms

@sjdoc Syms """
    Syms(m)

return a `List` of the symbols that the expression `m` "depends" on. The
list is wrapped in `HoldForm` in order to prevent evaluation of the symbols.
"""

function apprules(mx::Mxpr{:Syms})
    mxpr(:HoldForm, do_syms(mx[1]))
end

do_syms(mx::Mxpr) = mxpr(:List, listsyms(mx)...)
do_syms(s) = mxpr(:List,)

### BuiltIns

@sjdoc BuiltIns """
    BuiltIns()

return a `List` of all "builtin" symbols. These are in fact all symbols that
have the `Protected` attribute.
"""

## This does not cause infinite evaluation loop.
#apprules(mx::Mxpr{:BuiltIns}) = protectedsymbols()

function apprules(mx::Mxpr{:BuiltIns})
    syms = get_system_symbols();
    deleteat!(syms,findfirst(syms,:ans))  ## These two lines avoid infinite evaluation loop
    deleteat!(syms,find((x -> match(r"^O+$",string(x)) !== nothing),syms))
    nargs = newargs(length(syms))
    copyto!(nargs, syms)
    setfixed(MListA(nargs))
end

@sjdoc UserSyms """
    UserSyms()

return a `List` of symbols that have not been imported from the `System` namespace.
This is all user defined symbols (unless you have imported symbols from elsewhere).
"""

@mkapprule UserSyms  :nargs => 0

@doap UserSyms() = tolistfixed(usersymbols())

@sjdoc CurrentContext """
    CurrentContext()

return the name of the current context.
"""

@mkapprule CurrentContext :nargs => 0

# This does not return a context or module type, because we need to
# keep types out of the language as much as possible. Everything is
# an expression! In fact, we should probably return a string.

@doap CurrentContext() = string(get_current_context_name())
