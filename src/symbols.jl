# NB. attributes for builtin symbols are mostly set in protected_symbols.jl

# Check if we are trying to add to a symbol bound to itself. If
# so, we warn and return the unevaluated expression. This avoids
# an infinite evaluation loop. This is what Mma does.
macro checkunbound(mx,x,sv)
    esc( quote
           $sv = symval($x)
           if $sv == $x
             stwarn("The symbol " * string($x) * " does not have a value, so it's value cannot be changed")
             setfixed($mx)
             return $mx
          end
    end)
end

function checkprotect(s::Qsym)
    get_attribute(s,:Protected) &&
    error("Symbol '",s, "' is protected.")
end

function checkprotect(s::SJSym)
    get_attribute(symname(s),:Protected) &&
    error("Symbol '",symname(s), "' is protected.")
end
checkprotect(mx::Mxpr) = checkprotect(mhead(mx))

function warncheckprotect(s::SJSym)
    if get_attribute(symname(s),:Protected)
        stwarn(string("Symbol '",symname(s), "' is protected."))
        return false
    else
        return true
    end
end
warncheckprotect(mx::Mxpr) = warncheckprotect(mhead(mx))

#### Attributes

@sjdoc Attributes "
Attributes(s) returns attributes associated with symbol s. Builtin symbols have
the attribute Protected, and may have others, including HoldFirst, SequenceHold,
HoldRest, HoldAll, ReadProtected, Constant, Locked, Flat, Listable, NumericFunction,
OneIdentity.
"
apprules(mx::Mxpr{:Attributes}) = get_attributesList(mx[1])

#get_attributes(sj::SJSym) = ( ks = sort!(collect(Any, keys(symattr(sj)))) )
get_attributes(sj::SJSymbol) = ( ks = sort!(collect(Any, keys(symattr(sj)))) )


get_attributes(s::AbstractString) = get_attributes(Symbol(s))
get_attributesList(s::AbstractString) = get_attributesList(Symbol(s))

function get_attributesList(sj::SJSymbol)
    ks = get_attributes(sj)
    mxpr(:List,ks...) # need to splat because array ks is not of type Any
end

#### SetAttributes

@mkapprule SetAttributes :nargs => 2

@sjdoc SetAttributes "
SetAttributes(sym,attr) adds attr to the list of attributes for sym.
SetAttributes(list,attr) adds attr to the list of attributes for each symbol in list.
SetAttributes(sym,list) adds each attribute in list  to the list of attributes for sym.
"

function check_set_attributes(sym::SJSymbol, attr::SJSym)
    checkprotect(sym)
    set_attribute(sym,attr)
    Null
end

set_attributes(sym::SJSymbol, attr::SJSym) = check_set_attributes(sym,attr)

function set_attributes{T<:Array}(sym::SJSymbol, attrs::T)
    for a in attrs
        check_set_attributes(sym,a)
    end
end

function set_attributes{T<:Array}(syms::T, attr::SJSym)
    for a in syms
        check_set_attributes(a,attr)
    end
end

do_SetAttributes(mx::Mxpr{:SetAttributes}, sym::SJSymbol, attr::SJSym) = set_attributes(sym,attr)
do_SetAttributes(mx::Mxpr{:SetAttributes}, syms::Mxpr{:List}, attr::SJSym) = set_attributes(margs(syms),attr)
do_SetAttributes(mx::Mxpr{:SetAttributes}, sym::SJSymbol, attrs::Mxpr{:List}) = set_attributes(sym,margs(attrs))

#### Unprotect

###  Wolfram does this: Protect[a,b,c,....] returns a list of symbol names (strings) of those symbols which were not
###  protected before, but now are. Non-symbols are silently ignored. Symbols which were already protected are
###  silently ignored.

@sjdoc Unprotect "
Unprotect(z1,z2,...) removes the Protected attribute from the symbols z1, z2, ...
"

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
    if is_protected(a)
        unprotect(a)
        return string(a)
    end
    return false
end

do_unprotect(mx,a) = false

##### Protect

@sjdoc Protect "
Protect(z1,z2,...) adds the Protected attribute to the lists of attributes for the symbols z1, z2, ...
"

function apprules(mx::Mxpr{:Protect})
    nargs = newargs()
    for i in 1:length(mx)
        ret = do_protect(mx,mx[i])
        if ret != false
            push!(nargs, ret)
        end
    end
    mxprcf(:List, nargs)
end

function do_protect(mx,a::SJSym)
    if ! is_protected(a)
        protect(a)
        return string(a)
    end
    return false
end

do_protect(mx,a) = false

#### Set and SetDelayed

@sjdoc Set "
Set(a,b), a = b
Sets the value of a to b. b is evaluated only once, when `a=b' is evaluated.
obj[i,j,...] = val sets a part of obj to val. obj can be an Symata expression
or a Julia object, such as an Array or Dict.
"

@sjseealso_group( Set, SetDelayed, UpSet, DownValues, UpValues )

@sjexamp( Set,
         ("Clear(a,b,c)",""),
         ("b = a", "a"),
         ("a = 1", "1"),
         ("c = a", "1"),
         ("a = 2", "2"),
         ("b", "2"),
         ("c", "1"))

@sjdoc SetDelayed "
SetDelayed(a,b), a := b
Whenever a is evaluated, b is evaluated and the result is assigned to a.
So a is not set to the value of b at the time a := b is evaluated, but
rather to the current value of b every time a is evaluated.
"

# Set SJSym value.
# Set has HoldFirst, SetDelayed has HoldAll.

@mkapprule Set

function do_Set(mx::Mxpr{:Set})
    warn("Set called with 0 arguments; 1 or more arguments are expected.")
    setfixed(mx)
    mx
end

# This is what Mma does.
function do_Set(mx::Mxpr{:Set}, lhs::SJSym)
    checkprotect(lhs)
    rhs = mxprcf(:Sequence)
    setsymval(lhs,rhs)
    setdefinition(lhs, mx)
    rhs
end

apprules(mx::Mxpr{:SetDelayed}) = setdelayed(mx,mx[1],mx[2])

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
    Null
end

function do_Set(mx::Mxpr{:Set},lhs::SJSymbol, rhs)
    checkprotect(lhs)
    setsymval(lhs,rhs)
    setdefinition(lhs, mx)
    rhs
end

# function do_Set(mx::Mxpr{:Set},lhs::Qsym, rhs)
#     checkprotect(lhs)  #  FIXME
#     setsymval(lhs,rhs)
#     setdefinition(lhs, mx)
#     rhs
# end

# Create DownValue. "function" definition
# eg f(x_) := x  defines a DownValue for the SJSym f
function setdelayed(mx,lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    set_downvalue(mx,mhead(lhs),rule) # push DownValue
    rule
    Null
end

function do_Set(mx::Mxpr{:Set},lhs::Mxpr{:Part}, rhs::Mxpr{:Module})
    error("$mx is not implemented")
end

# Mma is not clear but seems to evaluate the first arg to the lhs (the expression
# whose part we want) exactly once. We should document what we do.
# We check is_Number several times, because we may have a Dict.
function do_Set(mx::Mxpr{:Set},lhs::Mxpr{:Part}, rhs)
    ex0 = meval(expr(lhs))  # evaluate once, eg, to get expr from symbol.
    tinds = inds(lhs)
    ex = ex0
    for j in 1:length(tinds)-1
        ind = doeval(tinds[j])
        ind = is_Number(ind) && ind < 0 ? length(ex)+ind+1 : ind
        ex = is_Number(ind) && ind == 0 ? mhead(ex) : ex[ind]
    end
    val = doeval(rhs)
    ind = doeval(tinds[end])
    ind = is_Number(ind) && ind < 0 ? length(ex)+ind+1 : ind
    if is_Number(ind) && ind == 0
        ex.head = val  #  TODO violation of abstraction
    else
        ex[ind] = val
    end
    unsetfixed(ex0) # maybe we can optimize this
    val
end

# Optimize a bit. Localize variables once, not every time pattern is evaluated
setdelayed(mx,lhs::Mxpr, rhs::Mxpr{:Module}) = setdelayed(mx,lhs,localize_module!(rhs))

#do_Set(mx::Mxpr{:Set},lhs::Mxpr, rhs::Mxpr{:Module}) = do_Set(mx,lhs,localize_module!(rhs))

@doap Set(lhs::Mxpr, rhs::Mxpr{:Module}) = do_Set(mx,lhs,localize_module!(rhs))

#### Contexts

@mkapprule Contexts  :nargs => 0:1

@sjdoc Contexts "
Contexts() returns a list of all contexts.
"

@doap function Contexts()
    mxpr(:List, getcontexts()...)
end


@mkapprule ContextSymbols  :nargs => 1

@sjdoc ContextsSymbols "
Contexts(context) returns a list of all symbols in context.
"

@doap function ContextSymbols(s)
    mxpr(:List, getsymbolsincontext(s)...)
end


#### UpSet

@sjdoc UpSet "
UpSet(a(g(x_)),b), or a(g(x_)) ^= b  associates the transformation rule with g.
"

apprules(mx::Mxpr{:UpSet}) = upset(mx,mx[1],mx[2])

function upset(mx,lhs::Mxpr, rhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    for i in 1:length(lhs)
        m = lhs[i]
        if is_Mxpr(m) && warncheckprotect(m)
            set_upvalue(mx,mhead(m),rule)
        elseif is_SJSym(m) && warncheckprotect(m)
            set_upvalue(mx, m,rule)
        end
    end
    return rhs
end

apprules(mx::Mxpr{:UpSetDelayed}) = upsetdelayed(mx,mx[1],mx[2])

# I think the only difference with UpSet again, is we don't return the
# rhs.
function upsetdelayed(mx,lhs::Mxpr, rhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    for i in 1:length(lhs)
        m = lhs[i]
        if is_Mxpr(m) && warncheckprotect(m)
            set_upvalue(mx,mhead(m),rule)
        elseif is_SJSym(m) && warncheckprotect(m)
            set_upvalue(mx, m,rule)
        end
    end
    Null
end


function do_Set(mx::Mxpr{:Set},lhs::Mxpr, rhs)
    checkprotect(lhs)
    unsetfixed(rhs)  # This allows f(3) to return 9, rather than 3^2 with f(x_) = x^2
                     # Not sure if this is the correct solution
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    set_downvalue(mx, mhead(lhs),rule) # push DownValue
    rhs # Set always returns the rhs. This is checked
end


#### Symbol

@sjdoc Symbol "
Symbol(str) converts the string str to a symbol. For example if a is 1,
then Symbol(\"a\") returns 1.
"

function apprules(mx::Mxpr{:Symbol})
    dosymbol(mx,mx[1])
end
dosymbol(mx,s::AbstractString) = getsym(Symbol(s))
dosymbol(mx,x) = (stwarn("Symbol: expected a string"); mx)

#### Clear

@sjdoc Clear "
Clear(x,y,z) removes the values associated with x,y,z. It does not remove
their DownValues.

Clear(Out) deletes all of the saved Output lines. It actually replaces them with
the value `Null'.
"

@sjseealso_group(Clear, ClearAll)

# 'Clear' a value. ie. set symbol's value to its name
function apprules(mx::Mxpr{:Clear})  # This will be threaded over anyway
    @inbounds for a in margs(mx)  # no inbounds does not work here
        a = typeof(a) <: AbstractString ?  Symbol(a) : a
        if a == :Out
            clear_all_output()
            return Null
        end
        checkprotect(a)
#        setsymval(a,symname(a))
        setsymval(a,a)        # May 2016. This works ?
    end
    Null
end

#### ClearAll

@sjdoc ClearAll "
ClearAll(x,y,z) removes all values and DownValues associated with x,y,z. The
symbols are removed from the symbol table and will not appear in the list returned
by UserSyms().
"

# FIXME. remove SymPy properties from symbol
# Remove all values associate with SJSym. values and DownValues
function apprules(mx::Mxpr{:ClearAll})  # already threaded
    for a in margs(mx)
        a = typeof(a) <: AbstractString ?  Symbol(a) : a
        checkprotect(a)
        delete_sym(a)
    end
end
