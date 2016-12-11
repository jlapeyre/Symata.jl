const _attributes_string =
"""
Protected
Orderless
Flat
OneIdentity
Listable
Constant
NumericFunction
Locked
ReadProtected
HoldFirst
HoldRest
HoldAll
HoldAllComplete
NHoldFirst
NHoldRest
NHoldAll
SequenceHold
Temporary
Stub
"""

const possible_attributes = map(Symbol,split(_attributes_string))

# For Heads that are not symbols
get_attribute(args...) = false

# Return true if sj has attribute attr
function get_attribute(sj::SJSymbol, attr::Symbol)
    get(getssym(sj).attr,attr,false)
end

_set_attribute(s,attr) = getssym(Symbol(s)).attr[Symbol(attr)] = true


symattr(s::SJSymbol) = getssym(s).attr
get_attributes(sj::SymString) = ( ks = sort!(collect(Any, keys(symattr(Symbol(sj))))) )

## We will get rid of Qsym
set_attribute(sj::Qsym, attr::Symbol) = (getssym(sj).attr[attr] = true)

"""
    unset_attribute(sj::SJSymbol, attr::Symbol)

unset a single attribute `attr` for `sj`.
"""
unset_attribute(sj::SJSymbol, attr::Symbol) = delete!(getssym(sj).attr, attr)

"""
    clear_attributes(sja::SymString...)

clear all attributes set for the symbols `sja`.
"""
clear_attributes(sja::SymString...) =  foreach( x -> empty!(getssym(x).attr), sja)

## to be removed
clear_attributes(sj::Qsym) =  empty!(getssym(sj).attr)

### All access to data structures is above this line, or in mxpr.jl
### i.e. the interface is above this line

# Return true if head of mx has attribute attr
function get_attribute{T}(mx::Mxpr{T}, attr::Symbol)
    get_attribute(T,attr)
end

## need a version that warns for users and asserts for developing ?
function set_attribute1(sj::SymString, attr::SymString)
    (ssj, sattr) = (Symbol(sj),Symbol(attr))
    if sattr in possible_attributes
        _set_attribute(sj,attr)
        return nothing
    end
    symwarn("'$attr' is not an attribute")
    return nothing
end

"""
    set_attribute(s::SymString,attr::SymString)

set the `attr` attribute for `s`. Arguments may be `Symbol` or `String`.
"""
set_attribute(s::SymString,attr::SymString)= set_attribute1(Symbol(s),Symbol(attr))
set_attribute(sa::AbstractArray, attr::SymString) = foreach( x -> set_attribute1(x,attr), sa)
set_attribute(s::SymString, attra::AbstractArray) = foreach( x -> set_attribute1(s,x), attra)
set_attribute(sa::AbstractArray, attra::AbstractArray) = foreach( x -> set_attribute(sa,x), attra)

"""
    set_pattribute(s::SymString, attr::SymString)

set the attribute `attr` for symbol `s`. Also set the `Protected` attribute.
"""
function set_pattribute(s::SymString, attr::SymString)
    protect(s)
    set_attribute(s,attr)
end

"""
    set_pattribute(sa::AbstractArray, attr::SymString)

call `set_pattribute` for each of the elements in `sa`.
"""
set_pattribute(sa::AbstractArray, attr::SymString) = foreach( x -> set_pattribute(x,attr), sa)


"""
    set_pattribute(sa::AbstractArray, attr::SymString)

call `set_pattribute` for each of the elements in `attra`.
"""
function set_pattribute(s::SymString, attra::AbstractArray)
    protect(s)
    set_attribute(s,attra)
end


"""
    set_pattribute(sa::AbstractArray, attra::AbstractArray)

set all of the attributes in `attra` for all of the symbols in `sa`. Also
set the attribute `Protected` for each of the `sa`.
"""
function set_pattribute(sa::AbstractArray, attra::AbstractArray)
    foreach(protect, sa)
    set_attribute(sa,attra)
end

"""
    set_sysattributes(sym, attr)

register `sym` as a system symbol and set attributes. Attribute
`Protected` is set even it is not specified. Both `sym` and `attr`
can a strings or a symbol or arrays of these. In case both are arrays,
all symbols get all attributes.
"""
function set_sysattributes(sym, attr)
    register_system_symbol(sym)
    set_pattribute(sym,attr)
end

function set_sysattributes(syms::AbstractArray, attr)
    foreach(register_system_symbol, syms)
    set_pattribute(syms,attr)
end

function set_sysattributes(sym)
    register_system_symbol(sym)
    protect(sym)
end

function set_sysattributes(syms::AbstractArray)
    foreach(register_system_symbol, sym)
    foreach(protect,sym)
end

function protectedsymbols_strings()
    symstrings = Array(Compat.String,0)
    for s in keys(CurrentContext.symtab)
        if get_attribute(s,:Protected) && s != :ans
            push!(symstrings,string(getsym(s))) end
    end
    sort!(symstrings)
end

function protectedsymbols()
    args = newargs()
    for s in keys(CurrentContext.symtab)
        if get_attribute(s,:Protected) && s != :ans
            push!(args,getsym(s)) end
    end
    mx = mxpra(:List, sort!(args))
end

# Related code in predicates.jl and attributes.jl
unprotect(sj::SJSym) = unset_attribute(sj,:Protected)

protect(sj::SymString) = set_attribute(sj,:Protected)

## hmmm, we could do this for all attributes
for s in (:HoldFirst,:HoldAll,:HoldRest,:HoldAllComplete, :SequenceHold, :Flat, :Orderless,
          :Listable, :Protected, :Constant)
    sf = Symbol("is",s)
    @eval ($sf)(x) = get_attribute(x,$(QuoteNode(s)))
end
