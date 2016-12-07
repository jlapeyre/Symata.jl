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

## need a version that warns for users and asserts for developing ?
function _set_attribute(sj::SymString, attr::SymString)
    (ssj, sattr) = (Symbol(sj),Symbol(attr))
    if sattr in possible_attributes
        getssym(Symbol(sj)).attr[Symbol(attr)] = true
        return nothing
    end
    symwarn("'$attr' is not an attribute")
    return nothing
end

"""
    set_attribute(s::SymString,attr::SymString)

set the `attr` attribute for `s`. Arguments may be `Symbol` or `String`.
"""
set_attribute(s::SymString,attr::SymString)= _set_attribute(Symbol(s),Symbol(attr))
set_attribute(sa::AbstractArray, attr::SymString) = foreach( x -> _set_attribute(x,attr), sa)
set_attribute(s::SymString, attra::AbstractArray) = foreach( x -> _set_attribute(s,x), attra)
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


# Better to delete the symbol
#unset_attribute(sj::SJSym, attr::Symbol) = (getssym(sj).attr[attr] = false)

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
