# NB. attributes for builtin symbols are mostly set in protected_symbols.jl

#### set_pattributes for internal use to set default attributes of builtin symbols

function set_pattributes{T<:AbstractString}(syms::Array{T,1},attrs::Array{Symbol,1})
    for s in syms
        ssym = symbol(s)
        clear_attributes(ssym)
        for a in attrs
            set_attribute(ssym,a)
        end
        set_attribute(ssym,:Protected)  # They all are Protected, But, we always include this explictly, as well.
        register_system_symbol(ssym)
    end
end

set_pattributes{T<:AbstractString}(sym::T,attrs::Array{Symbol,1}) = set_pattributes([sym],attrs)
set_pattributes{T<:AbstractString}(syms::Array{T,1},attr::Symbol) = set_pattributes(syms,[attr])
set_pattributes{T<:AbstractString}(sym::T,attr::Symbol) = set_pattributes([sym],[attr])
set_pattributes{T<:AbstractString}(sym::T) = set_pattributes([sym],Symbol[])
set_pattributes{T<:AbstractString}(syms::Array{T,1}) = set_pattributes(syms,Symbol[])

#### Attributes

@sjdoc Attributes "
Attributes(s) returns attributes associated with symbol s. Builtin symbols have
the attribute Protected, and may have others, including HoldFirst, SequenceHold,
HoldRest, HoldAll, ReadProtected, Constant, Locked, Flat, Listable, NumericFunction,
OneIdentity.
"
apprules(mx::Mxpr{:Attributes}) = get_attributesList(mx[1])

get_attributes(sj::SJSym) = ( ks = sort!(collect(Any, keys(symattr(sj)))) )

get_attributes(s::AbstractString) = get_attributes(symbol(s))
get_attributesList(s::AbstractString) = get_attributesList(symbol(s))

function get_attributesList(sj::SJSym)
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

function check_set_attributes(sym::SJSym, attr::SJSym)
    checkprotect(sym)
    set_attribute(sym,attr)
    Null
end

set_attributes(sym::SJSym, attr::SJSym) = check_set_attributes(sym,attr)

function set_attributes{T<:Array}(sym::SJSym, attrs::T)
    for a in attrs
        check_set_attributes(sym,a)
    end
end

function set_attributes{T<:Array}(syms::T, attr::SJSym)
    for a in syms
        check_set_attributes(a,attr)
    end
end

do_SetAttributes(mx::Mxpr{:SetAttributes}, sym::SJSym, attr::SJSym) = set_attributes(sym,attr)
do_SetAttributes(mx::Mxpr{:SetAttributes}, syms::Mxpr{:List}, attr::SJSym) = set_attributes(margs(syms),attr)
do_SetAttributes(mx::Mxpr{:SetAttributes}, sym::SJSym, attrs::Mxpr{:List}) = set_attributes(sym,margs(attrs))

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
