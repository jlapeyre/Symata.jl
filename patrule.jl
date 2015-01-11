# Pattern matching and rules

type pat
    name::Symbol
end

typealias ExSym Union(Expr,Symbol)

type PRule
    lhs::ExSym
    rhs::ExSym
end

==(a::PRule, b::PRule) =  (a.lhs == b.lhs && a.rhs == b.rhs)

PRule(lhs::String, rhs::String) = PRule(parse(lhs),parse(rhs))
prule(x,y) = PRule(x,y)

=>(lhs::ExSym,rhs::ExSym) = prule(lhs,rhs)

isexpr(x) = (typeof(x) == Expr)
ispat(x) = isexpr(x) && length(x.args) > 1 && x.args[1] == :pat
patsym(pat) = pat.args[2]

function cmppat1(ex,pat::ExSym)
    pat = ustopat(pat)
    capt = Array(Any,0)
    res = _cmppat(ex,pat,capt)
    return (res,capt)
end

#cmppat1(ex::String, pat::String) = cmppat(parse(ex),parse(pat))
#cmppat1(ex::Expr, pat::String) = cmppat(ex,parse(pat))
#cmppat1(ex::String, pat::Expr) = cmppat(parse(ex),pat)

ispatsym(x::Symbol) = string(x)[end] == '_'

# fix: return type depends on value
function ustopat(sym::Symbol)
    s= string(sym)
    if s[end] == '_'
        return Expr(:call, :pat, (symbol(s[1:end-1])))
    end
    return sym
end

function ustopat(ex::Expr)
    Expr(ex.head, map(ustopat,ex.args)...)
end    

ustopat(x) = x

# check if one pattern var captured different things.
function cmppat(ex,pat)
    (res,capt) = cmppat1(ex,pat)
    res == false && return (res,capt)
    cd = Dict{Any,Any}()
    for (p,c) in capt
        v = get(cd,p,nothing)
        if v == nothing
            cd[p] = c
        else
            v != c && return (false,capt)
        end
    end
    return (true,capt)
end

# TODO: fix type instability
function _cmppat(ex,pat,capt)
    if ispat(pat)
        push!(capt,(patsym(pat),ex))
        return true
    end
    !isexpr(ex)  && return ex == pat
    if !isexpr(pat) || pat.head != ex.head ||
        length(pat.args) != length(ex.args)
        return false
    end
    for i in 1:length(ex.args)
         _cmppat(ex.args[i],pat.args[i],capt) == false && return false
    end
    return true
end

function patrule(ex,pat1,pat2)
    (res,capt) = cmppat(ex,pat1)
    res == false && return false
    npat = ustopat(pat2) # deep copy and x_ -> pat(x)
    cd = Dict{Any,Any}()    
    for (p,c) in capt
        cd[p] = c
    end
    nnpat = patsubst!(npat,cd)
    nnpat
end
patrule(ex,pat1,pat2::String) = patrule(ex,pat1,parse(pat2))

function tpatrule(ex,pat1,pat2)
    res = patrule(ex,pat1,pat2)
    res == false ? ex : res
end

replace(ex::ExSym, r::PRule) = tpatrule(ex,r.lhs,r.rhs)

function replaceall(ex,pat1,pat2)
    if isexpr(ex)
        ex = Expr(ex.head, ex.args[1],
                  map((x)->replaceall(x,pat1,pat2),ex.args[2:end])...)
    end
    res = patrule(ex,pat1,pat2)
    res == false && return ex
    res
end

replaceall(ex::ExSym, r::PRule) = replaceall(ex,r.lhs,r.rhs)

function replaceall(ex,rules::Array{PRule,1})
    if isexpr(ex)
        ex = Expr(ex.head, ex.args[1],
                  map((x)->replaceall(x,rules),ex.args[2:end])...)
    end
    local res
    for r in rules
        res = patrule(ex,r.lhs,r.rhs)
        res != false && return res
    end
    ex
end

function patsubst!(pat,cd)
    if isexpr(pat) && ! ispat(pat)
        pa = pat.args
        for i in 1:length(pa)
            if ispat(pa[i])
                pa[i] = cd[patsym(pa[i])]
            elseif isexpr(pa[i])
                patsubst!(pa[i],cd)
            end
        end
    elseif ispat(pat)
        pat = cd[patsym(pat)]
    end
    return pat
end

####################

PRULES = Dict{Any,Any}()

PRULES[:sq1] = prule("x_ * x_", "x_^2")
PRULES[:sq2] = prule("x_^2 * x_", "x_^3")


true
