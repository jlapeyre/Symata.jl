# Pattern matching and rules

# pieces of expressions that we operate on are Symbols and expressions
typealias ExSym Union(Expr,Symbol)

# pattern capture variable
type pat
    name::Symbol  # name
    cond::ExSym   # condition for matching. Data
end

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
patcond(pat) = pat.args[3]

function cmppat1(ex,pat::ExSym)
    pat = ustopat(pat)
    capt = Array(Any,0)
    res = _cmppat(ex,pat,capt)
    return (res,capt)
end


ispatsym(x) = string(x)[end] == '_'
ustopat(sym::Symbol) = ispatsym(sym) ? :(pat($sym,None)) : sym

function ustopat(ex::Expr)
    if ex.head == :(::) && length(ex.args) > 0 &&
        typeof(ex.args[1]) == Symbol && ispatsym(ex.args[1])
        ea1 = ex.args[1]
        ea2 = ex.args[2]
        return :(pat($ea1,$ea2))
    end
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

function capturepat(capt,pat,ex)
    push!(capt,(pat,ex))
end

function storecapt(pat,cap,cd)
    cd[patsym(pat)] = cap
end

function retrivecapt(pat,cd)
    cd[patsym(pat)]
end

function evalcond(c)
    res = try
        eval(c)
    catch
        false
    end
    return res
end

function matchpat(pat,ex)
    c = patcond(pat)
    c == :None && return true
    if typeof(c) == DataType
        if typeof(ex) <: c
            return true
        else
            return false
        end
    end
    ce = evalcond(c)
    ce == false && return false # maybe true here ?!
    if typeof(ce) == DataType && !(typeof(ex) <: ce)
        return false
    end
    true
end

function _cmppat(ex,pat,capt)
    if ispat(pat) && matchpat(pat,ex)
        capturepat(capt,pat,ex)
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
        storecapt(p,c,cd)
#        cd[p] = c
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
                ith = pa[i]
                pa[i] = retrivecapt(pa[i],cd)
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
