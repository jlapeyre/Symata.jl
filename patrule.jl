# Pattern matching and rules

# pieces of expressions that we operate on are Symbols and expressions
typealias ExSym Union(Expr,Symbol)

# pattern capture variable
type pat
    name::Symbol  # name
    cond::ExSym   # condition for matching. DataType, function...
end

# replacement rule
# lhs is a pattern for matching
# rhs is a template pattern for replacing.
type PRule
    lhs::ExSym
    rhs::ExSym
end

==(a::PRule, b::PRule) =  (a.lhs == b.lhs && a.rhs == b.rhs)

prule(x,y) = PRule(x,y)

# syntax for creating a rule. collides with Dict syntax sometimes.
=>(lhs::ExSym,rhs::ExSym) = prule(lhs,rhs)

isexpr(x) = (typeof(x) == Expr)

# These operate on the expression for a pattern capture variable.
# ie.  :( pat(sym,cond) )
# the head is :call, but we don't check for that here.
ispat(x) = isexpr(x) && length(x.args) > 1 && x.args[1] == :pat
patsym(pat) = pat.args[2]
patcond(pat) = pat.args[3]

# high-level pattern match and capture
function cmppat1(ex,pat::ExSym)
    pat = ustopat(pat)   # convert underscore vars to pat()'s
    capt = Array(Any,0)  # allocate capture array
    success_flag = _cmppat(ex,pat,capt) # do the matching
    return (success_flag,capt)  # report whether matched, and return captures
end

# pattern vars are exactly those ending with '_'
ispatsym(x) = string(x)[end] == '_'

# convert var_ to pat(var,None), else pass through
ustopat(sym::Symbol) = ispatsym(sym) ? :(pat($sym,None)) : sym

# conditions are signaled by expression pat_::cond
# Construct pat() if we have this kind of expression.
# Else it is an ordinary expression and we walk it.
function ustopat(ex::Expr)
    if ex.head == :(::) && length(ex.args) > 0 &&
        typeof(ex.args[1]) == Symbol && ispatsym(ex.args[1])
        ea1 = ex.args[1]
        ea2 = ex.args[2]
        return :(pat($ea1,$ea2))
    end
    Expr(ex.head, map(ustopat,ex.args)...)
end

# everything else falls through
ustopat(x) = x

# Perform match and capture.
# and check consistency of assigne capture variables
function cmppat(ex,pat)
    (res,capt) = cmppat1(ex,pat)
    res == false && return (res,capt) # match failed
    cd = Dict{Any,Any}()
    for (p,c) in capt
        v = get(cd,p,nothing)
        if v == nothing
            cd[p] = c
        else
            v != c && return (false,capt) # one named var captured two different things
        end
    end
    return (true,capt)
end

# push onto array as we capture expressions
function capturepat(capt,pat,ex)
    push!(capt,(pat,ex))
end

# store captured expression in Dict. Here only the capture var name
function storecapt(pat,cap,cd)
    cd[patsym(pat)] = cap
end

# retrieve captured expression by caption var name
function retrivecapt(pat,cd)
    cd[patsym(pat)]
end

# if we don't know what the condition is, try to evalute it.
# slow.
function evalcond(c)
    res = try
        eval(c)
    catch
        false
    end
    return res
end

# check if conditions on capture var cvar are satisfied by ex
# No condition is signaled by None
# Only matching DataType is implemented
function matchpat(cvar,ex)
    c = patcond(cvar)
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

# Descend expression tree. If there is no pattern var in
# a subexpression of pattern `pat', then the subexpressions in
# ex and pat must match exactly.
# If pat is a capture var, then it matches the subexpression ex,
# if the condition as checked by matchpat is satisfied.
function _cmppat(ex,pat,capt)
    if ispat(pat) && matchpat(pat,ex)
        capturepat(capt,pat,ex)
        return true
    end
    !isexpr(ex)  && return ex == pat # 'leaf' on the tree. Must match exactly
    if !isexpr(pat) || pat.head != ex.head ||
        length(pat.args) != length(ex.args)
        return false
    end
    for i in 1:length(ex.args) # match and capture subexpressions.
         _cmppat(ex.args[i],pat.args[i],capt) == false && return false
    end
    return true
end

# match and capture on ex with pattern pat1.
# Replace pattern vars in pat2 with expressions captured from
# ex.
function patrule(ex,pat1,pat2)
    (res,capt) = cmppat(ex,pat1)
    res == false && return false # match failed
    npat = ustopat(pat2) # deep copy and x_ -> pat(x)
    cd = Dict{Any,Any}()   
    for (p,c) in capt
        storecapt(p,c,cd) # throw away condition information
    end
    nnpat = patsubst!(npat,cd) # do replacement
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

true
