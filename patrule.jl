# Pattern matching and rules

# pieces of expressions that we operate on are Symbols and expressions
typealias ExSym Union(Expr,Symbol)

# This is not used at all. We only construct expressions
# with pvars, but never evaluate them
# pattern capture variable
# type pvar
#     name::Symbol  # name
#     cond::ExSym   # condition for matching. DataType, function...
# end

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
ispvar(x) = isexpr(x) && length(x.args) > 1 && x.args[1] == :pvar
pvarsym(pat) = pat.args[2]
pvarcond(pat) = pat.args[3]
setpvarcond(pat,cond) = pat.args[3] = cond

# high-level pattern match and capture
function cmppat1(ex,pat::ExSym)
    pat = ustopat(pat)   # convert underscore vars to pat()'s
    capt = Array(Any,0)  # allocate capture array
    success_flag = _cmppat(ex,pat,capt) # do the matching
    return (success_flag,capt)  # report whether matched, and return captures
end

# pattern vars are exactly those ending with '_'
ispvarsym(x) = string(x)[end] == '_'

# convert var_ to pat(var,None), else pass through
ustopat(sym::Symbol) = ispvarsym(sym) ? :(pvar($sym,None)) : sym

# conditions are signaled by expression pat_::cond
# Construct pvar() if we have this kind of expression.
# Else it is an ordinary expression and we walk it.
function ustopat(ex::Expr)
    if ex.head == :(::) && length(ex.args) > 0 &&
        typeof(ex.args[1]) == Symbol && ispvarsym(ex.args[1])
        ea1 = ex.args[1]
        ea2 = ex.args[2]
        return :(pvar($ea1,$ea2))
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
    cd[pvarsym(pat)] = cap
end

# retrieve captured expression by caption var name
function retrivecapt(pat,cd)
    cd[pvarsym(pat)]
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
# Only matching DataType and anonymous functions are implemented
function matchpat(cvar,ex)
    c = pvarcond(cvar)
    c == :None && return true # no condition
    typeof(c) == DataType && return typeof(ex) <: c  # NOTE: We use <: !
    if isexpr(c)
        if c.head == :->  # anon function
            f = eval(c)
# Replacing expression with compiled anonymous function does not work.            .
            setpvarcond(cvar,f)  
            return f(ex)
        end
    end
    if typeof(c) == Function
        return c(ex)
    end
    ce = evalcond(c) # punt and try eval
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
    if ispvar(pat) && matchpat(pat,ex)
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

# Same as patrule, except if match fails, return original expression
function tpatrule(ex,pat1,pat2)
    res = patrule(ex,pat1,pat2)
    res == false ? ex : res
end

# apply replacement rule r to expression ex
replace(ex::ExSym, r::PRule) = tpatrule(ex,r.lhs,r.rhs)

# Do depth-first replacement applying the same rule to each subexpression
function replaceall(ex,pat1,pat2)
    if isexpr(ex)
        ex = Expr(ex.head, ex.args[1],
                  map((x)->replaceall(x,pat1,pat2),ex.args[2:end])...)
    end
    # we have applied replacement at all lower levels. Now do current level.
    res = patrule(ex,pat1,pat2)
    res == false && return ex # match failed; return unaltered expression
    res
end

# same as above, but patterns are wrapped in a rule
replaceall(ex::ExSym, r::PRule) = replaceall(ex,r.lhs,r.rhs)

# Apply an array of rules. each subexpression is tested.
# Continue after first match for each expression.
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

# Do the substitution.
# pat is the template pattern: an expression with some pattern vars.
# cd is a Dict with pattern var names as keys and expressions as vals.
# Subsitute the pattern vars in pat with the vals from cd.
function patsubst!(pat,cd)
    if isexpr(pat) && ! ispvar(pat)
        pa = pat.args
        for i in 1:length(pa)
            if ispvar(pa[i])
                pa[i] = retrivecapt(pa[i],cd)
            elseif isexpr(pa[i])
                patsubst!(pa[i],cd)
            end
        end
    elseif ispvar(pat)
        pat = retrivecapt(pat,cd)
    end
    return pat
end

true
