# Pattern matching and rules

# pieces of expressions that we operate on are Symbols and expressions
typealias ExSym Union(Expr,Symbol)
typealias CondT Union(Expr,Symbol,DataType,Function)

# Pattern variable. name is the name, ending in underscore cond is a
# condition that must be satisfied to match But, cond may be :All,
# which matches anything.  The most imporant feature is that the Pvar
# matches based on its context in an AST.
#
# Julia manual says the grab-bag data type is a sign
# of bad design!
type Pvar
    name::Symbol  # name
    cond::CondT   # condition for matching. DataType, function...
end
typealias ExSymPvar Union(Expr,Symbol,Pvar)

# we could allow non-underscored names
function Pvar(name::Symbol)
    ispvarsym(name) || error("Pvar: name '$name' does not end with '_'")
    Pvar(name,:All)
end

function pvar(name::Symbol, cond::CondT)
    ispvarsym(name) || error("pvar: name '$name' does not end with '_'")
    Pvar(name,cond)
end

pvar(name::Symbol) = pvar(name,:All)

==(a::Pvar, b::Pvar) = (a.name == b.name && a.cond == b.cond)

# ast is the pattern including Pvars for capture.
# cond is condition to apply to any Pvars in the pattern
type Pattern
    ast::Any
    cond::CondT
end

Pattern(ast::ExSymPvar) = Pattern(ast,:All)
pattern(ast::ExSym) = pattern(ast,:All)

# function pattern(ast::ExSym,cond::CondT)
#     Pattern(ustopat(ast),cond)
# end

pattern(x,cond::Symbol) = Pattern(ustopat(x),cond)

pattern(x) = pattern(x,:All)

# replacement rule
# lhs is a pattern for matching.
# rhs is a template pattern for replacing.
type PRule
    lhs::Pattern
    rhs::Pattern
end

PRule(lhs::ExSym, rhs::ExSym) = PRule(pattern(lhs),pattern(rhs))
==(a::PRule, b::PRule) =  (a.lhs == b.lhs && a.rhs == b.rhs)
==(a::Pattern, b::Pattern) = (a.ast == b.ast)
prule(x,y) = PRule(x,y)
# syntax for creating a rule. collides with Dict syntax sometimes.
=>(lhs::ExSym,rhs::ExSym) = prule(pattern(lhs),pattern(rhs))
=>(lhs::ExSym,rhs::Symbol) = prule(pattern(lhs),pattern(rhs))


# These operate on the expression for a pattern capture variable.
# ie.  :( pat(sym,cond) )
# the head is :call, but we don't check for that here.
ispvar(x) = typeof(x) == Pvar
pvarsym(pvar::Pvar) = pvar.name
pvarcond(pvar::Pvar) = pvar.cond
setpvarcond(pvar::Pvar,cond) = pvar.cond = cond

# high-level pattern match and capture
function cmppat1(ex,pat::Pattern)
    pat = ustopat(pat)   # convert underscore vars to pat()'s
    capt = Array(Any,0)  # allocate capture array
    success_flag = _cmppat(ex,pat.ast,capt) # do the matching
    return (success_flag,capt)  # report whether matched, and return captures
end
cmppat1(ex,pat::ExSym) = cmppat1(ex, pattern(pat))

# pattern vars are exactly those ending with '_'
ispvarsym(x) = string(x)[end] == '_'

# convert var_ to Pvar(var,:All), else pass through
ustopat(sym::Symbol) = ispvarsym(sym) ? Pvar(sym,:All) : sym

# Syntx for specifying condition is pat_::cond
# Construct pvar() if we have this kind of expression.
# Else it is an ordinary expression and we walk it.
# We eval the condition. It will be a DataType or a Function
function ustopat(ex::Expr)
    if ex.head == :(::) && length(ex.args) > 0 &&
        typeof(ex.args[1]) == Symbol && ispvarsym(ex.args[1])
        return pvar(ex.args[1],eval(ex.args[2]))
    end
    Expr(ex.head, map(ustopat,ex.args)...)
end

# everything else falls through
ustopat(x) = x

# Perform match and capture.
# Then check consistency of assigned capture variables
function cmppat(ex,pat::Pattern)
    (res,captures) = cmppat1(ex,pat)
    res == false && return (res,captures) # match failed
    cd = Dict{Symbol,Any}()
    for (pvar,capt) in captures
        pn = pvar.name
        v = get(cd,pn,nothing)
        if v == nothing
            cd[pn] = capt
        else
            v != capt && return (false,captures) # one named var captured two different things
        end
    end
    return (true,captures)
end

cmppat(ex,pat::ExSym) = cmppat(ex,pattern(pat))

# push onto array as we capture expressions
function capturepvar(capt,pat,ex)
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
    println("evaling condintion $c")
    res = try
        eval(c)
    catch
        false
    end
    return res
end

# check if conditions on capture var cvar are satisfied by ex
# No condition is signaled by :All
# Only matching DataType and anonymous functions are implemented
function matchpat(cvar,ex)
    c = pvarcond(cvar)
    c == :All && return true # no condition
    typeof(c) == DataType && return typeof(ex) <: c  # NOTE: We use <: !
    if isexpr(c)
        if c.head == :->  # anon function
            println("Got a function expressoin")            
            f = eval(c)
# Replacing expression with compiled anonymous function does not work.            .
            setpvarcond(cvar,f)  
            return f(ex)
        end
    end
    if typeof(c) == Function
#        println("Got a real function")
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
        capturepvar(capt,pat,ex)
        return true
    end
    !isexpr(ex)  && return ex == pat # 'leaf' on the tree. Must match exactly.
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
function patrule(ex,pat1::Pattern,pat2::Pattern)
    (res,capt) = cmppat(ex,pat1)
    res == false && return false # match failed
#    npat = ustopat(pat2) # deep copy and x_ -> pat(x)
    npat = deepcopy(pat2) # deep copy and x_ -> pat(x)    
    cd = Dict{Any,Any}()   
    for (p,c) in capt
        storecapt(p,c,cd) # throw away condition information
    end
    nnpat = patsubst!(npat.ast,cd) # do replacement
    nnpat
end
patrule(ex,pat1::ExSym,pat2::ExSym) = patrule(ex,pattern(pat1),pattern(pat2))

# Same as patrule, except if match fails, return original expression
function tpatrule(ex,pat1,pat2)
    res = patrule(ex,pat1,pat2)
    res == false ? ex : res
end

# apply replacement rule r to expression ex
replace(ex::ExSym, r::PRule) = tpatrule(ex,r.lhs,r.rhs)

# Do depth-first replacement applying the same rule to each subexpression
function replaceall(ex,pat1::Pattern,pat2::Pattern)
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
function replaceall(ex::ExSym, r::PRule)
    replaceall(ex,r.lhs,r.rhs)
end

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

#replacerepeated(ex, rules::Array{PRule,1}) = replacerepeated(ex,

function replacerepeated(ex, rules::Array{PRule,1})
    local ex1
    if isexpr(ex)
        ex1 = Expr(ex.head, ex.args[1],
             map((x)->replaceall(x,rules),ex.args[2:end])...)
    end
    local res
    for r in rules
        res = patrule(ex1,r.lhs,r.rhs)
        if (res != false)
            ex1 = res
            break
        end
    end
    if ( ex != ex1 )
        ex1 = replacerepeated(ex1,rules)
    end
    ex1
end


## macros

macro pattern(ex)
    Pattern(ustopat(ex),:All)
end

# We don't know yet how to apply conditions
macro pattcond(ex,cond)
    Pattern(ustopat(ex),cond)
end

macro rule(ex)
    ex.head != :(=>) && error("rule: expecting lhs => rhs")
    prule(pattern(ex.args[1]),pattern(ex.args[2]))
end

function mkrule(ex::Expr)
    ex.head != :(=>) && error("rule: expecting lhs => rhs")
    prule(pattern(ex.args[1]),pattern(ex.args[2]))
end

macro replaceall(ex,therule)
    if typeof(therule) == Symbol ||
        therule.head == :vcat
        therule = eval(therule)
    else
        therule = mkrule(therule)
    end
    Expr(:call, :replaceall, Expr(:quote, ex), therule)
end

macro replacerepeated(ex,therule)
    if typeof(therule) == Symbol ||
        therule.head == :vcat
        therule = eval(therule)
    else
        therule = mkrule(therule)
    end
    Expr(:call, :replacerepeated, Expr(:quote, ex), therule)
end

true
