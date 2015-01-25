## Pattern matching and rules

typealias InExpr Mxpr   # annotation to input arguments
typealias UExpr  Mxpr  # annotation for expressions in Unions

head(ex) = ex.head
margs(ex) = ex.args

# pieces of expressions that we operate on are Symbols and expressions
typealias ExSym Union(UExpr,Symbol,SJSym)
# TODO: need to split this up and separate them by dispatch
typealias CondT Union(UExpr,Symbol,SJSym,DataType,Function)

isexpr(x) = (typeof(x) <: AbstractMxpr)

# Pattern variable. name is the name, ending in underscore cond is a
# condition that must be satisfied to match But, cond may be :All,
# which matches anything.  The most imporant feature is that the Pvar
# matches based on its context in an AST.
type Pvar
    name::Symbol  # name
    cond::CondT   # condition for matching. DataType, function...
end
typealias ExSymPvar Union(UExpr,Symbol,SJSym,Pvar)

# we could allow non-underscored names
function Pvar(name::Symbol)
    Pvar(name,:All)
end

==(a::Pvar, b::Pvar) = (a.name == b.name && a.cond == b.cond)

# ast is the pattern including Pvars for capture.
# cond is condition to apply to any Pvars in the pattern
#
# Hack to get around hack. We are polluting Julia namespace
# with SJSym's just to get repl completion.
# So 'Pattern' is already used. So we use PatternT.
# But, we will fix the repl and rewrite code.
type PatternT
    ast::Any
    cond::CondT
end

PatternT(ast::ExSymPvar) = PatternT(ast,:All)
pattern(ast::ExSym) = pattern(ast,:All)

function Base.show(io::IO, pv::Pvar)
    show(io,pv.name)
end

function Base.show(io::IO, p::PatternT)
    show(io,p.ast)
end


pattern(x,cond::Symbol) = PatternT(x,cond)

pattern(x) = pattern(x,:All)

# replacement rule
# lhs is a pattern for matching.
# rhs is a template pattern for replacing.
type PRule
    lhs::PatternT
    rhs::PatternT
end

function Base.show(io::IO, p::PRule)
    print(io,"rule: ")
    show(io,p.lhs)
    print(io, " => ")
    show(io,p.rhs)
end


## most of this stuff is old. works in Julia, not SJulia

PRule(lhs::ExSym, rhs::ExSym) = PRule(pattern(lhs),pattern(rhs))
==(a::PRule, b::PRule) =  (a.lhs == b.lhs && a.rhs == b.rhs)
==(a::PatternT, b::PatternT) = (a.ast == b.ast)
prule(x,y) = PRule(x,y)
# syntax for creating a rule. collides with Dict syntax sometimes.
=>(lhs::ExSym,rhs::ExSym) = prule(pattern(lhs),pattern(rhs))
=>(lhs::ExSym,rhs::Symbol) = prule(pattern(lhs),pattern(rhs))
=>(lhs::ExSym,rhs::Number) = prule(pattern(lhs),pattern(rhs))
prule(lhs::Mxpr, rhs::Mxpr) = prule(pattern(lhs),pattern(rhs))
prule(x::Mxpr, y::Number) = prule(pattern(x),pattern(y))

const ruledict = Dict{Symbol,Array{PRule,1}}()

function downrule(sym::Symbol, r::PRule)
    if !haskey(ruledict,sym)
        ruledict[sym] = Array(PRule,0)
    end
    push!(ruledict[sym],r)
    r
end

macro dr(ex::Expr)
    ex.head != :(:=) && error("@dr expected head :=")
    ex1 = deepcopy(ex)
    ex1.head = :(=>)
    r = eval(ex1)
    downrule(r.lhs.ast.head, r)
    r
end
   
ispvar(x) = typeof(x) == Pvar
#pvarsym(pvar::Pvar) = pvar.name
pvarsym(pvar::Symbol) = pvar
getpvarcond(pvar::Pvar) = pvar.cond
setpvarcond(pvar::Pvar,cond) = pvar.cond = cond

# Perform match and capture.
function cmppat(ex,pat::PatternT)
    capt = capturealloc() # Array(Any,0)  # allocate capture array
    success_flag = _cmppat(ex,pat.ast,capt) # do the matching
    return (success_flag,capt)  # report whether matched, and return captures    
end
cmppat(ex,pat::ExSym) = cmppat(ex,pattern(pat))

function capturealloc()
    return Dict{Symbol,Any}()
end

function capturepvar(capt,pvar,ex)
    name = pvar.name
    haskey(capt,name) && capt[name] != ex  && return false
    capt[name] = ex
    return true
end

# store captured expression in Dict. Here only the capture var name
function storecapt(pat,cap,cd)
    cd[pvarsym(pat)] = cap
end

# retrieve captured expression by caption var name
function newretrievecapt(sym,cd)
    cd[sym]
end

function newretrievecapt(sym::SJSym,cd)
    cd[symname(sym)]
end

function havecapt(sym,cd)
    haskey(cd,sym)
end

function havecapt(sym::SJSym,cd)
    haskey(cd,symname(sym))
end

# if we don't know what the condition is, try to evalute it.
function evalcond(c)
    println("evaling expression $c")
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
#matchpat(x,y) = true  # for debugging
function matchpat(cvar,ex)
    @mdebug(1, "matchpat entering ex = ", ex)
    c = getpvarcond(cvar)
    c == :All && return true # no condition
    if typeof(c) == Symbol
        ce = eval(c)
        typeof(ce) == DataType && return typeof(ex) <: ce  # NOTE: We use <: !
    end
    if isexpr(c)
        if c.head == :->  # anon function
            println("Got a function expression")
            f = meval(c)
            println("Type of f is now ", typeof(f))
            # Replacing expression with compiled anonymous function does not work.
            # clean this up, anyway. it is usually compiled long before we get here.
            setpvarcond(cvar,f)  
            return f(ex)
        end
    end
    if typeof(c) == Function
#        println("Got a real function")
        return c(ex)
    end
    ce = evalcond(c) # punt and try eval
    ce === false && return false # maybe true here ?!
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
function _cmppat(mx,pat,captures)
    if ispvar(pat) && matchpat(pat,mx)
        return capturepvar(captures,pat,mx)  # false means contradicts previous capture
    end
    if !isexpr(mx)
        res = mx == pat # 'leaf' on the tree. Must match exactly.
        return res
    end
    if !isexpr(pat) || head(pat) != head(mx) ||
        length(pat) != length(mx)
        return false
    end
    for i in 1:length(mx) # match and capture subexpressions.
         _cmppat(mx[i],pat[i],captures) == false && return false
    end
    return true
end

# match and capture on ex with pattern pat1.
# Replace pattern vars in pat2 with expressions captured from
# ex.
function patrule(ex,pat1::PatternT,pat2::PatternT)
    @mdebug(1, "enter patrule with ", ex)
    (res,capt) = cmppat(ex,pat1)
    res == false && return false # match failed
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
    res === false ? ex : res
end

# apply replacement rule r to expression ex
replace(ex::ExSym, r::PRule) = tpatrule(ex,r.lhs,r.rhs)

replacefail(ex::ExSym, r::PRule) = patrule(ex,r.lhs,r.rhs)

# Do depth-first replacement applying the same rule to each subexpression
function replaceall(ex,pat1::PatternT,pat2::PatternT)
    if isexpr(ex)
        ex = mxpr(head(ex),
                    map((x)->replaceall(x,pat1,pat2),margs(ex))...)
    end
    # we have applied replacement at all lower levels. Now do current level.
    res = patrule(ex,pat1,pat2)
    res === false && return ex # match failed; return unaltered expression
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
        ex = mxpr(head(ex),
                    map((x)->replaceall(x,rules),margs(ex))...)
    end
    for r in rules
        res = patrule(ex,r.lhs,r.rhs)
        res !== false && return res
    end
    ex
end

# Do the substitution recursively.
# pat is the template pattern: an expression with 0 or more pattern vars.
# cd is a Dict with pattern var names as keys and expressions as vals.
# Subsitute the pattern vars in pat with the vals from cd.
# Version for new pattern matching format:  x_ => x^2
function patsubst!(pat,cd)
    if (isexpr(pat) || is_SJSym(pat)) && ! havecapt(pat,cd)
        pa = pat.args
        for i in 1:length(pa)
            if havecapt(pa[i],cd)
                pa[i] =  newretrievecapt(pa[i],cd)
            elseif isexpr(pa[i])
                pa[i] = patsubst!(pa[i],cd)
            end
        end
    elseif ispvar(pat) || is_SJSym(pat)
        pat = newretrievecapt(pat,cd)
    end
    return pat
end

replacerepeated(ex, rules::Array{PRule,1}) = _replacerepeated(ex,rules,0)
replacerepeated(ex, therule::PRule) = _replacerepeated(ex,[therule],0)

function _replacerepeated(ex, rules::Array{PRule,1},n)
    n > 10^5 && error("Exceeded max iterations, $n, in replacerepeated")
    ex1 = ex
    if isexpr(ex)
        ex1 = mxpr(ex.head, ex.args[1],
             map((x)->replaceall(x,rules),ex.args[2:end])...)
    end
    local res
    for r in rules
        res = patrule(ex1,r.lhs,r.rhs)
        if (res !== false)
            ex1 = res
            break
        end
    end
    if ( ex != ex1 )
        ex1 = _replacerepeated(ex1,rules,n+1)
    end
    ex1
end

nothing
