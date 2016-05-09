## Pattern matching and rules


#######  Matching

# Perform match and capture.
function cmppat(ex,pat::PatternT)
    capt = capturealloc() # Array(Any,0)  # allocate capture array
    success_flag = _cmppat(ex,pat.ast,capt) # do the matching
    return (success_flag,capt)  # report whether matched, and return captures
end

# pre-allocate the capture Dict. This can be much faster in a loop.
function cmppat(ex,pat::PatternT, capt)
    empty!(capt)
    success_flag = _cmppat(ex,pat.ast,capt) # do the matching
    return (success_flag,capt)  # report whether matched, and return captures
end

capturealloc() = Dict{SJSym,Any}()

# capture expression ex in pvar, or return false if the new value conflicts with old.
function capturepvar(capt,pvar,ex)
    name = pvar.name
    haskey(capt,name) && capt[name] != ex  && return false
    capt[name] = ex
    return true
end

# store captured expression in Dict. Here only the capture var name
storecapt(pat,cap,cd) = cd[pat] = cap
# retrieve captured expression by caption var name
retrievecapt(sym,cd) = cd[sym]
retrievecapt(sym::SJSym,cd) = cd[symname(sym)]
havecapt(sym,cd) = haskey(cd,sym)
havecapt(sym::SJSym,cd) = haskey(cd,symname(sym))

# For instance, in x_Integer, we match Integer.
function match_head(head::SJSym,ex)
    head == :All && return true
    return is_Mxpr(ex,head)
end

function match_head(head::DataType,ex)
    is_type_less(ex,head)
end

match_head(head,ex) = error("matchpat: Can't match Head of type ", typeof(head))

# Check if restrictions on Head and pattern test are satisfied.
# TODO: reorganize. maybe make type of BlankT.head Any
# so it can be a Symbol (only for finding SJSym),
# or a DataType. This is determined when the BlankT is
# created (of course later, this should be done once and stored with the downvalue)
# Then, much of the logic below can be eliminated
function matchpat(cvar,ex)
    @mdebug(1, "matchpat entering ex = ", ex)
    head = getpvarhead(cvar)  # head to match
    match_head(head,ex) || return false
    cc = getpvarpattern_test(cvar)   # This is an Mxpr or :None
    cc == :None && return true
    is_Mxpr(cc) || error("matchpat: Pattern test to match is not a Mxpr. $cc of type ", typeof(cc))
    cc.args[1] = ex           # we reuse a stored Mxpr.
    # This is likely not robust. Works for what we have now, but what about upvalues, ... ?
    res = apprules(cc)        # we decide that apprules (builtin) overrides and up or down values.
    res == true && return true
    res == false && return false
    if has_downvalues(cc)
        return infseval(applydownvalues(cc)) == true  # or maybe just return what infseval gives
    else
        return false
    end
end

# For non-orderless, non-flat matching only
# Descend expression tree. If there is no pattern var in
# a subexpression of pattern `pat', then the subexpressions in
# mx and pat must match exactly.
# If pat is a capture var, then it matches the subexpression ex,
# if the condition as checked by matchpat is satisfied.

# capturevar -> false means contradicts previous capture
_cmppat(mx, pat::BlankT, captures)  = matchpat(pat,mx) ? capturepvar(captures,pat,mx) : false

# FIXME. ambiguity warnings for the next three methods
# FIXME. We need to set unmatched named Blanks to Sequence[]
function _cmppat(mx, pat::Mxpr{:Alternatives}, captures)
    for alt in margs(pat)
        res = _cmppat(mx, alt, captures)
        if res != false return res end
    end
    false
end

function _cmppat(mx, pat::Mxpr{:Except}, captures)
    if length(margs(pat)) == 1
        res = _cmppat(mx, pat[1], captures)
        if res == false
            res = _cmppat(mx, patterntopvar(mxpr(:Blank)), captures)
            res == false && error("Programming error matching 'Except'") # assert
            return res
        end
        return false   # hmmm, but the capture is still there. We probably should delete it.
    elseif length(margs(pat)) == 2
        res = _cmppat(mx, pat[1], captures)
        if res == false
            res = _cmppat(mx, pat[2], captures)
            return res
        end
        return false
    end
    # FIXME  Except::argt: Except called with 3 arguments; 1 or 2 arguments are expected.
    # warning message only occurs when Except is "used"
    false  # number of args < 1  or > 2 is not documented.
end


# Matching a non-atomic expression. The head and length must match
# and each subexpression must match
function _cmppat(mx::Mxpr, pat::Mxpr, captures)
    (mhead(pat) == mhead(mx) && length(pat) == length(mx)) || return false
    @inbounds for i in 1:length(mx)      # match and capture subexpressions
         _cmppat(mx[i],pat[i],captures) == false && return false
    end
    return true
end

# This is a leaf on the tree, because mx is not an Mxpr and
# pat is not a BlankT. We are matching atoms
_cmppat(mx,pat,captures) = mx == pat  # 'leaf' on the tree. Must match exactly.

# Allow different kinds of integers and floats to match
_cmppat{T<:Integer,V<:Integer}(mx::T,pat::V,captures) = mx == pat

_cmppat{T<:AbstractFloat,V<:AbstractFloat}(mx::T,pat::V,captures) = mx == pat

# In general, Numbers should be === to match. Ie. floats and ints are not the same
_cmppat{T<:Number,V<:Number}(mx::T,pat::V,captures) = mx === pat

#######  Replacing

# match and capture on ex with pattern pat1.
# Replace pattern vars in pat2 with expressions captured from ex.
function patrule(ex,pat1::PatternT,pat2::PatternT)
    @mdebug(1, "enter patrule with ", ex)
    (res,capt) = cmppat(ex,pat1)
    res == false && return false # match failed
    # We need something more efficient than deepcopy !
    # deep copy and x_ -> pat(x) original
    npat = pat2.isdelayed ? deepcopy(infseval(pat2)) : deepcopy(pat2)
    nnpat = patsubst!(npat.ast,capt) # do replacement
    return nnpat
end

# Same as patrule, except if match fails, return original expression
function tpatrule(ex,pat1,pat2)
    res = patrule(ex,pat1,pat2)
    res === false ? ex : res
end

# apply replacement rule r to expression ex
replace(ex::ExSym, r::PRule) = tpatrule(ex,r.lhs,r.rhs)

function replacefail(ex::ExSym, r::PRule)
    patrule(ex,r.lhs,r.rhs)
end

function replaceall(ex,pat1::PatternT,pat2::PatternT)
    # first try at current level
    res = patrule(ex,pat1,pat2)
    res !== false && return res  # return if we had success
    if is_Mxpr(ex)               # no success so we try at lower levels.
        ex = mxpr(replaceall(mhead(ex),pat1,pat2),
                    map((x)->replaceall(x,pat1,pat2),margs(ex))...)
    end
    ex                # if lower levels changed nothing, this is the same as the input ex.
end

replaceall(ex, r::PRule) = replaceall(ex,r.lhs,r.rhs)

# TODO, check replaceall below and replacerepeated to see that they
# replace heads as does replaceall above.

# Apply an array of rules. each subexpression is tested.
# Continue after first match for each expression.
function replaceall(ex,rules::Array{PRule,1})
    if is_Mxpr(ex)
        args = margs(ex)
        nargs = newargs(length(args))
        for i in 1:length(args)
            nargs[i] = replaceall(args[i],rules)
        end
        ex = mxpr(mhead(ex),nargs)
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

function patsubst!(pat::Mxpr,cd)
    if ! havecapt(pat,cd)
        pa = margs(pat)
        @inbounds for i in 1:length(pa)
            if havecapt(pa[i],cd)
                pa[i] =  retrievecapt(pa[i],cd)
            elseif is_Mxpr(pa[i])
                pa[i] = patsubst!(pa[i],cd)
            end
        end
    end
    if havecapt(mhead(pat),cd)
        pat = mxpr(retrievecapt(mhead(pat),cd),margs(pat))
    end
    return pat
end

patsubst!(pat::SJSym,cd) = return  havecapt(pat,cd) ? retrievecapt(pat,cd) : pat
patsubst!(pat::BlankT,cd) = retrievecapt(pat,cd)
patsubst!(pat,cd) = pat

## ReplaceRepeated

function replacerepeated(ex, rules::Array{PRule,1}; kws...)
    too_many_iterations::Bool = true
    maxits::Int = 65536
    for p in kws
        (k,v) = p
        if k == :MaxIterations
            maxits = v
        end
    end
    res = replaceall(ex,rules)
    local res1 = res
    for i in 1:maxits
        res1 = doeval(replaceall(res,rules))
        if res1 == res
            too_many_iterations = false
            break
        end
        res = res1
    end
    if too_many_iterations
        warn("ReplaceRepeated: exceed maximum number of iterations $maxits")
    end
    return res1
end

replacerepeated(ex, therule::PRule; kws...) =  replacerepeated(ex,[therule]; kws...)

nothing
