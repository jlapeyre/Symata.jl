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

# capture expression ex in blank, or return false if the new value conflicts with old.
# An example of the later case is f(x_,x_) on f(2,3).
function captureblank(capt,blank::BlankT,ex)
    name = blank.name
    captureblank(capt,name,ex)
end

function captureblank(capt,name::Symbol,ex)
    haskey(capt,name) && capt[name] != ex  && return false
    capt[name] = ex
    return true
end

# store captured expression in Dict. Here only the capture var name
# storecapt(pat,cap,cd) = cd[pat] = cap  # not used
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
function matchpat(blank::BlankT,ex)
    @mdebug(1, "matchpat entering ex = ", ex)
    head = getBlankhead(blank)  # head to match
    match_head(head,ex) || return false
    cc = getpvarpattern_test(blank)   # This is an Mxpr or :None
    cc == :None && return true
    is_Mxpr(cc) || error("matchpat: Pattern test to match is not a Mxpr. $cc of type ", typeof(cc))
    cc.args[1] = ex           # we reuse a stored Mxpr.
    # This is likely not robust. Works for what we have now, but what about upvalues, ... ?
    res = apprules(cc)        # we decide that apprules (builtin) overrides and up or down values.
    res == true && return true
    res == false && return false
    if has_downvalues(cc)
        return doeval(applydownvalues(cc)) == true  # or maybe just return what infseval gives        
    else
        return false
    end
end

# For non-orderless, non-flat matching only
# Descend expression tree. If there is no pattern var in
# a subexpression of pattern `pat', then the subexpressions in
# mx and pat must match exactly.
# If pat is a capture var, then it matches the subexpression ex,
# if the 'test' as checked by matchpat is satisfied.

# capturevar -> false means contradicts previous capture
_cmppat(mx, pat::BlankT, captures)  = matchpat(pat,mx) ? captureblank(captures,pat,mx) : false

#### Alternatives

# FIXME. ambiguity warnings for the next several methods
# Lots of room for optimization here. But, we want to avoid premature optimization
function _cmppat(mx, pat::Mxpr{:Alternatives}, captures)
    for i in 1:length(pat)
        alt = pat[i]
        res = _cmppat(mx, alt, captures)
        if res != false       # We accept the first match
            names = Symbol[]  # We bind all the symbols that don't match to Sequence[], so they disappear
            for j in 1:length(pat)
                i == j && continue  # skip the alternative that did match
                append!(names,get_blank_names(pat[j]))  # get all blank names
            end
            for name in names
                captureblank(captures,name,mxpr(:Sequence)) # we could use a const empty sequence here.
            end
            return res # return the matching alternative
        end
    end
    false  # no alternative matched
end

#### Except

function _cmppat(mx, pat::Mxpr{:Except}, captures)
    if length(margs(pat)) == 1   # Not matching is matching
        res = _cmppat(mx, pat[1], captures)
        if res == false    # no match
            res = _cmppat(mx, patterntoBlank(mxpr(:Blank)), captures) # match anything
            res == false && error("Programming error matching 'Except'") # use assert
            return res
        end
        return false   # hmmm, but the capture is still there. We probably should delete it.
    elseif length(margs(pat)) == 2  # Must not match first and must match second
        res = _cmppat(mx, pat[1], captures)
        if res == false   # need no match with pat[1] ....
            res = _cmppat(mx, pat[2], captures)  # and a match with pat[2]
            return res
        end
        return false
    end
    # In Mma, warning message only occurs when Except is "used". It is here, in a Pattern rule,
    # rather than a standard evaluation rule.
    # FIXME. For Cases([1, 0, 2, 0, 3], Except(0,1,2)), Mma prints this warning message once.
    # We print if 5 times, one for each test. I think the reason  is that evaluating Cases is aborted rather,
    # than the message surpressed. Maybe, we do want to throw an exception. But, how is it done in general ?
    sjthrow(TwoNumArgsErr("Except", 1:2, length(pat)))
    false  # number of args < 1  or > 2 is not documented.
end

#### Condition

# Get both of these messages. Usage with number of args != 2 is not documented.
# Condition::argm: Condition called with 0 arguments; 1 or more arguments are expected.
# Condition::argm: Condition called with 0 arguments; 2 or more arguments are expected.
# NB. Condition[1,2,3] --> 1 /; Sequence[2, 3]
function _cmppat(mx, pat::Mxpr{:Condition}, captures)
    if length(pat) != 2  # For now, we require 2 args
#        sjthrow(MoreNumArgsErr("Condition", 1, length(pat)))
        sjthrow(ExactNumArgsErr("Condition", 2, length(pat)))        
        return false
    end
    lhs = doeval(pat[1])
    res = _cmppat(mx, lhs, captures)
    res == false && return false
    # We must copy, otherwise, on repeated calls to this function, rhs has the previous substituted value on entry.
    # Must be deep because replacements can be deep.
    rhs = deepcopy(pat[2]) 
    patsubst!(rhs, captures)
    condres = doeval(rhs)
    condres != true && return false  # Anything other than true is failure
    return res                       # return the match with captures
end

#### General Mxpr

# Matching a non-atomic expression. The head and length must match and each subexpression must match.
function _cmppat(mx::Mxpr, pat::Mxpr, captures)
    (mhead(pat) == mhead(mx) && length(pat) == length(mx)) || return false
    @inbounds for i in 1:length(mx)      # match and capture subexpressions
         _cmppat(mx[i],pat[i],captures) == false && return false
    end
    return true
end

##### Atoms

# This is a leaf on the tree, because mx is not an Mxpr and
# pat is not a BlankT. 
_cmppat(mx,pat,captures) = mx == pat  # 'leaf' on the tree. Must match exactly.

# Allow different kinds of integers and floats to match
_cmppat{T<:Integer,V<:Integer}(mx::T,pat::V,captures) = mx == pat

_cmppat{T<:AbstractFloat,V<:AbstractFloat}(mx::T,pat::V,captures) = mx == pat

# In general, Numbers should be === to match. Ie. floats and ints are not the same
_cmppat{T<:Number,V<:Number}(mx::T,pat::V,captures) = mx === pat

# Collect all the names of blanks in expression ex or its subexpressions
function get_blank_names(ex)
    names = Symbol[]
    _get_blank_names(names,ex)
    return names
end

_get_blank_names(names,x) = nothing

_get_blank_names(names, b::BlankT) = push!(names,b.name)

function _get_blank_names(names,mx::Mxpr)
    for arg in margs(mx)
        _get_blank_names(names,arg)
    end
end

#######  Replacing

# match and capture on ex with pattern pat1.
# Replace pattern vars in pat2 with expressions captured from ex.
function patrule(ex,pat1::PatternT,pat2::PatternT)
    @mdebug(1, "enter patrule with ", ex)
    (res,capt) = cmppat(ex,pat1)
    res == false && return false # match failed
    # We need something more efficient than deepcopy !
    # deep copy and x_ -> pat(x) original
    npat = pat2.isdelayed ? deepcopy(doeval(pat2)) : deepcopy(pat2)
    nnpat = patsubst!(npat.ast,capt) # do replacement
    return nnpat
end

# Same as patrule, except if match fails, return original expression
# function tpatrule(ex,pat1,pat2)
#     res = patrule(ex,pat1,pat2)
#     res === false ? ex : res
# end

# apply replacement rule r to expression ex
function replace(ex::ExSym, r::PRule)
    res = patrule(ex,r.lhs,r.rhs)
    res === false ? ex : res    
end

type ReplaceData
    rule
end

function replace(levelspec::LevelSpec, ex::ExSym, r::PRule)
    data = ReplaceData(r)
    action = LevelAction(data,
                         function (data, expr)
                             action.levelind == 0 && return 
                             res = patrule(expr,data.rule.lhs, data.rule.rhs)
                             if res !== false
                               action.parent[action.subind] = res
                             else
                               nothing
                             end 
                         end)
    exnew = deepcopy(ex)  # This is expensive. We need something more efficient.
    if has_level_zero(levelspec)  # Do level zero separately
        res = patrule(exnew, r.lhs, r.rhs)
        if res !== false
            exnew = res
        end
    end
    traverse_levels!(action,levelspec,exnew)
    unsetfixed(exnew)
end


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
