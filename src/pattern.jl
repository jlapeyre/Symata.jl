## Pattern matching and rules

#### BlankT

abstract Blanks

# BlankT is a "compiled" version of Mxpr{:Blank}
# This prevents process_blank_head from being called repeatedly, for what that is worth.
# This is the only remnant of the earliest version when all pattern related objects were Julia types.
# This requires all "patterns" (i.e. expressions that will be used as matching patterns) to be
# walked to convert Blank to BlankT.
# We could try using Mxpr{:Blank} instead. I removed all other "compiled" things in the
# name of flexibility and eschewing premature optimization.
#
# head -- a head that must match (or :All)
type BlankT{T}  <: Blanks
    head::T
end

# This dict translates the requested Head to match in a Blank
#
# Julia will complain if we use 'String'. For the moment, we
# do this replacement. This probably still allows matching different
# Julia string types.
#
# We allow both Float and Real for AbstractFloat. In Mma, 'Real' means
# real floating point number.
const blank_head_dict = Dict(    :String => :AbstractString,
                                 :Float => :AbstractFloat,
                                 :Real => :AbstractFloat)

function makeBlankT(head)
    BlankT(head)
end

# Not yet implemented
type BlankSequenceT{T}  <: Blanks
    head::T
end

# Not yet implemented
type BlankNullSequenceT{T}  <: Blanks
    head::T
end

getBlankhead(pvar::Blanks) = pvar.head

just_pattern(s) = patterntoBlank(s)

function patterntoBlank(mx::Mxpr)
    nargs = newargs()
    for x in mx.args
        nx = patterntoBlank(x)
        push!(nargs,nx)
    end
    nmx = mxpr(mhead(mx), nargs...)
    nmx
end

patterntoBlank(x) = x

function process_blank_head(head)
    head = get(blank_head_dict, head, head)
    ehead = isdefined(head) ? eval(head) : head  # Symbol may eval to DataType
    head = (typeof(ehead) == Symbol || typeof(ehead) == DataType) ? ehead : head
end


function patterntoBlank(mx::Mxpr{:Blank})
    blank = mx
    if length(blank) == 0 # match any head
       res = makeBlankT(:All)
    else
        head = blank[1]
        head = process_blank_head(head)
        res = makeBlankT(head)
    end
    res
end

function patterntoBlank(mx::Mxpr{:BlankSequence})
    blank = mx
    if length(blank) == 0 # match any head
       res = BlankSequenceT(:All)
    else
        head = blank[1]
        head = process_blank_head(head)
        res = BlankSequenceT(head)
    end
    res
end

# Try a downvalue
# hmmm, is it possible is is Rule rather than RuleDelayed ?
function trysymbolrule(mx::Mxpr,rd::Mxpr{:RuleDelayed})
    local lhs
    lhs = rd[1]
    rhs = rd[2]
    ptp = patterntoBlank(lhs)
    rrd = mxpr(:RuleDelayed, ptp, rhs)
    res = match_and_replace(mx,rrd)    
end

#### DownValues

function trydownvalues(mx::Mxpr)
    dvs = downvalues(mhead(mx))
    for r in dvs
        if is_down_trace() println("downvalue ",r) end
        increment_try_downvalue_count()
        res = trysymbolrule(mx,r)
        if res !== false  # false can be a legitimate value ?
            return res
        end
    end
    return false
end

function applydownvalues(mx::Mxpr)
    res = trydownvalues(mx)
    res === false ? mx : res
end

applydownvalues(x) = x

#### UpValues

function tryupvalues(mx::Mxpr,m::SJSym)
    dvs = upvalues(m)
    for r in dvs
        if is_up_trace() println("upvalue ",r) end
        increment_try_upvalue_count()
        res = trysymbolrule(mx,r)  # should be the same
        if res !== false  # false can be a legitimate value ?
            return res
        end
    end
    return false
end

# Maybe need to unsetfixed here, as well
function applyupvalues(mx::Mxpr,m::Mxpr)
    res = tryupvalues(mx,mhead(m))
    res === false && return mx
    return res
end

function applyupvalues(mx::Mxpr,m::SJSym)
    res = tryupvalues(mx,m)
    res === false && return mx
    unsetfixed(res)
    res
end


applyupvalues(x) = x


#######  Matching

# Perform match and capture.

# Compare expr to pattern.
# pattern may be almost anything: a literal atom or literal Mxpr.
# Or contain blanks, alternatives, conditions, etc., at any level.
# In particular, Mxpr{:Pattern} specifies a pattern and a name to associate
# the matching text. Any subsequent matches via Pattern with the same name
# must match identical text. This information is stored in the Dict 'capt'.
# We had a few optimizations, but they are removed for flexibility.
function cmppat(expr,pattern)
    capt = capturealloc() # Array(Any,0)  # allocate capture array
    success_flag = _cmppat(expr,pattern,capt) # do the matching
    return (success_flag,capt)  # report whether matched, and return captures
end

# pre-allocate the capture Dict. This can be much faster in a loop.
function cmppat(ex,pat, capt)
    empty!(capt)
    success_flag = _cmppat(ex,pat,capt) # do the matching
    return (success_flag,capt)  # report whether matched, and return captures
end

capturealloc() = Dict{SJSym,Any}()

# capturepatternname -> false means contradicts previous capture
function capturepatternname(capt,name::Symbol,ex)
    haskey(capt,name) && capt[name] != ex  && return false
    capt[name] = ex
    return true
end

# retrieve captured expression by caption var name
retrievecapt(sym,cd) = cd[sym]
retrievecapt(sym::SJSym,cd) = cd[symname(sym)]
havecapt(sym,cd) = haskey(cd,sym)
havecapt(sym::SJSym,cd) = haskey(cd,symname(sym))

#### HoldPattern

# HoldPattern has held its argument during the evaluation sequence.
# We now strip HoldPattern during pattern matching
_cmppat(ex, pat::Mxpr{:HoldPattern}, captures) = _cmppat(ex,pat[1],captures)

#### Pattern

# First arg is a name.
# Second arg is a pattern. If it matches, store the matching expression in
# a dict under the name.
function _cmppat(ex, pat::Mxpr{:Pattern}, captures)
    if length(pat) == 2
        (name,pattern) = (margs(pat)...)
        success_flag::Bool = _cmppat(ex,pattern,captures)
        if success_flag
            capture_success_flag::Bool = capturepatternname(captures,name,ex)
            return capture_success_flag
        else
            return success_flag
        end
    else
        sjthrow(ExactNumArgsErr("Pattern", length(pat), 2))
    end
end

#### PatternTest

function apply_test(ex,test)
    is_Mxpr(test) || error("PatternTest: Pattern test to match is not a Mxpr. $cc of type ", typeof(cc))
    test.args[1] = ex           # we reuse a stored Mxpr. Not any longer. We create the Mxpr every time
    res = apprules(test)        # we decide that apprules (builtin) overrides and up or down values.
    res == true && return true
    res == false && return false
    if has_downvalues(test)
        return doeval(applydownvalues(test)) == true  # or maybe just return what infseval gives
    else
        return false
    end
end

function _cmppat(ex, pat::Mxpr{:PatternTest}, captures)
    if length(pat) == 2
        (pattern,test) = (pat[1],pat[2])
        success_flag::Bool = _cmppat(ex, pattern, captures)
        success_flag == false && return false
        test = isa(test,Symbol) ? mxpr(symval(test),0) : isa(test,Function) ?
            mxpr(test,0) : error("PatternTest: unrecognized test $test")
        success_flag = apply_test(ex,test)
        return success_flag
    else
        sjthrow(ExactNumArgsErr("PatternTest", 2, length(pat)))
    end
end

#### BlankT

# For instance, in x_Integer, we match Integer.
function match_head(head::SJSym,ex)
    head == :All && return true
    return is_Mxpr(ex,head)
end

function match_head(head::DataType,ex)
    is_type_less(ex,head)
end

match_head(head,ex) = error("matchBlank: Can't match Head of type ", typeof(head))

function matchBlank(blank::BlankT,ex)
    head = getBlankhead(blank)  # head to match
    match_head(head,ex) || return false
end

_cmppat(mx, pat::BlankT, captures)  = matchBlank(pat,mx)

#### Alternatives

# Lots of room for optimization here. But, we want to avoid premature optimization.
function _cmppat(mx, pat::Mxpr{:Alternatives}, captures)
    for i in 1:length(pat)
        alt = pat[i]
        res = _cmppat(mx, alt, captures)
        if res != false       # Accept the first match
            names = Symbol[]  # We bind all the symbols that don't match to Sequence[], so they disappear
            for j in 1:length(pat)
                i == j && continue  # skip the alternative that did match
                append!(names,get_pattern_names(pat[j]))  # get all pattern names
            end
            for name in names
                capturepatternname(captures,name,mxpr(:Sequence)) # we could use a const empty sequence here.
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
# NB. Condition[1,2,3] --> 1 /; Sequence[2, 3]
function _cmppat(mx, pat::Mxpr{:Condition}, captures)
    if length(pat) != 2  # For now, we require 2 args
        sjthrow(ExactNumArgsErr("Condition", 2, length(pat)))
        return false
    end
    lhs = doeval(pat[1]) # Condition has Attribute HoldAll
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

#### Optional

# When this method is called, the optional argument has
# been supplied, so we just check if it matches. Of course, failure
# does not revert to the default value.
function _cmppat(mx, pat::Mxpr{:Optional}, captures)
    if length(pat) != 2
        sjthrow(ExactNumArgsErr("Optional", 2, length(pat)))
        return false
    end
    pattern = pat[1]
    default = pat[2]
    patname = pattern[1]
    res = _cmppat(mx, pattern, captures)
    res == true && return true
    return false
end

# This method is called when an optional argument has not been
# supplied. We put the default value in the capture dictionary
# and return true, signaling success.
function _do_optional(pat::Mxpr{:Optional}, captures)
    if length(pat) != 2
        sjthrow(ExactNumArgsErr("Optional", 2, length(pat)))
        return false
    end
    pattern = pat[1]
    default = pat[2]
    patname = pattern[1]
    capturepatternname(captures,patname,default)
    return true
end


#### General Mxpr

function mxpr_head_q(mx1::Mxpr, head)
    mhead(mx1) == head ? true : false
end

mxpr_head_q(x,head) = false

function mxpr_count_heads(mx::Mxpr, head)
    cnt = 0
    for i in 1:length(mx)
        if mxpr_head_q(mx[i],head) cnt += 1  end
    end
    cnt
end

function mxpr_head_freeq(mx::Mxpr, head)
    for i in 1:length(mx)
        mxpr_head_q(mx[i],head)  && return false
    end
    return true
end

function cmppat_no_optional_no_repeated(mx,pat,captures)
    length(mx) != length(pat) && return false
    for i in 1:length(mx)
        _cmppat(mx[i],pat[i],captures) == false && return false
    end
    return true
end

function cmppat_yes_optional_no_repeated(mx,pat,captures,lm,lp)
    for i in 1:lp
        if i > lm
            if is_Mxpr(pat[i],:Optional)
                _do_optional(pat[i], captures)
            else
                return false
            end
        else
            _cmppat(mx[i],pat[i],captures) == false && return false
        end
    end
    return true
end

# mx -- expression to match
# p -- Repeated or RepeatedNull
# imx -- current index in mx
# captures -- dict of captured variables
# default_min -- 0 for RepeatedNull, 1 for Repeated
function doRepeated(mx, p, imx, captures, default_min)
    if length(p) < 1 || length(p) > 2
        sjthrow(TwoNumArgsErr(default_min == 0 ? "RepeatedNull" : "Repeated" , 1:2, length(p)))
    end
    local rmin
    local rmax
    repeat_pattern = p[1]
    if length(p) == 1   # no repeat range specified
        rmin = default_min
        rmax = typemax(Int)
    else
        rspec = p[2]    # second arg is the repeat specification
        if is_Mxpr(rspec,:List)
            if length(rspec) == 1  # List of length one means exactly n repeats
                rmin = rmax = rspec[1]
            elseif length(rspec) == 2  # List of length two means repeat range
                rmin = rspec[1]
                rmax = rspec[2]
            end
        elseif isa(rspec,Integer)  # Integer n means 0 or 1 through n, for RepeatedNull, Repeated resp.
            rmin = default_min
            rmax = rspec
        else
            error("Expecting a number or a List of one or two numbers for range specification for ",
                  default_min == 0 ? "RepeatedNull" : "Repeated")
        end
    end
    repeat_count = 0
    while imx <= length(mx) && _cmppat(mx[imx],repeat_pattern,captures)
        repeat_count += 1
        imx += 1
        repeat_count >= rmax && break
    end
    imx -= 1
    repeat_count >= rmin && repeat_count <= rmax && return (true,imx)
    (false, imx)
end

function cmppat_no_optional_yes_repeated(mx,pat,captures)
    imx = 0
    for i in 1:length(pat)
        p = pat[i]
        imx += 1
        if is_Mxpr(p, :Repeated)
            (success, imx) =  doRepeated(mx, p, imx, captures, 1)
            success == false && return false
        elseif is_Mxpr(p, :RepeatedNull)
            (success, imx) =  doRepeated(mx, p, imx, captures, 0)
            success == false && return false
        else
            imx > length(mx) && return false
            _cmppat(mx[imx],p,captures) == false && return false
        end
    end
    imx < length(mx) && return false
    return true
end

# Matching a non-atomic expression. The head and length must match and each subexpression must match.
# We get ambiguity warnings if first arg is annotated: mx::Mxpr. But, it should always be Mxpr
#function _cmppat(mx::Mxpr, pat::Mxpr, captures)
# Accounting for Optional here is ugly. I don't like it. Must be a better way
function _cmppat(mx, pat::Mxpr, captures)
#    println("cmmpat Mxpr")
    (mhead(pat) == mhead(mx)) || return false
    nopt = mxpr_count_heads(pat, :Optional)
    have_repeated::Bool = ! (mxpr_head_freeq(pat,:Repeated) && mxpr_head_freeq(pat,:RepeatedNull))
    lp = length(pat)
    lm = length(mx)
    # TODO: Optimize the line below
    if have_repeated  # FIXME: handle repeated and optional
        return cmppat_no_optional_yes_repeated(mx,pat,captures)
    end
    ((lm >= lp - nopt) && (lm <= lp)) || return false
    if nopt == 0
        return cmppat_no_optional_no_repeated(mx,pat,captures)
    end
    return cmppat_yes_optional_no_repeated(mx,pat,captures,lm,lp)
end

##### Atoms

# This is a leaf on the tree, because mx is not an Mxpr and
# pat is not a BlankT.
_cmppat(mx,pat,captures) = mx == pat  # 'leaf' on the tree. Must match exactly.

# Allow different kinds of integers and floats to match
_cmppat(mx::Integer,pat::Integer,captures) = mx == pat

_cmppat{T<:AbstractFloat,V<:AbstractFloat}(mx::T,pat::V,captures) = mx == pat

# In general, Numbers should be === to match. Ie. floats and ints are not the same
_cmppat{T<:Number,V<:Number}(mx::T,pat::V,captures) = mx === pat

##### Get Pattern names

# Walk a (pattern) expression collecting all of the Pattern names
# A "pattern" is more or less anything. It may contain expressions with head Pattern,
# which is only used to bind a matched expression to a symbol. These symbols are
# what we collect here.
function get_pattern_names(ex)
    names = Symbol[]
    _get_pattern_names(names,ex)
    return names
end

_get_pattern_names(names,x) = nothing

_get_pattern_names(names, b::Mxpr{:Pattern}) = push!(names,b[1])

function _get_pattern_names(names,mx::Mxpr)
    for arg in margs(mx)
        _get_pattern_names(names,arg)
    end
end

#######  Replacing

function match_and_replace(ex,r::Rules)
    @mdebug(1, "enter match_and_replace with ", ex)
    lhs =r[1]
    rhs =r[2]
    (res,capt) = cmppat(ex,lhs)
    res == false && return false # match failed
    local rhs1
    if is_Mxpr(rhs, :Condition)
        rhs1 = rhs[1]
        condition = rhs[2]
        cond_pat_subst = patsubst!(deepcopy(condition),capt)
        cres = doeval(cond_pat_subst)
        cres == false && return false
        rhs  = rhs1
    end
    rhs_copy = deepcopy(rhs)
    rhs_copy_subst = patsubst!(rhs_copy,capt) # do replacement
    return rhs_copy_subst
end

#### Replace

# apply replacement rule r to expression ex
# No level spec
function replace(ex, r::Rules)
    r = patterntoBlank(r)
    res = match_and_replace(ex,r)
    res === false ? (false,ex) : (true,res)
end

type ReplaceData
    rule
end

# With level spec
function replace(levelspec::LevelSpec, ex, r::Rules)
    r = patterntoBlank(r)
    data = ReplaceData(r)
    action = LevelAction(data,
                         function (data, expr)
                             action.levelind == 0 && return
                             res = match_and_replace(expr,data.rule)
                             if res !== false
                               action.parent[action.subind] = res
                             else
                               nothing
                             end
                         end)
    exnew = deepcopy(ex)  # This is expensive. We need something more efficient.
    if has_level_zero(levelspec)  # Do level zero separately
        res = match_and_replace(exnew, r)
        if res !== false
            exnew = res
        end
    end
    traverse_levels!(action,levelspec,exnew)
    # FIXME: We always report success, i.e. there was a replacement, even if there was not.
    # But, we don't need this yet.
    (true, unsetfixed(exnew))
end

#### ReplaceAll

function replaceall(ex,rule::Rules)
    # first try at current level
    rule = patterntoBlank(rule)
    res = match_and_replace(ex,rule)
    res !== false && return res  # return if we had success
    if is_Mxpr(ex)               # no success so we try at lower levels.
        ex = mxpr(replaceall(mhead(ex),rule),
                    map((x)->replaceall(x,rule),margs(ex))...)
    end
    ex                # if lower levels changed nothing, this is the same as the input ex.
end

# TODO, check replaceall below and replacerepeated to see that they
# replace heads as does replaceall above.

# Apply an array of rules. each subexpression is tested.
# Continue after first match for each expression.
function replaceall(ex,rules::Array) # array needs to hold both Rule and RuleDelayed at once
    if is_Mxpr(ex)
        args = margs(ex)
        nargs = newargs(length(args))
        for i in 1:length(args)
            nargs[i] = replaceall(args[i],rules)
        end
        ex = mxpr(mhead(ex),nargs)
    end
    for r in rules
        res = match_and_replace(ex,patterntoBlank(r))
        res !== false && return res
    end
    ex
end

# Do the substitution recursively.
# pat is the template pattern: an expression with 0 or more captured vars.
# cd is a Dict with pattern var names as keys and expressions as vals.
# Subsitute the pattern vars in pat with the vals from cd.

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

function replacerepeated(ex, rules::Array; kws...)
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

replacerepeated{T<:Rules}(ex, therule::T; kws...) =  replacerepeated(ex,Rules[therule]; kws...)

#### FreeQ

type FreeQData
    pattern
    gotmatch::Bool
end

function freeq(levelspec::LevelSpec, expr, pat)
    data = FreeQData(pat,false)
    action = LevelAction(data,
                         function (data, expr)
                             (gotmatch,cap) = cmppat(expr,just_pattern(data.pattern))                         
                             if gotmatch
                               data.gotmatch = true
                               action.levelbreak = true
                             end 
                         end)
    if has_level_zero(levelspec)  # Do level zero separately
        (gotmatch,cap) = cmppat(expr,just_pattern(data.pattern))
        gotmatch && return false
    end
    traverse_levels!(action,levelspec,expr)
    data.gotmatch && return false
    return true
end


nothing
