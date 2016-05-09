abstract Blanks

# name -- the name, ending in underscore
# cond -- a condition that must be satisfied to match
#         cond may be :All, which matches anything.
type BlankT{T}  <: Blanks
    name::SJSym  # name
    head::T
    pattern_test::Any    # either symbol :None, or Mxpr to be mevaled for test.
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

# Disbled translating here. It happens below in process_blank_head
function makeBlankT(name,head,pattern_test)
    BlankT(name,head,pattern_test)
    #    newhead = get(blank_head_dict,head,head)
end

# Not yet implemented
type BlankSequenceT{T}  <: Blanks
    name::SJSym  # name
    head::T
    pattern_test::Any    # either symbol :None, or Mxpr to be mevaled for test.
end

# Not yet implemented
type BlankNullSequenceT{T}  <: Blanks
    name::SJSym  # name
    head::T
    pattern_test::Any
end

getpvarpattern_test(pvar::Blanks) = pvar.pattern_test
getpvarhead(pvar::Blanks) = pvar.head

# pieces of expressions that we operate on are Symbols and expressions
typealias ExSym Union{Mxpr,SJSym}
typealias ExSymBlankT Union{ExSym,Blanks}

# ast  -- the pattern including BlankTs for capture.
# cond -- condition to apply to any BlankTs in the pattern
# We don't need to use PatternT anymore. It could be Pattern.
# Currently cannot be made immutable. And, only cond can have static type.
type PatternT{T}
    ast::Any
    cond::T
    isdelayed::Bool  # if true, we evaluate before trying
end

PatternT(ast,cond) = PatternT(ast,cond,false)
PatternT(ast::ExSymBlankT) = PatternT(ast,:All)

# replacement rule
# lhs -- a pattern for matching.
# rhs -- a template pattern for replacing.
type PRule
    lhs::PatternT
    rhs::PatternT
end

function Rule_to_PRule(mx::Mxpr{:RuleDelayed})
    local lhs
    if mhead(mx[1]) == :HoldPattern  # inefficient
        lhs = mx[1][1]
    else
        lhs = mx[1]
    end
    rhs = mx[2]
    ptp = patterntopvar(lhs)
    nlhs = PatternT(ptp,:All)
    nrhs = PatternT(rhs,:All, true)  # rhs is delayed
    PRule(nlhs,nrhs)                 # true means rhs delayed
end

function Rule_to_PRule(mx::Mxpr{:Rule})
    lhs = mx[1]
    rhs = mx[2]
    ptp = patterntopvar(lhs)
    nlhs = PatternT(ptp,:All)
    nrhs = PatternT(rhs,:All)
    PRule(nlhs,nrhs)
end

# Works on just a blank, and ... ?

just_pattern(s) =   PatternT(patterntopvar(s), :All)

function patterntopvar(mx::Mxpr)
    nargs = newargs()
    for x in mx.args
        nx = patterntopvar(x)
        push!(nargs,nx)
    end
    nmx = mxpr(mhead(mx), nargs...)
    nmx
end

patterntopvar(x) = x
patterntopvar(mx::Mxpr{:PatternTest}) =  patterntopvar(mx,margs(mx)...)

function patterntopvar(mx::Mxpr{:PatternTest}, pattern, cond::Symbol)
    pvar = patterntopvar(pattern)
    pvar.pattern_test = mxpr(symval(cond),0) # reserve 1 arg, allocate mxpr here, not in loop using pattern.
    return pvar
end

function patterntopvar(mx::Mxpr{:PatternTest}, pattern, cond::Function)
    pvar = patterntopvar(pattern)
    pvar.pattern_test = mxpr(cond,0)
    return pvar
end

function process_blank_head(head)
    head = get(blank_head_dict, head, head)
    ehead = isdefined(head) ? eval(head) : head  # Symbol may eval to DataType
    head = (typeof(ehead) == Symbol || typeof(ehead) == DataType) ? ehead : head
end

function patterntopvar(mx::Mxpr{:Pattern})
    var = mx[1]
    blank = mx[2]
    if length(blank) == 0 # match any head
       res = makeBlankT(symname(var),:All,:None)
    else # match only if head is blank[1]
        head = blank[1]
        head = process_blank_head(head)
        res = makeBlankT(symname(var),head,:None)
    end
    res
end

# Be careful this is not called depth-first.
# It should apply when Blank is  not wrapped in Pattern.
# Eg. MatchQ( a, _Integer)
function patterntopvar(mx::Mxpr{:Blank})
    var = :_  # Underscore is currently illegal in SJulia identifiers, so this is safe.
    blank = mx
    if length(blank) == 0 # match any head
       res = makeBlankT(var,:All,:None)
    else
        head = blank[1]
        head = process_blank_head(head)
        res = makeBlankT(symname(var),head,:None)
    end
    res
end

function patterntopvar(mx::Mxpr{:BlankSequence})
    var = :__  # Underscore is currently illegal in SJulia identifiers, so this is safe.
    blank = mx
    if length(blank) == 0 # match any head
       res = BlankSequenceT(var,:All,:None)
    else
        head = blank[1]
        head = process_blank_head(head)
        res = BlankSequenceT(symname(var),head,:None)
    end
    res
end


function trysymbolrule(mx::Mxpr,rd::Mxpr{:RuleDelayed})
    prule = Rule_to_PRule(rd)
    res = replacefail(mx,prule)
    res
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

# Maybe need to setunfixed here, as well
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
