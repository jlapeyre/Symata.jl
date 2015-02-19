# This is a hack into the older pattern matcher.
# applrules call this code, which then calls older pattern matching code

#function Pattern_to_PatternT(mx::Mxpr)

function RuleDelayed_to_PRule(mx::Mxpr{:RuleDelayed})
    lhs = mx[1][1]
    rhs = mx[2]
    ptp = patterntopvar(lhs)
    nlhs = PatternT(ptp,:All)
    nrhs = PatternT(rhs,:All)
#    if ptp == lhs  println("same ptp ", lhs) end
    PRule(nlhs,nrhs)
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
function just_pattern(mx::Mxpr)
    PatternT(patterntopvar(mx), :All)
end

function just_pattern(s)
    PatternT(patterntopvar(s), :All)
end

function patterntopvar(mx::Mxpr)
    nargs = newargs()
    for x in mx.args
        nx = patterntopvar(x)
        push!(nargs,nx)
    end
    nmx = mxpr(mx.head,nargs...)
    nmx
end

patterntopvar(x) = x

function patterntopvar(mx::Mxpr{:PatternTest})
    pvar = patterntopvar(mx[1])
    cond = mx[2]
    pvar.ptest = symname(cond)
    pvar
end

function patterntopvar(mx::Mxpr{:Pattern})
    var = mx[1]
    blank = mx[2]
    if length(blank) == 0 # match any head
       res = Pvar(symname(var),:All,:None)
    else # match only if head is blank[1]
       res = Pvar(symname(var),symname(blank[1]),:None)
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
       res = Pvar(var,:All,:None)
    else
       res = Pvar(var,symname(blank[1]),:None)
    end
    res    
end


function trysymbolrule(mx::Mxpr,rd::Mxpr{:RuleDelayed})
#    prule = @time(RuleDelayed_to_PRule(rd))
    prule = RuleDelayed_to_PRule(rd)
#    res = @time(replacefail(mx,prule))
    res = replacefail(mx,prule)
    res
end


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

## UpValues

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

function applyupvalues(mx::Mxpr,m::Mxpr)
    res = tryupvalues(mx,mhead(m))
    res === false ? mx : res
end

function applyupvalues(mx::Mxpr,m::SJSym)
    res = tryupvalues(mx,m)
    res === false ? mx : res
end


applyupvalues(x) = x
