# This is a hack into the older pattern matcher.
# We will redesign this.
function RuleDelayed_to_PRule(mx::Mxpr{:RuleDelayed})
    lhs = mx[1][1]
    rhs = mx[2]
    ptp = patterntopvar(lhs)
    nlhs = PatternT(ptp,:All)
    nrhs = PatternT(rhs,:All)    
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

function patterntopvar(mx::Mxpr)
    nargs = Array(Any,0)
    for x in mx.args
        nx = patterntopvar(x)
        push!(nargs,nx)
    end
    nmx = mxpr(mx.head,nargs...)
    nmx
end

patterntopvar(x) = x

function patterntopvar(mx::Mxpr{:Pattern})
    var = mx[1]
    blank = mx[2]
    if length(blank) == 0
       res = Pvar(symname(var),:All)
    else
       res = Pvar(symname(var),symname(blank[1]))
    end
    res
end

function trydownvalue(mx::Mxpr,rd::Mxpr{:RuleDelayed})
    prule = RuleDelayed_to_PRule(rd)
    replacefail(mx,prule)
end

function trydownvalues(mx::Mxpr)
    dvs = downvalues(mx.head)
    for r in dvs
        res = trydownvalue(mx,r)
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
