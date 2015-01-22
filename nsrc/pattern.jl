# # pattern names can't contain _, so this is no pattern name
# const nullblank = :_null

# type Blank
#     name::Symbol
#     head::Symbol    
# end

# Blank() = Blank(nullblank)

# type BlankSequence
#     name::Symbol
#     head::Symbol    
# end
# BlankSequence() = BlankSequence(nullblank)

# This is a hack into the older pattern matcher.
# We will redesign this.
function RuleDelayed_to_PRule(mx::Mxpr{:RuleDelayed})
    lhs = mx[1][1]
    rhs = mx[2]
#    println("lhs $lhs, rhs $rhs")
    ptp = patterntopvar(lhs)
#    println("ptp is $ptp")
    nlhs = PatternT(ptp,:All)
    nrhs = PatternT(rhs,:All)    
    PRule(nlhs,nrhs)
end

function patterntopvar(mx::Mxpr)
    nargs = Array(Any,0)
#    println("patterntopaver mxpr $mx")
    for x in mx.args
#        println("x is $x")
        nx = patterntopvar(x)
#        println("nx is $nx")        
        push!(nargs,nx)
    end
#    println("nargs is $nargs")    
    nmx = mxpr(mx.head,nargs...)
#    println("nmx is $nmx")
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
