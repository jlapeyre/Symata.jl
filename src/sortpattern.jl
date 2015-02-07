## Order downvalues by specificity, or canonically.
# Probably impossible, in general. We start with
# getting some common cases correct.
# But, we get many simple cases wrong.

## associating Julia data structues with downvalues would be
# more efficient.

# @ex ClearAll(g)
# @ex g(x_^2) := x
# @ex g(x_Integer^2) := x
# @ex g(z^2) := x
# dvs = @ex DownValues(g)

#
function has_pattern{T}(mx::Mxpr{T})
    for i in 1:length(mx)
        res = has_pattern(mx[i])
        res[1] == true && return res
    end
#    println("Head is '", symname(head(mx)), "'")
    if symname(mhead(mx)) == :Pattern
        return (true, mx[2])
    end
    return (false,false)
end

function has_pattern(x)
#    println("in generic $x")
    (false,false)
end

#  a == f(blah,blah) , b == f(foo,barblah), (or they can be at a deeper level)
function isless_patterns(a::Mxpr, b::Mxpr)
    n = min(length(a),length(b))
    for i in 1:n
        (haspata,blanka) = has_pattern(a)
        (haspatb,blankb) = has_pattern(b)
#        println("($haspata,$haspatb)")
        res = haspatb && (! haspata)
#        println("($res)")
        if haspatb && (! haspata)  return true end 
        length(blanka) > length(blankb) && return true
    end
    return false
end 

#sort!(dvs.args,lt=isless_patterns)
