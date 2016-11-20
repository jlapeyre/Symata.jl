## Order downvalues by specificity, or canonically.
# Probably impossible, in general. We start with
# getting some common cases correct.
# But, we get many simple cases wrong.

## associating Julia data structues with downvalues would be
# more efficient.

# @sym ClearAll(g)
# @sym g(x_^2) := x
# @sym g(x_Integer^2) := x
# @sym g(z^2) := x
# dvs = @sym DownValues(g)

#
function has_pattern{T}(mx::Mxpr{T})
    for i in 1:length(mx)
        res = has_pattern(mx[i])
        res[1] == true && return res
    end
    if symname(mhead(mx)) == :Pattern
        return (true, mx[2])
    end
    return (false,false)
end

function has_pattern(x)
    (false,false)
end

#  a == f(blah,blah) , b == f(foo,barblah), (or they can be at a deeper level)
function isless_patterns(a::Mxpr, b::Mxpr)
    n = min(length(a),length(b))
    for i in 1:n
        (haspata,blanka) = has_pattern(a)
        (haspatb,blankb) = has_pattern(b)
        res = haspatb && (! haspata)
        if haspatb && (! haspata)  return true end
        length(blanka) > length(blankb) && return true
    end
    return false
end
