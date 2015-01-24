## Order downvalues by specificity, or canonical

## associating Julia data structues with downvalues would be
# more efficient.

#  a == f(blah,blah) , b == f(foo,barblah)
function isless_patterns(a::Mxpr, b::Mxpr)
    length(a) < length(b) && return true
    
end 
