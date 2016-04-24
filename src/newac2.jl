type MatchCapts
    data::Dict{Symbol,Any}
end
newmatchcapts() = MatchCapts(Dict{Symbol,Any}())

# NB, If we use this code, don't use tasks. Theyr
# are inefficient.


# This type corresponds to the expression:
# PatternTest[Pattern[name,Blank[head]],test]

# abstract AbstractPattern
# type BlankT <: AbstractPattern
#     head::Symbol
# end
# type PatternT1 <: AbstractPattern
#     name::Symbol
#     head::Symbol
# end
# function toBlankPat(mx::Mxpr{:Blank})
#     if length(mx) > 0
#         BlankT(mx[1])
#     else
#         BlankPat(:Any)
#     end
# end
# function toBlankPat(mx::Mxpr{:Pattern})
#     if length(mx) > 0
#         BlankT(mx[1])
#     else
#         BlankPat(:Any)
#     end
# end

# Match item to a single term in args of an AC expression.
# item can potentially match any of the terms. Find each match
# sequentially and return captures from that match and the remaining
# arguments so they can be matched by the next item.
single_ac_matches(item,args) = @task _single_ac_matches(item,args)
function _single_ac_matches(item,args)
    for i in 1:length(args)
        (goodflag,capts) = find_match(item,args[i])
        if goodflag
            restargs = copy(args)
            splice!(restargs,i)
            produce (capts, restargs)
        end
    end
end

# Patterns, such as  x_ can match multiple flat, orderess terms.
# I think, only x_, x__, x___, _, __, and ___ can match.
# Other qualifiers, heads and tests, mean only one term can match.
# This iterable (task) returns all partitions of args into two
# (a,b). First all subsets a of length 1 are chosen, then of
# length 2, etc. This is the correct order, since x_ should match
# the minimum number of terms possible.
mult_ac_matches(item,args) = @task _mult_ac_matches(item,args)
function _mult_ac_matches(item,args)
    for n in 1:length(args)
        for x in combinations(args,n) # all subsets of args of length n
            produce(x,setdiff(args,x)) # x and complement of x in args
        end
    end
end


function find_match(item,args)
    return (true, ())
end
