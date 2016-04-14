## Flatten, flatten nested operations that are associative

# eg   (a + b + (c + d)) --> (a + b + c + d)

# FlatT because Flat is already a symbol
typealias FlatT Union{Mxpr{:Plus},Mxpr{:Times}}

# Might be faster to interleave the terms
# because they may need less ordering then.
# As it is, this is very fast for flattening two long sums.
# We tried this:
# m1 = Apply(Plus,Table(x^i,[i,1000])); m2 = Apply(Plus,Table(x^i,[i,1000]));
# then m3 = mxpr(:Plus,symval(:m1),symval(:m2))
# flatten!(m3) -->  6e-5s
# canonexpr!(m3) (ie ordering) --> 0.011s
# running canonexpr! again (i.e. it is already sorted and combined)
# takes 0.00925 s, that is only a little bit faster.
# So maybe optimizing here is not worth anything at the moment.
# At SJulia cli, m3 = m1 + m2 --> 0.04 s.
# calling setfixed() after canonexpr cuts this time to 0.01s
# But, this cannot be done, in general.
# Maxima generates the two sums much more slowly but adds
# them much more quickly.
function flatten!{T<:FlatT}(mx::T)
    needsflat::Bool = false
    for x in margs(mx)
        if is_type(x,T)
            needsflat = true
            break
        end
    end
#    println("Doing flat")
    needsflat == false && return mx
#    println("Needs flat")
    na = newargs()
    for x in margs(mx)
        if is_type(x,T)
            for y in margs(x)
                push!(na,y)
            end
        else
            push!(na,x)
        end
    end
    return mxpr(mhead(mx),na)
end

# Here we do not copy
flatten!(x) = x
