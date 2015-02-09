## Flatten, flatten nested operations that are associative

# eg   (a + b + (c + d)) --> (a + b + c + d)

# FlatT because Flat is already a symbol
typealias FlatT Union(Mxpr{:Plus},Mxpr{:Times})

# Here we copy
function flatten!{T<:FlatT}(mx::T)
    needsflat = false
    for x in margs(mx)
        if is_type(x,T)
            needsflat = true
            break
        end
    end
    needsflat == false && return mx
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

# Here we do not
flatten!(x) = x
