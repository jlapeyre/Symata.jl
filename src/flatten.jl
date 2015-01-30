## Flatten, flatten nested operations that are associative

# eg   (a + b + (c + d)) --> (a + b + c + d)

typealias Flat Union(Mxpr{:Plus},Mxpr{:Times})

# Here we copy
function flatten!{T<:Flat}(mx::T)
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
    nmx = mxpr(head(mx),na...)
    nmx
end

# Here we do not
flatten!(x) = x

# only Flat, or Mxpr
function deepflatten!(mx::Mxpr)
    for i = 1:length(mx)
        mx.args[i] = deepflatten!(mx.args[i])
    end
    mx = flatten!(mx)
end
deepflatten!(x) = x
