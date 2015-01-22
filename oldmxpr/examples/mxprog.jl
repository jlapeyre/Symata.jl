function mxtest(mx::Mxpr, n::Int)
    local res
    for i in 1:n
        res = runmx("mx = mx +  mx")
    end
    return res
end
