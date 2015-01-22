function cos_array(n::Int)
    res = Array(Int,0)
    for i in 1:n
        push!(res,Cos(i*:Pi))
    end
    res
end
