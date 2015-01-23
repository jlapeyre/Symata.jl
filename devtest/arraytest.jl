# Test time for push! and unshift!
# They are supposed to be amortized constant time

function art(m)
    a = Array(Any,m)
    for i in 1:m
        a[i] = i
    end
    a
end


function partloop(n,m)
    s = 0    
    t = @elapsed  for i in 1:n
     a = art(m)
        x = pop!(a)
        push!(a,3)
        s += 1
    end
    t
end

function sartloop(n,m)
    s = 0
    t = @elapsed  for i in 1:n
     a = art(m)
        x = shift!(a)
        unshift!(a,3)
        s += 1
    end
    t
end
