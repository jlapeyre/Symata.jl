type A
    x::Int
end

type B
    x::Int
end

type C
    x::Int
end

typealias AB Union(A,B)
typealias BC Union(B,C)

f(x::AB) = 1
g(x::BC) = 2

