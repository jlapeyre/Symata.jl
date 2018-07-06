mutable struct A
    x::Int
end

mutable struct B
    x::Int
end

mutable struct C
    x::Int
end

const AB = Union(A,B)
const BC = Union(B,C)

f(x::AB) = 1
g(x::BC) = 2

