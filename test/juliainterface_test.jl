using Symata
using Base.Test

@sym ClearAll(a,r,re,r1,m)

##
r = mplus(:a, :a)

@test length(r) == 2

## the addition is unevaluated
@test (r[1],r[2]) == (:a,:a)

@test mhead(r) == :Plus

## evaluate the expression.
re = symeval(r)

@test mhead(re) == :Times
@test (re[1],re[2]) == (2,:a)

@test mplus(:a, :a) == Plus(:a,:a)

@sym a = 1
@test getsymata(:a) == 1

setsymata(:a, 3)
@test getsymata(:a) == 3

m = unpacktoList(linspace(0,1,10))
@test mhead(m) == :List
@test Length(m) == 10

symparseeval("""
             zz = 3
             yy = q + r
             """)

@test getsymata(:zz) == 3
@test getsymata(:yy) == @sym q + r

