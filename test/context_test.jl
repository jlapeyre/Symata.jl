# Contexts are only partially implemented.

@testex testUserSyms

# comparisons
@testex a.b == a.b
@testex a.b <= a.b
@testex a.b >= a.b
@testex Args(a.b == a.c) == List(a.b, == , a.c)

# canonical ordering
@testex Args(a.a + z) == [z,a.a]


# sympy translation
@testex Expand( (a + b.c)^3 ) == a^3 + 3*a*(b.c^2) + b.c^3 + 3*(a^2)*b.c

@ex      b.c = 1
@testex  b.c == 1
@ex Clear(b.c)
@testex Args(b.c == 1) == [b.c,==,1 ]

@ex ClearAll(b.c,a,z,a.a,a.b,a.c)

@ex   b.d = 1
@testex ContextSymbols(b)  == ["d"]
@ex ClearAll(b.d)
@testex ContextSymbols(b)  == []
@testex Args(b.d == 1) == [b.d,==,1 ]



@ex ClearAll(b.c,b.d,b)
