# Contexts are only partially implemented.

T testUserSyms

# comparisons
T a.b == a.b
T a.b <= a.b
T a.b >= a.b
T Args(a.b == a.c) == List(a.b, == , a.c)

# canonical ordering
T Args(a.a + z) == [z,a.a]


# sympy translation
T Expand( (a + b.c)^3 ) == a^3 + 3*a*(b.c^2) + b.c^3 + 3*(a^2)*b.c

      b.c = 1
T  b.c == 1
 Clear(b.c)
T Args(b.c == 1) == [b.c,==,1 ]

 ClearAll(b.c,a,z,a.a,a.b,a.c)

   b.d = 1
T ContextSymbols(b)  == ["d"]
 ClearAll(b.d)
T ContextSymbols(b)  == []
T Args(b.d == 1) == [b.d,==,1 ]



 ClearAll(b.c,b.d,b)
