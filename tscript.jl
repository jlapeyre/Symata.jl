#include("PatRule.jl")

expr = @replacerepeated  Log( a*((b+b)*c^d)^(1/k) )  [r1,r2,r3]
# :(Log(a) + (1 / k) * ((Log(2) + Log(b)) + d * Log(c)))

@replacerepeated  Log(b^(a - 1/(1/a))) rules
# 0

@replacerepeated  3 *(a^3 * a^2) - 2*a^5 - a * a^4 rules
# 0

@replacerepeated  LambertW(x*exp(x)) / ( LambertW(x) * exp(LambertW(x) ))  rules
# 1

#expr = replacerepeated(expr , @rule Log(x_) => log(x_) )
# :(log(a) + (1 / k) * ((0.6931471805599453 + log(b)) + d * log(c)))




@replacerepeated  (a + a )  + (a + a)  [r4,r3,r5]
# :(2 * (2a))

@replacerepeated  (a + a) + (a + a)  [r4,r3,r5,r6]
# :((2 * 2) * a)
