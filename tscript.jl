#include("PatRule.jl")

r1 = @rule  Log(x_^n_) => n_ * Log(x_)
r2 = @rule  Log(x_ * y_) => Log(x_) + Log(y_)
r3 = @rule  _  + _ => 2 * _
r4 = @rule  x_  + n_ * x_ => (n_+1) * x_
r5 = @rule  n_ * x_ + x_ => (n_+1) * x_
r6 = @rule  n_::Number  * (m_::Number * x_) => (n_*m_) * x_
r7 = @rule  +(x_,x_,x_) => 3 * x_
r8 = @rule  x_ - x_  => 0
r9 = @rule  0 + x_  => x_
r10 = @rule  1/(1/x_) => x_
r11 = @rule  x_ + -x_  => 0
r12 = @rule  x_^n1_ * x_^n2_ => x_^(n1_+n2_)
r13 = @rule  x_^0 => 1
r14 = @rule  0 * x_ => 0
r15 = @rule  x_ * 0  => 0

rules = [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15]

expr = @replacerepeated  Log( a*((b+b)*c^d)^(1/k) )  [r1,r2,r3]
# :(Log(a) + (1 / k) * ((Log(2) + Log(b)) + d * Log(c)))

#expr = replacerepeated(expr , @rule Log(x_) => log(x_) )
# :(log(a) + (1 / k) * ((0.6931471805599453 + log(b)) + d * log(c)))




@replacerepeated  (a + a )  + (a + a)  [r4,r3,r5]
# :(2 * (2a))

@replacerepeated  (a + a) + (a + a)  [r4,r3,r5,r6]
# :((2 * 2) * a)
