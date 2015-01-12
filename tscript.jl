#include("PatRule.jl")

r1 = @rule  log(x_^n_) => n_ * log(x_)
r2 = @rule  log(x_ * y_) => log(x_) + log(y_)
r3 = @rule  _  + _ => 2 * _
r4 = @rule  x_  + n_ * x_ => (n_+1) * x_
r5 = @rule  n_ * x_ + x_ => (n_+1) * x_
r6 = @rule  n_::Number  * (m_::Number * x_) => (n_*m_) * x_
r7 = @rule  +(x_,x_,x_) => 3 * x_

@replacerepeated  log( a*((b+b)*c^d)^(1/k) )  [r1,r2,r3]
# :(log(a) + (1 / k) * ((log(2) + log(b)) + d * log(c)))

@replacerepeated  (a + a )  + (a + a)  [r4,r3,r5]
# :(2 * (2a))

@replacerepeated  (a + a) + (a + a)  [r4,r3,r5,r6]
# :((2 * 2) * a)
