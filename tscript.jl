include("PatRule.jl")

r1 = @rule  log(x_^n_) => n_ * log(x_)
r2 = @rule  log(x_ * y_) => log(x_) + log(y_)
r3 = @rule  _  + _ => 2 * _

@replacerepeated  log( a*((b+b)*c^d)^(1/k) )  [r1,r2,r3]

# :(log(a) + (1 / k) * ((log(2) + log(b)) + d * log(c)))

