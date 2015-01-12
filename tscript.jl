r1 = @rule  log(x_^n_) => n_ * log(x_)
r2 = @rule  log(x_ * y_) => log(x_) * log(y_)

@replacerepeated  log( a*(b*c^d)^e )  [r1,r2]

# :(log(a) * (e * (log(b) * (d * log(c)))))
