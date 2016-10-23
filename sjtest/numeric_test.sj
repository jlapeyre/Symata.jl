ClearAll(approxeq, f, f1, x)

approxeq(x_, y_) := Abs(x-y) < 1.0*10^(-10)

# Different ways to give integrand to NIntegrate

# Explicitly compile Symata to Julia function
T  approxeq( NIntegrate(Compile(x^2), [0,2])[1], 8.0/3)

# Wrap Symata expression
   f(x_) := x^2
T  approxeq(NIntegrate(f(x), [x,0,2])[1], 8.0/3)

# Wrap Symata expression
T  approxeq(NIntegrate(x^2, [x,0,2])[1], 8.0/3)

# Use Function compiled from Julia code
   f1 = :( x -> x^2)
T  approxeq(NIntegrate(f1, [0,2])[1], 8.0/3)   


ClearAll(approxeq, f, f1, x)
