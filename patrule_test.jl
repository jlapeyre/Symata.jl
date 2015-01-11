using Base.Test

# syntax for constructing a rule
@test  (:a => :b)  == PRule(:a,:b)

# match literal expression; no pattern
@test patrule( :( a ), :( a ), :( b ) ) == :b

# no match
@test patrule( :( a ), :( z ), :( b ) ) == false

# tpatrule acts as identity if there is no match
@test tpatrule( :( a ), :( z ), :( b ) ) == :a

# match pattern and replace
@test patrule( :( a * a ) , :( x_ * x_ ) , :( x_^2 )) == :(a ^ 2)

# same as tpatrule, but second arguement is a PRule
@test replace( :(a*a) , :(x_*x_)  =>  :(x_^2) ) == :(a^2)

sq_rule = :(x_*x_)  =>  :(x_^2)

# The entire expression is tested, not subexpressions
@test replace( :(a*a+1) , sq_rule) == :(a*a+1)

# Subexpressions are tested. Depth first.
# This differs from Mma, which applies rule top-down and
# stops if there is a success.
@test replaceall(:(a*a+1) , sq_rule) == :(a^2+1)

# applies everywhere
@test replaceall(:(a*a+1 / ((z+y)*(z+y))) , sq_rule) == :(a^2+1/(z + y)^2)

# Note depth first, rather than top level first and then stopping
@test replaceall(:( (a*a) * (a*a) ) , sq_rule) == :((a^2)^2)

## Examples

mulpow_rule = :(x_^n1_ * x_^n2_) => :(x_^(n1_+n2_))

@test replace(:(a^2 * a^3 ), mulpow_rule) == :(a ^ (2 + 3))

@test replaceall(:( (a^2 * a^3)*a^4 ), mulpow_rule) == :(a ^ ((2 + 3)+4))

inv_rule = :(1/(1/x_)) => :(x_)

@test replace( :( 1/(1/(a+b)) ), inv_rule) == :(a+b)
