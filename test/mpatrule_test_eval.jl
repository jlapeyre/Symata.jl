using Base.Test

# If we eval expressions in patsubst!, then these
# tests must be used. eg.  :(2 + 3) -->  5

# syntax for constructing a rule
@test  (:a => :b)  == PRule(:a,:b)

# match literal expression; no pattern
@test patrule( :a , :a, :b ) == :b

# no match
@test patrule( :a, :z, :b ) == false

# tpatrule acts as identity if there is no match
@test tpatrule( :a , :z, :b ) == :a

# match pattern and replace
@test patrule( :a * :a  , :x_ * :x_ ,  :x_^2 ) == :a ^ 2

# same as tpatrule, but second arguement is a PRule
@test replace( :a * :a , :x_ * :x_ =>  :x_^2 ) == :a^2

# Obsolete, squares are already done now.
# # Can use underscore for capture var.
# sq_rule = :_ * :_  =>  :_^2

# # replace tests the entire expression, not subexpressions
# @test replace(:a * :a + 1 , sq_rule) == :a * :a + 1

# # replaceall tests subexpressions; depth first.
# # This differs from Mma, which applies rule top-down and
# # stops if there is a success.
# @test replaceall(:a * :a + 1, sq_rule) == :a^2+1

# # applies everywhere
# # Fails first time. passes second time.
# # probably because symbols are read twice
# replaceall(@sn(a*a+1 / ((z+y)*(z+y))) , sq_rule) == @sj(a^2+1/(z + y)^2)
# @test replaceall(@sn(a*a+1 / ((z+y)*(z+y))) , sq_rule) == @sj(a^2+1/(z + y)^2)


# These stopped working because @sn no longer prevents ordering!
# Note depth first, rather than top level first and then stopping
# @test replaceall( @sn((a*a) * (a*a)) , sq_rule) == @sn((a^2)^2)

## Examples

mulpow_rule = :x_^:n1_ * :x_^:n2_ => :x_^(:n1_+:n2_)

@test replace(:a^2 * :a^3 , mulpow_rule) == :a^5
@test replace( :a^1.2 * :a^2.3, mulpow_rule) == :a^ 3.5

# you can put a condition on the pattern
@test replace( :a^1.2 * :a^2.3 , @sj(x_^n1_::Int * x_^n2_::Int) => :x_^(:n1_+:n2_)) ==
    :a ^ 1.2 * :a ^ 2.3
@test replace( :a^5 * :a^6 , @sj(x_^n1_::Int * x_^n2_::Int) => :x_^(:n1_+:n2_)) == :a ^11

# This works, but keeps changing
#@test string(cmppat( :( "dog" ) , :( x_::String ))) ==
#    "(true,Any[(Pvar(:x_,AbstractString),\"dog\")])"

# Need to use @sn here to prevent flattening of product
# But, @sn no longer prevents flattening
# We need to devise tests for more complicated replacements
#@test replaceall( @sn((a^2 * a^3)*a^4), mulpow_rule) == :a ^ 9

inv_rule = @sn(1/(1/x_)) => :x_

@test replace( @sn( 1/(1/(a+b)) ), inv_rule) == :a+:b

plusmul_rule =  :x_ + :x_  =>  @sn(2 * x_)
mulpow1_rule = :x_ * :x_   =>  :x_^2

# Tests are tried one at a time until one matches
@test replaceall( :a * :a , [plusmul_rule, mulpow1_rule]) == :a^2
@test replaceall( :a + :a , [plusmul_rule, mulpow1_rule]) == @sn(2*a)

@test replace( @sn( sin(a+b)/cos(a+b) ) , @sn( sin(_) / cos(_) ) => @sn(tan(_) ) ) == @sn(tan(a + b))

# Test anonymous functions as conditions
@test (cmppat1( @sn( 3 ) , @sn( x_::((x)->(x>4)) )))[1] == false
@test (cmppat1( @sn( 3 ) , @sn( x_::((x)->(x>2)) )))[1] == true

@test Pvar( :x_ ) == Pvar(:x_,:All)

# Macro situation should be rethought. These are from the old Expr, pre Mxpr code
## test some macros
# let r1, r2, ex, ex1
#     r1 =    _ + _ => 2 * _ 
#     r1a =  :_ + :_ => 2 * :_
#     @test r1 == r1a
#     ex = @replaceall (a+a) + (a+a)  _ + _ => 2 * _
#     @test ex == :(2 * (2a))    
#     ex = @replaceall  z * z  [ @rule( _ * _ => _^2 ),  @rule(_ + _ => 2 * _ ) ]
#     @test ex == :(z^2)
#     ex = @replaceall  z + z  [ @rule( _ * _ => _^2 ),  @rule(_ + _ => 2 * _ ) ]
#     @test ex == :(2 * z)    
# end

#replaceall( :((a + a)/(z+z)  ),  @rule  _ + _ => 2 * _) == :((2a) / (2z))
# Fails because of new canonical order
# replaceall( @sn((a + a)/(z+z)),   :_ + :_ => 2 * :_) == @sj((2a) / (2z))

# Redo all of this.
# let r,r1
#     r =  @sn( _ / _::((x)-> x != 0))  => 1
#     r1 =  @sn( _^-1 * _::((x)-> x != 0))  => 1    
#     @test replaceall( @sn( 0 / 0) , r) == @sn( 0 / 0 )
#     @test replaceall( @sn( (a+b) / (a+b) ) , r) == 1
# end

# Broken
# use a helper function, iscomplex
#let r
#    r = @sj(Exp(Log(x_::iscomplex))) => :x_
#    @test replaceall( @sj(Exp(Log(1))) , r) == @sj(Exp(Log(1)))
#    @test replaceall( @sj(Exp(Log(complex(1,1)))) , r) == complex(1,1)
#end

# bug fix: Pvars are not hashable
@test cmppat( :(a + b) , :( x_ + x_ ))[1] == false
# bug fixes:  0 == false ->  0 === false
@test replaceall(  :a - :a,   :x_ - :x_ => 0 )  == 0
@test replaceall(  1/:a - :a , :x_- :x_ => 0)  == 1/:a - :a
# Broken
#@test replaceall( 1/:a - 1/:a  , -(:x_) + :x_ => 0 )  == 0
# Following fix old bugs. Some code is completely rewritten now
# This is broken again!!
#@test replaceall( @sn( b^(a-a) ),  :_ + -:_  =>  0) == :b ^ 0

# FIXME: why did this break ?
#@test replaceall( @sn([a,b,c,d]) ,   :a =>  :b)   ==  @sn([b,b,c,d])
# Fixed bug in mx_to_ex! and mx_to_ex. Deep copy was not enough. There are two refs to 'a'
let a = :c + 1
    @test a * a == (:c + 1) * (:c + 1)
end

# Problem with symbol scope
#module TestMod1

@test 1 * :zebra == :zebra * 1 == :zebra
@test 0 + :a == :a + 0 == :a

#end
    

# Fix these when we fix the macros
# replaceall vs. replacerepeated
# let r, res,r1,r2
#     r1 = @rule  log(x_^n_) => n_ * log(x_)
#     r2 = @rule  log(x_*y_) => log(x_) * log(y_)
#     replaceall( :(log(a*x^2)) , [r1,r2]) == :(log(a) * log(x ^ 2))
#     replacerepeated( :(log(a*x^2)) , [r1,r2]) == :(log(a) * (2 * log(x)))
#     replacerepeated( :(log( (a*x^2)*z^(1/k) ))  , [r1,r2]) ==
#         :((log(a) * (2 * log(x))) * ((1 / k) * log(z)))

#     replacerepeated( :( log( a*(b*c^d)^e ) ) , [r1,r2]) ==
#         :(log(a) * (e * (log(b) * (d * log(c)))))
#    These do not work in this let block, but do work on cli. Don't know why
#    res = @replacerepeated  log( (a*x^2)*z^(1/k+1) )  [r1,r2]
#    @test res == :((log(a) * (2 * log(x))) * ((1 / k + 1) * log(z)))
#end


