# methods for Julia math functions that operate
# on symbols and Mxpr. Some of these are used
# in Symata code. They can also be used at the Julia
# repl.

# SJSym is an alias of Symbol

# FIXME: These are needed by some reasonable tests in the test suite.
# But, we don't want to define these methods.
# We need to remove dependence on these.
# *(a::Number,b::SJSym) = mxpr(:Times,a,b)
# *(a::SJSym,b::SJSym) = mxpr(:Times,a,b)
# +(a::SJSym,b::Number) = mxpr(:Plus,b,a)

# Symata depends on some of these too. Don't want this.
# Probably depends on all of them. But there I am not sure
# there is a test for each one.
*(a::SJSym,b::Number) = mxpr(:Times,b,a)
+(a::SJSym,b::SJSym) = mxpr(:Plus,a,b)
+(a::Number,b::SJSym) = mxpr(:Plus,a,b)
^(base::SJSym,expt::Integer) = mxpr(:Power,base,expt)
^(base::SJSym,expt) = mxpr(:Power,base,expt)

#### Useful and OK methods

*(a::Mxpr,b::Mxpr) = mxpr(:Times,a,b)
*(a::Mxpr,b) = mxpr(:Times,a,b)
*(a,b::Mxpr) = mxpr(:Times,a,b)


+(a::Mxpr,b::Mxpr) = mxpr(:Plus,a,b)
+(a::Mxpr,b) = mxpr(:Plus,a,b)
+(a,b::Mxpr) = mxpr(:Plus,a,b)

-(a,b::Mxpr) = mxpr(:Plus,a,mxpr(:Times,-1,b))
^(base::Mxpr,expt::Integer) = mxpr(:Power,base,expt)
^(base::Mxpr,expt) = mxpr(:Power,base,expt)

/(a::Mxpr,b) = mxpr(:Times,a,mxpr(:Power,b,-1))
