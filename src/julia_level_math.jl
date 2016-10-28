# methods for Julia math functions that operate
# on symbols and Mxpr. Some of these are used
# in Symata code. They can also be used at the Julia
# repl.

# SJSym is an alias of Symbol

## The following could be used optionally as a convenience for
## the user. But, they are disabled because they extend Base methods for Base objects.

# *(a::Number,b::SJSym) = mxpr(:Times,a,b)
# *(a::SJSym,b::SJSym) = mxpr(:Times,a,b)
# +(a::SJSym,b::Number) = mxpr(:Plus,b,a)

# *(a::SJSym,b::Number) = mxpr(:Times,b,a)
# +(a::SJSym,b::SJSym) = mxpr(:Plus,a,b)
# +(a::Number,b::SJSym) = mxpr(:Plus,a,b)
# ^(base::SJSym,expt::Integer) = mxpr(:Power,base,expt)
# ^(base::SJSym,expt) = mxpr(:Power,base,expt)

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
