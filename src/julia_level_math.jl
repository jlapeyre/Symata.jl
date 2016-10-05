# methods for Julia math functions that operate
# on symbols and Mxpr. Some of these are used
# in Symata code. They can also be used at the Julia
# repl.

*(a::SJSym,b::SJSym) = mxpr(:Times,a,b)
*(a::SJSym,b::Number) = mxpr(:Times,b,a)
*(a::Number,b::SJSym) = mxpr(:Times,a,b)
*(a::Mxpr,b::Mxpr) = mxpr(:Times,a,b)
*(a::Mxpr,b) = mxpr(:Times,a,b)
*(a,b::Mxpr) = mxpr(:Times,a,b)

+(a::SJSym,b::SJSym) = mxpr(:Plus,a,b)
+(a::SJSym,b::Number) = mxpr(:Plus,b,a)
+(a::Number,b::SJSym) = mxpr(:Plus,a,b)
+(a::Mxpr,b::Mxpr) = mxpr(:Plus,a,b)
+(a::Mxpr,b) = mxpr(:Plus,a,b)
+(a,b::Mxpr) = mxpr(:Plus,a,b)

-(a,b::Mxpr) = mxpr(:Plus,a,mxpr(:Times,-1,b))
^(base::Mxpr,expt::Integer) = mxpr(:Power,base,expt)
^(base::Mxpr,expt) = mxpr(:Power,base,expt)
^(base::SJSym,expt::Integer) = mxpr(:Power,base,expt)
^(base::SJSym,expt) = mxpr(:Power,base,expt)

/(a::Mxpr,b) = mxpr(:Times,a,mxpr(:Power,b,-1))
