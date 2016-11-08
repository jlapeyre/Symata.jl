## methods for Julia math functions that operate
## on symbols and Mxpr. Some of these are used
## in Symata code. They can also be used at the Julia
## repl. Note, using mmul, mplus, mpow is just as good,
## and is defined for more objects.

# SJSym is an alias of Symbol

# """
#     symatamath()

# define math methods that operate on symbols. This allows Julia expressions such as
# ```
# :a + :b
# :c^2
# ```

# These methods are disabled by default because they extend `Base` methods for `Base` types. These methods are
# in general reserved for definition in future versions of Julia.
# """
# function symatamath()
#     @eval begin
#         *(a::Number,b::SJSym) = mxpr(:Times,a,b)  # why not mmul ?
#         *(a::SJSym,b::SJSym) = mxpr(:Times,a,b)
#         +(a::SJSym,b::Number) = mxpr(:Plus,b,a)
#         *(a::SJSym,b::Number) = mxpr(:Times,b,a)
#         +(a::SJSym,b::SJSym) = mxpr(:Plus,a,b)
#         +(a::Number,b::SJSym) = mxpr(:Plus,a,b)
#         ^(base::SJSym,expt::Integer) = mxpr(:Power,base,expt)
#         ^(base::SJSym,expt) = mxpr(:Power,base,expt)
#     end
# end

# #### Useful and OK methods

# *(a::Mxpr,b::Mxpr) = mxpr(:Times,a,b)
# *(a::Mxpr,b) = mxpr(:Times,a,b)
# *(a,b::Mxpr) = mxpr(:Times,a,b)


# +(a::Mxpr,b::Mxpr) = mxpr(:Plus,a,b)
# +(a::Mxpr,b) = mxpr(:Plus,a,b)
# +(a,b::Mxpr) = mxpr(:Plus,a,b)

# -(a,b::Mxpr) = mxpr(:Plus,a,mxpr(:Times,-1,b))
# ^(base::Mxpr,expt::Integer) = mxpr(:Power,base,expt)
# ^(base::Mxpr,expt) = mxpr(:Power,base,expt)

# /(a::Mxpr,b) = mxpr(:Times,a,mxpr(:Power,b,-1))
