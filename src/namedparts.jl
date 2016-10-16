## Julia-level access of parts (arguments) of Mxpr's by name.
#
# First, recall that Mxpr expressions have the form h(a,b,c,d...)
# Parts can be accessed from the Julia level by eg  h[1] --> a
# They can also be accesed by eg margs(h(a,b,c,d))[1]
# The arguments (parts) have varying semantic interpretation depending on the head 'h'.
# We can get cheap, consistent, semantic access to the parts using multiple dispatch.
# For example if p is an instance of Mxpr{:Power}, we can write base(p) to get the base.
# This works for getting, but not for setting yet.

# There may be a performance penalty, in some cases, for getting the
# args field of the Mxpr each time named access is used, rather
# getting the args once and then indexing directly into them.

Base.base(p::Mxpr{:Power}) = p[1]
expt(p::Mxpr{:Power}) = p[2]
exponent(p::Mxpr{:Power}) = p[2]

expr(p::Mxpr{:Part}) = p[1]
inds(p::Mxpr{:Part}) = (a = margs(p); view(a,2:length(a)))

# We used these a bit
terms(s::Mxpr{:Plus}) = margs(s)
factors(p::Mxpr{:Times}) = margs(p)
