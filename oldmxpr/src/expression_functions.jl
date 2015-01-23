############################################
## Expression manipulation                 #
############################################

function Base.reverse(mx::Mxpr)
    mx1 = deepcopy(mx)
    Base.reverse!(margs(mx1))
    return mx1
end
Base.reverse(mx::Orderless) = deepcopy(mx)
Base.reverse!(mx::Mxpr) = (reverse!(margs(mx)); mx)
Base.reverse!(mx::Orderless) = mx

# Test orderless ordering. This is not for normal use.
function testreverse(mx::Mxpr)
    mx1 = deepcopy(mx)
    reverse!(mx1.args)
    sortiforderless!(mx1)
    mx1
end

# Make random permutation of Orderless Mxpr
# and test if permutation is the same  (it won't
# be for large expressions
function test_rand_orderless(mx::Orderless)
    mx1 = deepcopy(mx)
    permute!(mx1.args,randperm(length(mx1)))
    return mx1.args == mx.args
end

# Input what we think is canonically ordered.
# Random permute and sort and see if we get
# the inpute back.
function test_order_rand_orderless(mx::Orderless)
    mx1 = deepcopy(mx)
    permute!(mx1.args,randperm(length(mx1)))
    sortiforderless!(mx1)
    return mx1.args == mx.args
end

# How many times does sorting fail ?
function test_orderless(mx::Orderless, n::Int)
    s = 0
    for i in 1:n
        if ! test_order_rand_orderless(mx) s += 1 end
    end
    s
end
