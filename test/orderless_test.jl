using Base.Test

# Restore these after fixing
# let s
#     s = @sj(1 + a + b + c)
#     s[1] = :z
#     margs(s) == @sj([a,b,c,z])  # terms are sorted
#     margs(s)[1] = q
#     margs(s) == @sj([q,b,c,z])  # terms are not sorted

#     s = @sj(2 * a * b * c)
#     s[2] = :z
#     margs(s) == @sj([2,b,c,z])  # terms are sorted
#     margs(s)[1] = q
#     margs(s) == @sj([q,b,c,z])  # terms are not sorted    
# end
