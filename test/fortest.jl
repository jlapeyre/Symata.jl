# This is an example of writing a function for SJulia in Julia, rather than
# SJulia. This is less general and convenient but runs faster.
#
# sjulia> x = Range(1000);  # x = [1,2,...]
# julia> addtom(11,:x)     # x = [12,13,...]
#
# This is the same thing is SJulia:
# For( i=1, i <= Length(x), i = i + 1, x[i] = x[i] +  11)
#
function addtom(n,m)
    mx = symval(m) # m is a symbol, symval(m) is the SJulia expression
    for i in 1:length(mx)  doeval( mxpr(:Set, mxpr(:Part, mx , i),  i + n)) end
end

