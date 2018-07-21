## This file is not used

struct SIndex
    inds::Array{Int,1}
    lims::Array{Int,1}
end

SIndex() = SIndex(Array{Int}(undef, 0), Array{Int}(undef, 0))

function atlevel(lev,func,test)
end
