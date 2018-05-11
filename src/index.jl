immutable SIndex
    inds::Array{Int,1}
    lims::Array{Int,1}    
end

SIndex() = SIndex(Array{Int}(0),Array{Int}(0))

function atlevel(lev,func,test)

end
