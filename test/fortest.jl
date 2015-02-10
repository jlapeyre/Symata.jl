# much faster than SJulia For. Not so convenient
function addtom(n)
    # setpart no longer exists
    for i in 1:1000  doeval( mxpr(:SetPart, :(m) , i,  i + n)) end
end

