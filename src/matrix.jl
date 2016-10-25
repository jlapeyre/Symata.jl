"""
    sympy_matrix_to_sj(spmat::[ a sympy matrix object ])

convert spmat to a Symata `List` of `Lists`.
"""
function sympy_matrix_to_sj(spmat)
    jlist = pytosj(spmat[:tolist]())
    for i in 1:length(jlist)
        jlist[i] = pytosj(jlist[i])
    end
    mat = newargs()
    (nx,ny) = size(jlist)
    for i in 1:nx
        push!(mat, mxpr(:List, jlist[i,:]...))
    end
    mxpr(:List,mat)
end


### Inverse

@mkapprule Inverse :nargs => 1

@sjdoc Inverse """
    Inverse(m)

compute the matrix inverse of `m`.
"""
@doap function Inverse(expr)
    arg1 = _sjtopy(mx[1])
    mat = sympy[:Matrix](arg1)
    pyres = mat[:inv]()
    sympy_matrix_to_sj(pyres)
end

### Dot

@mkapprule Dot

@sjdoc Dot """
    Dot(u,v)
    u ⋅ v
    ⋅(u,v)

return the inner product of vectors `u` and `v`. The complex conjugate is
applied to neither `u` nor `v`.

The infix operator `⋅` can usually be entered by typing `\\cdot` followed by `TAB`
"""
@doap Dot(u::List,v::List) =  mxpr(:Plus, [mxpr(:Times, x[1], x[2]) for x in zip(margs(u), margs(v))]...)
