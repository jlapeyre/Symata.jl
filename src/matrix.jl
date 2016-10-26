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
@doap function Dot(u::List,v::List)
    symjlength(u) == 0 || symjlength(v) == 0 && return mx
    vv = vectorq(v)
    if vectorq(u) && vv
        return  mxpr(:Plus, [mxpr(:Times, x[1], x[2]) for x in zip(margs(u), (margs(v)))]...)
    end
    if matrixq(u) && vv && symjlength(u[1]) == symjlength(v)
       return matmulvec(u,v)
    end
    mx
end

"""
    matmulvec(m,v)

Matrix multiply matrix `m` and vector `v`.
"""
function matmulvec(m,v)
    n1 = symjlength(m)
    n2 = symjlength(m[1])
    a = newargs(n1)
    for i=1:n1
        a[i] = 0
        for j=1:n2
            a[i] += m[i,j] * v[j]
        end
    end
    mxpr(:List,a...)
end

### IdentityMatrix

@mkapprule IdentityMatrix

@sjdoc IdentityMatrix """
    IdentityMatrix(n)

return the `n x n` identity matrix.
"""
@doap function IdentityMatrix(n::Integer)
    r = newargs(n)
    for i=1:n
        c = newargs(n)
        fill!(c,0)
        r[i] = mxpr(:List,c...)
    end
    mr = mxpr(:List,r...)
    for i=1:n
        mr[i,i] = 1
    end
    mr
end


### Transpose

@mkapprule Transpose

@sjdoc Transpose """
    Transpose(m)

return the transpose of the matrix `m`.
"""

@doap function Transpose(x::Mxpr{:List})
    matrixq(x) || return mx
    a = margs(x)
    n = length(a)
    m = symjlength(a[1])
    a0 = newargs(m)
    for i in 1:m
        a1 = newargs(n)
        for j in 1:n
#            a1[j] = margs(margs(x)[j])[i]
            a1[j] = x[j,i]
        end
        a0[i] = mxpr(:List,a1...)
    end
    mxpr(:List,a0...)
end

### Outer

### EigenValues

### Tr

# assumes we have a matrix
matrixdims(m::List) = (length(m),length(m[1]))

@mkapprule Tr

@sjdoc Tr """
    Tr(m)

Compute the trace of matrix `m`.
"""
@doap function Tr(m::List)
    matrixq(m) || return mx
    n = min(matrixdims(m)...)
    a = newargs(n)
    for i=1:n
        a[i] = m[i,i]
    end
    mxpr(:Plus,a...)
end


### Eigenvalues

@mkapprule Eigenvalues :nargs => 1

@sjdoc Eigenvalues """
    Eigenvalues(m)

Compute the eigenvalues of matrix `m`.
"""
@doap function Eigenvalues(expr)
    arg1 = _sjtopy(mx[1])
    mat = sympy[:Matrix](arg1)
    pyres = mat[:eigenvals]()
    eigdict = pyres |> pytosj
    a = newargs()
    for (k,v) in eigdict
        push!(a, mxpr(:List,k,v))
    end
    mxpr(:List,a...)
end
