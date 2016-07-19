@mkapprule Inverse :nargs => 1

@sjdoc Invserse "
Rewrites(m) computes the matrix inverse of m
"

@doap function Inverse(expr)
    arg1 = _sjtopy(mx[1])
    mat = sympy.Matrix(arg1)
    pyres = mat[:inv]()
    sympy_matrix_to_sj(pyres)
end

# spmat -- a sympy Matrix
# output a List of Lists
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
