#### This code is autoloaded when trigger symbols are encountered when reading input.

### ExpToTrig

Unprotect(ExpToTrig)

ExpToTrig(ex_) := ReplaceRepeated( ex , E^(x_) => Cosh(x) + Sinh(x))

Protect(ExpToTrig)


### Array

Unprotect(Array)

Array(f_, n_Integer) := Table(f(i), [i,n])

Array(f_, n_Integer, r_Integer) := Table(f(i), [i,r,n+r-1])

Array(f_, [n_Integer, m_Integer]) := Table(f(i,j), [i,n], [j,m])

Array(f_, [n_Integer, m_Integer, p_Integer]) := Table(f(i,j,k), [i,n], [j,m], [k,p])

Array(f_, n_Integer, [a_,b_]) := Table(f(i), [i, Range(a,b,(b-a)/n)])

Array(f_, [n1_Integer, n2_Integer], [r1_Integer, r2_Integer] ) := Table(f(i,j), [i,r1,n1+r1-1], [j,r2,n2+r2-1])

Array(f_, [n1_Integer, n2_Integer, n3_Integer], [r1_Integer, r2_Integer, r3_Integer] ) := Table(f(i,j,k), [i,r1,n1+r1-1], [j,r2,n2+r2-1],  [k,r3,n3+r3-1])


Array(f_, [n1_Integer, n2_Integer], [[a1_,b1_], [a2_,b2_] ] ) := Table(f(i,j), [i, Range(a1,b1,(b1-a1)/n1)], [j, Range(a2,b2,(b2-a2)/n2)])

Protect(Array)

### Subdivide

# FIXME: Having these call each other causes evaluation oscillation
Subdivide(n_) := Range(0,n)/n

Subdivide(xmax_, n_) := xmax*Range(0,n)/n

Subdivide(xmin_, xmax_, n_) := xmin + (xmax-xmin)*Range(0,n)/n

Protect(Subdivide)


