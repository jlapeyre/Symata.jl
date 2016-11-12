#### This code is autoloaded when trigger symbols are encountered when reading input.

### ExpToTrig

Unprotect(ExpToTrig)

ExpToTrig(ex_) := ReplaceRepeated( ex , E^(x_) => Cosh(x) + Sinh(x))

Protect(ExpToTrig)


### Array

Unprotect(Array)

# We wrap these in Module just to prevent `i` from being entered as a symbol. But, this still pollutes
# the symbol table with gensyms. The mechanism for removing them automatically is not working.

Array(f_, n_Integer) := Module([i], (Null, Table(f(i), [i,n])))

Array(f_, n_Integer, r_Integer) := Module([i ],  (Null, Table(f(i), [i,r,n+r-1])))

Array(f_, [n_Integer, m_Integer]) := Module([i ],  (Null, Table(f(i,j), [i,n], [j,m])))

Array(f_, [n_Integer, m_Integer, p_Integer]) := Module([i ],  (Null, Table(f(i,j,k), [i,n], [j,m], [k,p])))

Array(f_, n_Integer, [a_,b_]) := Module([i ],  (Null, Table(f(i), [i, Range(a,b,(b-a)/n)])))

Array(f_, [n1_Integer, n2_Integer], [r1_Integer, r2_Integer] ) := Module([i ],  (Null, Table(f(i,j), [i,r1,n1+r1-1], [j,r2,n2+r2-1])))

Array(f_, [n1_Integer, n2_Integer, n3_Integer], [r1_Integer, r2_Integer, r3_Integer] ) :=
    Module([i ],  (Null, Table(f(i,j,k), [i,r1,n1+r1-1], [j,r2,n2+r2-1],  [k,r3,n3+r3-1])))

Array(f_, [n1_Integer, n2_Integer], [[a1_,b1_], [a2_,b2_] ] ) := Module([i ],  (Null, Table(f(i,j), [i, Range(a1,b1,(b1-a1)/n1)], [j, Range(a2,b2,(b2-a2)/n2)])))

Protect(Array)


### Subdivide

@sjdoc Subdivide """
    Subdivide(n)

is equivalent to `Range(0,n)/n`.

    Subdivide(xmax, n)

is equivalent to `xmax*Range(0,n)/n`

    Subdivide(xmin, xmax, n)

is equivalent to `xmin + (xmax-xmin)*Range(0,n)/n`.
"""

Unprotect(Subdivide)

Subdivide(n_) := Range(0,n)/n

Subdivide(xmax_, n_) := xmax*Range(0,n)/n

Subdivide(xmin_, xmax_, n_) := xmin + (xmax-xmin)*Range(0,n)/n

Protect(Subdivide)


### TakeDrop

Unprotect(TakeDrop)

TakeDrop(x_, seq_) := [Take(x,seq), Drop(x,seq)]
TakeDrop(x_, seq1_, seq2_) := [Take(x,seq1,seq2), Drop(x,seq1,seq2)]

Protect(TakeDrop)

### ArrayDepth

Unprotect(ArrayDepth)

ArrayDepth(x_) := Length(Dimensions(x))

# We want to protect ArrayDepth, but this causes a mysterious error.
Protect(ArrayDepth)

### TensorRank

Unprotect(TensorRank)

TensorRank(x_) := Length(Dimensions(x))

Protect(TensorRank)


### UnsameQ

Unprotect(UnsameQ)

UnsameQ(x_,y_) := Not(SameQ(x,y))

Protect(UnsameQ)

### SameQ

Unprotect(SameQ)

SameQ(x_,y_) := x === y
SameQ(x_BigInt,y_BigInt) := x == y

Protect(SameQ)

### Unequal

Unprotect(Unequal)

Unequal(x_,y_) := Not( Equal(x,y) )

Protect(Unequal)

### Equal

Unprotect(Equal)

Equal(x_,y_) := x == y

Protect(Equal)

### Divide

Unprotect(Divide)

Divide(x_, y_) := x/y

Protect(Divide)

## ListCorrelate and ListConvolve are ok here for symbolic elements. For numeric, they are very slow

### ListCorrelate

Unprotect(ListCorrelate)

ListCorrelate(ker_List, list_List) := Module([pl,zzz],
                                    (
                                     pl = Partition(list, Length(ker), 1),
                                     Map( Function(zzz, Dot(ker,zzz)), pl)))

Protect(ListCorrelate)


### ListConvolve

Unprotect(ListConvolve)

ListConvolve(ker_List, list_List) := Module([pl,zzz,rker],
                                    (
                                     pl = Partition(list, Length(ker), 1),
                                     rker = Reverse(ker),
                                     Map( Function(zzz, Dot(rker,zzz)), pl)))

Protect(ListConvolve)

### Accumulate

Unprotect(Accumulate)

Accumulate(list_) := Rest(FoldList(Plus,0,list))

Protect(Accumulate)
