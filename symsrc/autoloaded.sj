#### This code is autoloaded when trigger symbols are encountered when reading input.

### Through

Unprotect(Through)
Through(h_(a__)(args__),h_) := h(Apply(Sequence,Map( ηηη -> Apply(ηηη,[args]), [a])))
Through(h_(a__)(args__)) := h(Apply(Sequence,Map( ηηη -> Apply(ηηη,[args]), [a])))
Protect(Through)

### Operate

Unprotect(Operate)
Operate(p_, f_(x__)) := p(f)(x)
Protect(Operate)

### ExpToTrig

Unprotect(ExpToTrig)
ExpToTrig(ex_) := ReplaceRepeated( ex , E^(x_) => Cosh(x) + Sinh(x))
Protect(ExpToTrig)

### Array

Unprotect(Array)

Array(f_, n_Integer) := Map(f,Range(n))
Array(f_, n_Integer, r_Integer) := Map(f,Range(r,n+r-1))
Array(f_, lims_List) := Module([vars = Array(x -> Unique(), Length(lims))],
                               (Null,
                                Table(Evaluate(Apply(f,vars)), Evaluate(Splat(Transpose([vars,lims]))))))
Array(f_, n_Integer, [a_,b_]) := Map(f, Range(a,b,(b-a)/n))

## TODO: reimplement these as above.
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

## Note: this is wrong for many `seq's`
Unprotect(TakeDrop)
TakeDrop(x_, seq_) := [Take(x,seq), Drop(x,seq)]
TakeDrop(x_, seq1_, seq2_) := [Take(x,seq1,seq2), Drop(x,seq1,seq2)]
Protect(TakeDrop)

### ArrayDepth

Unprotect(ArrayDepth)
ArrayDepth(x_) := Length(Dimensions(x))
Protect(ArrayDepth)

### TensorRank

Unprotect(TensorRank)
TensorRank(x_) := Length(Dimensions(x))
Protect(TensorRank)

### Divide

Unprotect(Divide)
Divide(x_, y_) := x/y
Protect(Divide)

## ListCorrelate and ListConvolve are ok here for symbolic elements. For numeric, they are very slow.
## These are mostly for demonstration.

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

### NestWhile

Unprotect(NestWhile)
NestWhile(f_,ex_,test_) := Module([res=f(ex)], (Null, While( test(res), res = f(res)), res))
Protect(NestWhile)

### NestWhileList

Unprotect(NestWhileList)
NestWhileList(f_,ex_,test_) := Module([res=f(ex), list], (list=[ex,res], While( test(res),( res = f(res), Push!(list,res))  ), list))
Protect(NestWhileList)

### NextPrime

Unprotect(NextPrime)
NextPrime(x_Integer) := NestWhile( zz -> zz + 1 , x, yy -> Not(PrimeQ(yy)))
Protect(NextPrime)

### FixedPoint

Unprotect(FixedPoint)

FixedPoint(f_,ex_) := Module([res = f(ex), res1],
                             (While(
                                    res !== res1,
                                    (res1 = res,
                                     res = f(res1))) , res))

FixedPoint(f_,ex_, max_) := Module([res = f(ex), res1, n = 0],
                             (While(
                                    res !== res1,
                                    (
                                     n += 1,
                                     If(n>max,Break()),
                                     res1 = res,
                                     res = f(res1))) , res))


FixedPoint(f_,ex_, Rule(SameTest, test_)) := FixedPoint(f,ex,Infinity,Rule(SameTest, test_))

FixedPoint(f_,ex_, max_, Rule(SameTest, test_)) := Module([res = f(ex), res1, n = 0],
                             (While(
                                    ! test(res,res1),
                                    (
                                     n += 1,
                                     If(n>max,Break()),
                                     res1 = res,
                                     res = f(res1))) , res))

Protect(FixedPoint)
