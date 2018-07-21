T InexactNumberQ(1) == False
T InexactNumberQ(1.0)
T InexactNumberQ(1.0 + I)
T InexactNumberQ(BI(1)) == False
T InexactNumberQ(a) == False
T InexactNumberQ(a+1.0) == False

ClearAll(a)

T IntegerQ(1)
T IntegerQ(a) == False
T NumberQ(2.0)
T NumberQ(2)
T NumberQ(1 + I)
T NumberQ(I)
T NumberQ(1/2)
T NumberQ(2^(1/2)) == False
T NumericQ(2^(1/2))

T AtomQ(1)
T AtomQ(1.0)
T AtomQ(1/2)
T AtomQ(1+I)
T AtomQ(I)
T AtomQ(2.0 * 3.0 *I)
T AtomQ(a)
T AtomQ("cat")
T AtomQ(2^(1/2)) == False
T AtomQ(a + b) == False
T AtomQ(a)
T AtomQ(Pi)

T PermutationQ([1,2,3])
T PermutationQ([3,2,1])
T Not(PermutationQ([3,2,3]))
T Not(PermutationQ([x,2,3]))

ClearAll(a,b)

T Positive(1)
T Positive(1/3)
T Not(Positive(-1))
T Not(Positive(I))
T Positive("cat") == Null
T Head(Positive(a)) == Positive

T VectorQ(a) == False
T VectorQ([a,b,c])
T VectorQ([a,b,[c]]) == False
T VectorQ([a,b,c], IntegerQ) == False
T VectorQ([1,2,3], IntegerQ)

T VectorQ([[a,2],[a,b,c],[c]], ListQ)

T Element(1,Integers)
T ! Element(1.1,Integers)
T Element(1.1,Reals)
T Element(1.1,Complexes)
T Element(E,Complexes)
T ! Element(E,Rationals)
T ! Element(GoldenRatio,Rationals)
T Element(GoldenRatio,Algebraics)
T Element(I,Algebraics)
T Map( Function(x, Element(Sqrt(2), x)), [Complexes, Algebraics, Reals, Rationals, Integers, Primes]) == [True,True,True,False,False,False]
T Element(3*Sqrt(3),Algebraics)
T Head(Element(a*Sqrt(3),Algebraics)) == Element

ClearAll(a,b,c)
