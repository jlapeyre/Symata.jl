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

ClearAll(a,b)

T VectorQ(a) == False
T VectorQ([a,b,c])
T VectorQ([a,b,[c]]) == False
T VectorQ([a,b,c], IntegerQ) == False
T VectorQ([1,2,3], IntegerQ)

T VectorQ([[a,2],[a,b,c],[c]], ListQ)

ClearAll(a,b,c)
