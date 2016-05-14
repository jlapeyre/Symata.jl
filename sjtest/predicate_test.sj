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
