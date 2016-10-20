### Pack

a = Pack([1,2,3])

T  If( BigIntInput() , :( eltype(Symata.symval(:a)) == BigInt ), :( eltype(Symata.symval(:a)) == Int ))

#T :( eltype(Symata.symval(:a)) == Int )

a = Pack([1,2,3.0])
T :( eltype(Symata.symval(:a)) == Real )

a = Pack([1,2,3.0, "cat"])
T :( eltype(Symata.symval(:a)) == Any )

:(eval(Main, :( b = 1 )))

T JVar(b) == 1

T Length(Unpack( :( [1, 2, 3] ))) == 3
T Head(Unpack( :( [1, 2, 3] ))) == List

ClearAll(a)

