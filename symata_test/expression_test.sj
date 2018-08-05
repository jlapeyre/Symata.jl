ClearAll(a,b)
ClearTemporary()

T b^0 == 1
T b^1 == b

T b*1 == b
T 1*b == b
T -(a+b) == -a - b  # lhs is expanded
T -1*(a+b) == -a - b

ClearAll(a,b)

T Chop(Abs(Apply(Plus,J(range(1, stop=10, length=100))) - 550.0)) == 0
T Chop(Abs(Apply(Times,J(range(1, stop=3.0, length=10))) - 583.0397134081362)) == 0

  mx = ExpandA((a+b)^3)
T mx == Plus(Power(a,3),Times(3,Power(a,2),b),Times(3,a,Power(b,2)),Power(b,3))
T FixedQ(mx) == true
  a = 1
  mx
T FixedQ(mx) == false
  ClearAll(a)
  mx
T FixedQ(mx) == true

  ClearAll(a,b,c,d,z)
T ExpandA(1) == 1
T ExpandA(a) == a
T ExpandA(a+b) == a+b
T ExpandA(a*b) == a*b
T (ExpandA(a*(b+c)) == a*b + a*c) == True
T ExpandA(z*(b+c)) == b*z + c*z  # tests diffent branch in doexpand because z < a,b,c.
T ExpandA(3*(b+c)) == 3*b + 3*c
T ExpandA(z*(c+d)*(a+b)) == a*c*z + b*c*z + a*d*z + b*d*z
T ExpandA((a+1)^3) == 1 + 3*a + 3*(a^2) + a^3
T ExpandA((a+b)*(c+d)) == a*c + b*c + a*d + b*d
T Apply(List,ExpandA(z*a*(c+d)*(a+b))) == [(a^2)*c*z,a*b*c*z,(a^2)*d*z,a*b*d*z]
  ClearAll(a,b,c,d,z,mx)

 ClearAll(m,f,g)
 m = [1,2,[3,4,5,f(6,7)]]
T m[2] == 2
T m[0] == List
T m[3] == [3,4,5,f(6,7)]
  m[1] = "cat"
T m == ["cat",2,[3,4,5,f(6,7)]]
  m[3,4,0] = g
T m == ["cat",2,[3,4,5,g(6,7)]]
  m[-2] = 88
T m == ["cat",88,[3,4,5,g(6,7)]]
T m[-1,-1] == g(6,7)
  ClearAll(m,f,g)

  ClearAll(f,a,b,g,c,d,m)
  m = f(a,b,g(c(d,b),a(a,b(c(d)))))
T Position(m,f) == [[0]]
T Position(m,a) == [[1],[3,2,1],[3,2,0]]
T Position(m,b) == [[2],[3,1,2],[3,2,2,0]]
T Position(m,c(d)) == [[3,2,2,1]]
  m = f(a,b,g(c(d,b),a(a,b(c(d)))),c(d,b))
T Position(m, c(d,b)) == [[3,1],[4]]
T Position([1 + x^2, 5, x^4, a + (1 + x^2)^2], x^_) == [[1,2],[3],[4,2,1,2]]

ClearAll(f,a,b,g,c,d,m)

### Span
  ClearAll(m)
  m = Range(10)
  m[3] = Range(10)*4
T m[3,4:6] == [16,20,24]
T m[3,1:10:2] == [4,12,20,28,36]
  ClearAll(m)

### ReleaseHold

## FIXME:
## These only worked with old Comparison, because they
## evaluate to Comparison(==) --> True
## We need to find another way to test this
# T ReleaseHold(Hold()) == Sequence()
# T ReleaseHold(HoldForm()) == Sequence()
# T ReleaseHold(HoldPattern()) == Sequence()
# T ReleaseHold(HoldComplete()) == Sequence()

T ReleaseHold(Hold(a)) == a
T List(ReleaseHold(Hold(a,b))) == [a,b]
T ReleaseHold(f(a,b)) == f(a,b)

ClearAll(a,b,f)

a = Range(10)

T Push!(a,11) == Range(11)
T Pop!(a) == 11
T a == Range(10)

### Through

T Through((f+g+h)(x,y)) == f(x,y) + g(x,y) + h(x,y)
T Through([f,g,h](x)) == [f(x),g(x),h(x)]
T Through(r(a,b,c)(x,y,z)) == r((a(x,y,z)),(b(x,y,z)),c(x,y,z))
T NestList(Through, f(a)(b)(c)(d), 3) == [f(a)(b)(c)(d),f(a)(b)(c(d)),f(a)(b(c(d))),f(a(b(c(d))))]

### Operate

T Operate(p, f(x,y)) == p(f)(x,y)
T Nest(xx -> Operate(wrap,xx), f(x), 4) == wrap(wrap(wrap(wrap(f))))(x)
T Operate( Function(x,g), f(a,b,c)) == Apply(g, f(a,b,c))
T Through(Operate(p,f(x))) == p(f(x))
T Operate((xx->xx(a)), f(x,y)) == f(a)(x,y)

### Head

# Fails unless head(s::SJSym) = :Symbol defined in expressions.jl
# But, see the note there.
ClearAll(a)
T Head(a) == Symbol
