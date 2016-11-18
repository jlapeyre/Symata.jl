T -1^n == Times(-1,Power(1,n))
T (-1)^n == Power(-1,n)

ar = [
     1
     a + b
     "cat"

     f(x)
    ]

T ar == [1,a + b,"cat",f(x)]

## Compound expression

## Must have commas here
ar1 = (
     1,
     a + b,
     "cat",

     f(x)
       )

ar2 = begin
     1,
     a + b,
     "cat",
     f(x)
end


ar2 = begin
     1
     a + b
     "cat"
     f(x)
end


Apply(ClearAll,UserSyms())
