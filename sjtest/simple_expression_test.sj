ClearTemporary()

ClearAll(a,b)
## FIXME: Following two succeed in v0.5. Fail in later versions of Julia
# T  b^0 == 1
# T  b^1 == b

T  b*1 == b
T  1*b == b
T  -(a+b) == -a - b  # lhs is expanded
T  -1*(a+b) == -a - b

ClearAll(a,b)
