# This tests apprules and canonializer
@ex ClearAll(z)

@testex  Abs(-1) == 1
@testex  Abs(-2.0) == 2.0
@testex  Abs(z^3) == Abs(z)^3
@testex  Abs(z^4.1) == Abs(z)^4.1
@testex  Abs(-z) == Abs(z)
@testex  Abs(-z^(1/2))^2 == Abs(z)
@testex  Sin(-z) == -1*Sin(z)
@testex  Sin(-z^2)^2 == Sin(z^2)^2

@ex ClearAll(z)
