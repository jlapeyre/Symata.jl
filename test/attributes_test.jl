@testex testUserSyms

@ex       ClearAll(a)
@testex   Protect(a) == ["a"]
@testex   Attributes(a) == [Protected]
@testex   Unprotect(a) == ["a"]
@testex   Attributes(a) == []
@ex       ClearAll(a)

@testex testUserSyms
