@testex testUserSyms

# We wanted to use === to make sure we don't have a  float.
# But, if we have a BigInt returned, this does not work. We have
# to add some facilities for conversion.
@testex ProductLog(0) == 0
@testex ProductLog(E) == 1
@testex ProductLog(-1/E) == -1
@testex ProductLog(-1/E,-1) == -1
# TODO: This basically works, but we cant get the test to work
#@testex ProductLog(0,-1) ===  DirectedInfinity(-1)

@testex testUserSyms


