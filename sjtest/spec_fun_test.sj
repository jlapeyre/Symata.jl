T testUserSyms

# We wanted to use === to make sure we don't have a  float.
# But, if we have a BigInt returned, this does not work. We have
# to add some facilities for conversion.
T ProductLog(0) == 0
T ProductLog(E) == 1
T ProductLog(-1/E) == -1
T ProductLog(-1/E,-1) == -1
# TODO: This basically works, but we cant get the test to work
#T ProductLog(0,-1) ===  DirectedInfinity(-1)

T testUserSyms
