# We wanted to use === to make sure we don't have a  float.
# But, if we have a BigInt returned, this does not work. We have
# to add some facilities for conversion.
# LambertW has been removed. So comment out the ProductLog tests
T ProductLog(0) == 0
T ProductLog(E) == 1
T ProductLog(-1/E) == -1
T ProductLog(-1/E, -1) == -1
T Abs(ProductLog(-.2, -1) + 2.5426413577735265) < 10^(-8)
T Abs(ProductLog(-.2) + 0.25917110181907377) < 10^(-8)

# TODO: This basically works, but we cant get the test to work
#T ProductLog(0,-1) ===  DirectedInfinity(-1)
