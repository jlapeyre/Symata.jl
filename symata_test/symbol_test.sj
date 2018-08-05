## Some other tests should be moved here perhaps.

# Issue #151
  ClearAll(n, g, f)
  n = 1
  f = g
  f(n) = 2
T g(1) == 2
  Clear(f)
T Head(f(n)) == f
  ClearAll(n, g, f)
