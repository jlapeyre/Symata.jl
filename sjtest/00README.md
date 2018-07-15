### Symata-language tests

These tests can be run via

    symata> Tests()

or

    julia> Symata.run_testsuite()


`sjruntests.jl` runs tests using Symata itself. The files listed in
`sjruntests.jl` are read as Symata code.

#### Syntax of test files

There is very little syntax added for testing. Lines beginning with "T " and a new expression denote a test.
The test passes only if expression returns `true` This syntax "T ",
is not part of Symata, but is particular to the test code.
