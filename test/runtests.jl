using Symata
using Test

## FIXME: needed ?
import Test: @test

@Symata.sym TimeOff()   # don't print hundreds of diagnostic lines

# For debugging
# @Symata.ex VersionInfo()

# Run tests for Julia-side interface
include("juliainterface_test.jl")
include("mxpr_test.jl")

# Run Symata language tests
function runtests()
    eval(parse("@sym Tests()"))
end
@test (runtests() ; true)
