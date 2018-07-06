# Test Symata code.
#
# We don't use the ordinary Julia Test module because we preprocess
# strings of Symata code before they are parsed by the Julia parser. A
# macro like @test cannot be used because the parser will only accept
# syntactically valid Julia expressions, and, before the
# preprocessing, the Symata expressions are not. So, this would
# necessitate wrapping all the Symata code in strings. (hmmm. I guess
# a string macro might work well for that) Instead, we read and evaluate
# the test code using the same method we use for Symata user code.
#
# An Symata test code file is contains ordinary Symata expressions,
# with the exception that some lines begin with "T " as the first two
# characters.  The result of evaluating these lines is tested. The
# result is expected to be true or false. The results are tallied. We
# use a single read-eval function and method in IO.jl for testing and
# ordinary user code. For ordinary code, we pass the Symata_NullTest
# type, which signifies that we are not testing. I expect this will
# evolve, but for the moment I don't want to duplicate the relatively
# untested read-eval code.
#
# At the moment We have one type of test. It just records the number
# of passes and fails and prints the result.

abstract type Symata_Test end

"""
    Symata_NullTest <: Symata_Test

A test passed to `read_Symata_file` that causes the contents
of the file to be read as ordinary Symata code, with no test syntax.
"""
mutable struct Symata_NullTest <: Symata_Test
end

abstract type AbstractSymataPlainTest <: Symata_Test end

mutable struct SymataPlainTest <: AbstractSymataPlainTest
    total::Int
    pass::Int
end

mutable struct SymataPlainPrintTest <: AbstractSymataPlainTest
    total::Int
    pass::Int
end

SymataPlainTest() = SymataPlainTest(0,0)
SymataPlainPrintTest() = SymataPlainPrintTest(0,0)

function print_test_results(test::AbstractSymataPlainTest)
    print_with_color(:green, string(test.pass), " passed.\n")
    failed = test.total - test.pass
    if failed == 0
        println("$failed failed.")
    else
        print_with_color(:red, "$failed failed.")
    end
end

function record_SJTest(test::AbstractSymataPlainTest, fname, linenumber, res)
    test.total += 1
    if res == true
        test.pass += 1
    else
        warn("Test failed in $fname, line $linenumber")
    end
end

function Symata_test_path()
    joinpath(Symata_module_path(), "sjtest")
end

function runtest(test,fname)
    path = joinpath(Symata_test_path(), fname)
    read_Symata_file(path, test)
    nothing
end

function run_testsuite()
    startfile = joinpath(Symata_test_path(), "sjruntests.jl")
    include(startfile)
end

### Tests

## FIXME. Sometimes errors silently cut the testing short, reporting only successes
## FIXME  can't give both :nargs => 0:1 and :options
@mkapprule Tests :options => Dict( :PrintTests => false )

@sjdoc Tests """
    Tests()

run the Symata test suite. This runs the code in the directory `sjtest`, this should
be newer and better maintained than the code in the `test` directory.

    Tests(filename)

runs the tests in `filename` in the directory `sjtest`.
"""

# kws not yet implemented here
#@doap Tests(;kws...) = run_testsuite(kws...)

do_Tests(mx::Mxpr{:Tests};kws...) = run_testsuite()

## TODO: need a way to assert type Bool
function do_Tests(mx::Mxpr{:Tests}, s::String; PrintTests=false)
    test = PrintTests ? SymataPlainPrintTest() : SymataPlainTest()    
    runtest(test, s)
end


### Test

@mkapprule Test :nargs => 1

@doap function Test(ex)
    trueq(doeval(ex))
end

#set_pattributes(:Test, :HoldAll)
