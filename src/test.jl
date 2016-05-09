# Test SJulia code.
#
# We don't use the ordinary Julia Test module because we preprocess
# strings of SJulia code before they are parsed by the Julia parser. A
# macro like @test cannot be used because the parser will only accept
# syntactically valid Julia expressions, and, before the
# preprocessing, the SJulia expressions are not. So, this would
# necessitate wrapping all the SJulia code in strings. (hmmm. I guess
# a string macro might work well for that) Instead, we read and evaluate
# the test code using the same method we use for SJulia user code.
#
# An SJulia test code file is contains ordinary SJulia expressions,
# with the exception that some lines begin with "T " as the first two
# characters.  The result of evaluating these lines is tested. The
# result is expected to be true or false. The results are tallied. We
# use a single read-eval function and method in IO.jl for testing and
# ordinary user code. For ordinary code, we pass the SJulia_NullTest
# type, which signifies that we are not testing. I expect this will
# evolve, but for the moment I don't want to duplicate the relatively
# untested read-eval code.
# 
# At the moment We have one type of test. It just records the number
# of passes and fails and prints the result.

abstract SJulia_Test

type SJulia_NullTest <: SJulia_Test
end

type SJulia_Plain_Test <: SJulia_Test
    total::Int
    pass::Int
end

SJulia_Plain_Test() = SJulia_Plain_Test(0,0)

function print_test_results(test::SJulia_Plain_Test)
    println(test.pass, " passed.")
    println(test.total - test.pass, " failed.")
end

function record_SJTest(test::SJulia_Plain_Test, fname, linenumber, res)
    test.total += 1
    if res == true
        test.pass += 1
    else
        warn("Test failed in $fname, line $linenumber")
    end
end

function SJulia_test_path()
    joinpath(SJulia_module_path(), "sjtest")
end

function runtest(test,fname)
    path = joinpath(SJulia_test_path(), fname)
    read_SJulia_file(path, test)
    nothing
end

function run_testsuite()
    startfile = joinpath(SJulia_test_path(), "sjruntests.jl")
    include(startfile)
end

## FIXME. Sometimes errors silently cut the testing short, reporting only successes
@mkapprule Tests :nargs => 0

@sjdoc Tests "
Tests() runs the SJulia test suite. This runs the code in 'sjtest', this should
be newer and better maintained than the code in the 'test' directory.
"

@doap Tests() = run_testsuite()
