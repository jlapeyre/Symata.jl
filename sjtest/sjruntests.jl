# Files with testcode contain ordinary Symata code, except that line
# that start a new expression and begin with "T " are tested The
# expression should return true or false

## FIXME: remove most of all of these imported symbols. Use fully qualified symbols
import Symata
import Symata: @sym, @ex, @exsimple, SymataPlainTest, print_test_results

## Standard test type
## TODO: set this in test.jl so printing test results is an option
symatatest = SymataPlainTest()

# "context_test.sj",
function runalltests()
    for f in (
        "ntable_test.sj",
        "flowcontrol_test.sj",
        "input_test.sj",
        "output_test.sj",
        "numeric_test.sj",
        "sympy_test.sj",
        "pattern_test.sj",
        "integral_derivative_test.sj",
        "evaluation_test.sj",
        "list_test.sj",
        "math_function_test.sj",
        "numcomb_test.sj",
        "ast_translation_test.sj",
        "system_test.sj",
        "stack_exchange_test.sj",
        "part_test.sj",
        "shifrin_test.sj",
        "function_test.sj",
        "up_downvalue_test.sj",
        "linalg_test.sj",
        "string_test.sj",
        "julia_interface_test.sj",
        "latex_test.sj",
        "measurement_test.sj",
        "io_test.sj",
        # "comparison_test.sj"
        "newcomparison_test.sj",
        "predicate_test.sj",
        "trig_test.sj",
        ## FIXME! reimplement the following after change in comparison code
        # "context_test.sj",
        "mxpr_test.sj",
        "algebraic_transformation_test.sj",
        "attribute_test.sj",
        "module_test.sj",
        "orderless_test.sj",
        "arithmetic_test.sj",
        "expression_test.sj",
        "spec_fun_test.sj"
    )
        Symata.runtest(symatatest, f)
        println(stdout, "Done testing $f")
        Symata.symataevaluate(Meta.parse("Apply(ClearAll, UserSyms())"),
                              Symata.EvaluateJuliaSyntaxSimple())
    end
end

# Test once with Int as default integer type
save_biginput_state = Symata.setkerneloptions(:bigint_input, false)

try
    runalltests()
catch e
    warn("Failed running Symata tests")
    rethrow(e)
finally
    Symata.setkerneloptions(:bigint_input, save_biginput_state)
end

# Test once with BigInt as default integer type

println()

setkerneloptions(:bigint_input, true)

try
    runalltests()
catch
    warn("Failed running Symata tests")
finally
    setkerneloptions(:bigint_input, save_biginput_state)
end

@exsimple ClearAll(testUserSyms)
print_test_results(symatatest)
