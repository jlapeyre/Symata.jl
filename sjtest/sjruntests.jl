# Files with testcode contain ordinary Symata code, except that line
# that start a new expression and begin with "T " are tested The
# expression should return true or false

import Symata: runtest, @ex, setkerneloptions, Symata_Plain_Test, print_test_results

# Standard test type
test = Symata_Plain_Test()

@ex testUserSyms = True

# "context_test.sj",
function runalltests()
    for f in (
              "latex_test.sj",
              "output_test.sj",
              "measurements_test.sj",
              "io_test.sj",
              "comparison_test.sj",  # No idea why emacs indents like this.
              "matrix_test.sj",
              "math_functions_test.sj",
              "flowcontrol_test.sj",
              "evaluation_test.sj",
              "lists_test.sj",
              "patterns_test.sj",
              "up_downvalues_test.sj",
              "predicate_test.sj",
              "trig_test.sj",
              "context_test.sj",   #    breaks
              "mxpr_test.sj",
              "integral_derivative_test.sj",
              "algebraic_transformations_test.sj",
              "sympy_test.sj",
        "attributes_test.sj",
        "module_test.sj",
        "orderless_test.sj",
        "somemath_test.sj",
        "expressions_test.sj",
        "arithmetic_test.sj",
        "spec_fun_test.sj",
        "simple_expression_test.sj"
        )
        runtest(test,f)
        println(STDERR, "Done testing $f")
    end
end

# Test once with Int as default integer type
save_biginput_state = setkerneloptions(:bigint_input, false)

try
    runalltests()
catch e
    warn("Failed running Symata tests")
    rethrow(e)
finally
    setkerneloptions(:bigint_input, save_biginput_state)
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

print_test_results(test)
