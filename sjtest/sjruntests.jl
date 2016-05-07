# Files with testcode contain ordinary SJulia code, except that line
# that start a new expression and begin with "T " are tested The
# expression should return true or false

import SJulia: runtest, @ex, setkerneloptions

# Standard test type
test = SJulia.SJulia_Plain_Test()

@ex testUserSyms = True

# "context_test.sj",
function runalltests()
    for f in (
              "context_test.sj",   #    breaks
              "output_test.sj",
              "math_functions_test.sj",
              "flowcontrol_test.sj",
              "mxpr_test.sj",
              "integral_derivative_test.sj",
              "algebraic_transformations_test.sj",
              "sympy_test.sj",
              "up_downvalues_test.sj",
              "lists_test.sj",
              "trig_test.sj",
              "comparison_test.sj",  # No idea why emacs indents like this.
        "attributes_test.sj",
        "patterns_test.sj",
        "module_test.sj",
        "io_test.sj",
        "orderless_test.sj",
        "somemath_test.sj",
        "expressions_test.sj",
        "arithmetic_test.sj",
        "spec_fun_test.sj",
        "evaluation_test.sj",
        "simple_expression_test.sj"
        )
        runtest(test,f)
    end
end

# Test once with Int as default integer type
setkerneloptions(:bigint_input, false)
runalltests()

# Test once with BigInt as default integer type
setkerneloptions(:bigint_input, true)
runalltests()

SJulia.print_test_results(test)
