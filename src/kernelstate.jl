#### This file should contain all state variables
#### But, early_kernelstate.jl contains things that must be defined first.

# We need to find a place to put VersionInfo

@mkapprule VersionInfo :nargs => 0

@sjdoc VersionInfo "
VersionInfo() returns the version numbers of Symata, Julia, and Python.
"

@doap function VersionInfo()
    println("symata version ", SYMATA_VERSION)
    println("julia version  ", Base.VERSION)
    if isdefined(PyCall, :pyversion)
        println("python version ", pyversion)
    else
        println("no python version available")
    end
    try
        println("sympy version  " * sympy[:__version__])
    catch
        println("sympy version unavailable")
    end
end

# Data structure for monitoring evaluation
type Meval
    entrycount::Int             # For trace
    trace_ev_flag::Bool         # Trace()
    trace_upvalues_flag::Bool   # TraceUpValues()
    trace_downvalues_flag::Bool # TraceDownValues()
    timingon::Bool              # Time() does @time on every user input
    try_downvalue_count::Int
    try_upvalue_count::Int
    is_sjinteractive::Bool
end
const MEVAL = Meval(0,false,false,false,false,0,0,true)

reset_meval_count() = MEVAL.entrycount = 0
get_meval_count() = MEVAL.entrycount
increment_meval_count() = MEVAL.entrycount += 1
decrement_meval_count() = MEVAL.entrycount -= 1

set_meval_trace() = MEVAL.trace_ev_flag = true
unset_meval_trace() = MEVAL.trace_ev_flag = false
is_meval_trace() = MEVAL.trace_ev_flag

set_down_trace() = MEVAL.trace_downvalues_flag = true
unset_down_trace() = MEVAL.trace_downvalues_flag = false
is_down_trace() = MEVAL.trace_downvalues_flag

set_up_trace() = MEVAL.trace_upvalues_flag = true
unset_up_trace() = MEVAL.trace_upvalues_flag = false
is_up_trace() = MEVAL.trace_upvalues_flag

set_timing() = MEVAL.timingon = true
unset_timing() = MEVAL.timingon = false
is_timing() = MEVAL.timingon

reset_try_downvalue_count() = MEVAL.try_downvalue_count = 0
reset_try_upvalue_count() = MEVAL.try_upvalue_count = 0
get_try_downvalue_count() = MEVAL.try_downvalue_count
get_try_upvalue_count() = MEVAL.try_upvalue_count
increment_try_downvalue_count() = MEVAL.try_downvalue_count += 1
increment_try_upvalue_count() = MEVAL.try_upvalue_count += 1

function set_issjinteractive(val::Bool)
    oldval = MEVAL.is_sjinteractive
    MEVAL.is_sjinteractive = val
    return oldval
end

is_sjinteractive() = MEVAL.is_sjinteractive

#### Tracing evaluation

# For (possibly) improved efficiency, we don't include Trace() in the KernelOptions Dict below.
# The value is checked twice on every pass through meval.

@sjdoc Trace "
Trace(True) enables tracing the evaluation loop
Trace(False) disables tracing the evaluation loop
The previous value is returned.
Trace() returns the current value.
"

@mkapprule Trace  :nargs => 0:1

@doap Trace() = is_meval_trace()

@doap function Trace(v::Bool)
    oldval = is_meval_trace()
    v ? set_meval_trace() : unset_meval_trace()
    oldval
end

#### Time  turn timing on and off

@sjdoc Time "
Time(True) enables printing CPU time consumed and memory allocated after each evaluation of command line input.
Time(False) disables printing CPU time consumed and memory allocated.
Time() returns the current value.
"

@mkapprule Time  :nargs => 0:1

@doap Time() = is_timing()

@doap function Time(v::Bool)
    oldval = is_timing()
    v ? set_timing() : unset_timing()
    oldval
end

#### Tracing Up and Down value evaluation

@sjdoc TraceDownValues "
TraceDownValues(True) enables tracing attempted applications of DownRules.
TraceDownValues(False) disables tracing attempted applications of DownRules.
"

@mkapprule TraceDownValues  :nargs => 0:1

@doap TraceDownValues() = is_down_trace()

@doap function TraceDownValues(v::Bool)
    oldval = is_down_trace()
    v ? set_down_trace() : unset_down_trace()
    oldval
end

@sjdoc TraceUpValues "
TraceUpValues(True) enables tracing attempted applications of UpRules.
TraceUpValues(False) disables tracing attempted applications of UpRules.
"

@mkapprule TraceUpValues  :nargs => 0:1

@doap TraceUpValues() = is_up_trace()

@doap function TraceUpValues(v::Bool)
    oldval = is_up_trace()
    v ? set_up_trace() : unset_up_trace()
    oldval
end

##### More evaluation things

type SavedOutput
    expr::Any
end

function clear_all_output()
    for i in 1:length(Output)
        Output[i].expr = :Null   # temporary solutions
    end
end

const LineNumber = Int[0]
get_line_number() = LineNumber[1]
set_line_number(n::Integer) =  (LineNumber[1] = n)
increment_line_number() = LineNumber[1] += 1

const Output = SavedOutput[]

function get_saved_output_by_index(n::Integer)
    if n > 0 && n <= length(Output)
        return Output[n].expr
    end
    nothing
end

function push_output(expr)
    n0 = getkerneloptions(:history_length)
    while length(Output) >= n0
        shift!(Output)
    end
    push!(Output,SavedOutput(expr))
    nothing
end

function get_output_by_line(lineno::Integer)
    idx = length(Output) - (get_line_number() - lineno)
    if idx <= length(Output) && idx > 0
        res = get_saved_output_by_index(idx)
    else
        Null
    end
end

global do_we_print_outstring = true


##### Break

type FlowFlags
    breakflag::Bool
    throwflag::Bool
end

const FLOWFLAGS = FlowFlags(false,false)

# const FLOWFLAGS = Dict{Symbol,Bool}()
# FLOWFLAGS[:Break] = false

function is_break()
    return FLOWFLAGS.breakflag
#    return FLOWFLAGS[:Break]
end

function clear_break()
    FLOWFLAGS.breakflag = false
#    FLOWFLAGS[:Break] = false
end

function set_break()
    FLOWFLAGS.breakflag = true
#    FLOWFLAGS[:Break] = true
end

is_throw()  = FLOWFLAGS.throwflag
clear_throw() = FLOWFLAGS.throwflag = false
set_throw() = FLOWFLAGS.throwflag = true

##### User options and info

const Kerneloptions = Dict{Any,Any}(
                                    :unicode_output => false,
                                    :show_sympy_docs => true,
                                    :return_sympy => false,
                                    :sympy_error => nothing,
                                    :compact_output => true,
                                    :history_length => 100,
                                    :bigint_input => false,
                                    :bigfloat_input => false
                                  )

function getkerneloptions(sym::Symbol)
    Kerneloptions[sym]
end

function setkerneloptions(sym::Symbol, val)
    oval = Kerneloptions[sym]
    Kerneloptions[sym] = val
    oval
end

#### UnicodeOutput

@mkapprule UnicodeOutput :nargs => 0:1

@sjdoc UnicodeOutput "
UnicodeOutput(True)  enables printing unicode characters for some symbols, such as Pi.
UnicodeOutput(False)  (default) disables printing unicode characters.
UnicodeOutput() returns the current state
"

#### ShowSymPyDocs

@mkapprule ShowSymPyDocs  :nargs => 0:1

@sjdoc ShowSymPyDocs "
ShowSymPyDocs(True) (default) enables printing SymPy document strings.
ShowSymPyDocs(False) disables printing these document strings.
ShowSymPyDocs() returns the current state.
"

#### ReturnSymPy

@mkapprule ReturnSymPy  :nargs => 0:1

@sjdoc ReturnSymPy "
ReturnSymPy(True) disables conversion of expressions computed by SymPy to Symata.
ReturnSympy(False) (default) enables conversion to Symata.
ReturnSympy() returns the current state.
"

#### SymPyError

@mkapprule SymPyError :nargs => 0

@sjdoc SymPyError "
SymPyError() returns the most recent sympy error message. If you see a message warning that
a SymPy error has occurred, you can find the detailed error message.
"

#### CompactOutput

@mkapprule CompactOutput  :nargs => 0:1

@sjdoc CompactOutput "
CompactOutput(True) (default) enables printing fewer spaces between operators.
Compact() returns the current state.
"

#### BigIntInput

@mkapprule BigIntInput  :nargs => 0:1

@sjdoc BigIntInput "
BigIntInput(True) enables interpreting all integers as arbitrary precision BigInts.
BigIntInput(False) (default) disables interpreting all integers as arbitrary precision BigInts.
BigIntInput() returns the current state.

You can always specify that an integer should be a BigInt by using BI(n).
"

#### BigFloatInput

@mkapprule BigFloatInput  :nargs => 0:1

@sjdoc BigFloatInput "
BigFloatInput(True) enables interpreting all floating point numbers as arbitrary precision BigFloats.
BigFloatInput(False) (default) disables interpreting all floating point numbers as arbitrary precision BigFloats.
BigFloatInput() returns the current state.

You can always specify that a float should be a BigFloat by using BF(n).
"
@sjseealso_group(BI, BigIntInput, BF, BigFloatInput)

for (fn,sym) in ((:ShowSymPyDocs, :show_sympy_docs), (:UnicodeOutput, :unicode_output), (:ReturnSymPy, :return_sympy),
                 (:CompactOutput, :compact_output), (:BigIntInput, :bigint_input),(:BigFloatInput, :bigfloat_input))

    fnf = Symbol("do_",fn)
    fns = string(fn)
    ssym = string(sym)
    @eval begin
        ($fnf)(mx::Mxpr{Symbol($fns)}, v::Bool) = v ? setkerneloptions(Symbol($ssym), true) : setkerneloptions(Symbol($ssym), false)
        ($fnf)(mx::Mxpr{Symbol($fns)}) = getkerneloptions(Symbol($ssym))
    end
end

for (fn,sym) in ((:SymPyError, :sympy_error),)
    fnf = Symbol("do_",fn)
    fns = string(fn)
    ssym = string(sym)
    @eval begin
        ($fnf)(mx::Mxpr{Symbol($fns)}) = getkerneloptions(Symbol($ssym))
    end
end

@mkapprule HistoryLength  :nargs => 0:1

@sjdoc HistoryLength "
HistoryLength(n) enables storing the n most recent output expressions.
HistoryLength() returns the current value.
"

@doap function HistoryLength(n::Integer)
    oldn = getkerneloptions(:history_length)
    setkerneloptions(:history_length, n)
    for i in 1:(length(Output)-n)
        shift!(Output)
    end
    oldn
end

@doap HistoryLength() = getkerneloptions(:history_length)
