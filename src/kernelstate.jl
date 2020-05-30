#### This file should contain all state variables
#### But, early_kernelstate.jl contains things that must be defined first.

# We need to find a place to put VersionInfo
@mkapprule VersionInfo :nargs => 0:1

@sjdoc VersionInfo """
    VersionInfo()

Return the version numbers of Symata, Julia, and Python.

    VersionInfo(All)

give more verbose information.
"""

@doap function VersionInfo()
    return _versioninfo()
end

_vpad(s) = rpad(s,19)

function _versioninfo()
    println(_vpad("Symata version"), SYMATA_VERSION)
#    println(_vpad("Commit"), string(commit_hash, "*"))
    println(_vpad("Julia version"), Base.VERSION)
#    if isdefined(PyCall, :pyversion)
    if hasproperty(PyCall, :pyversion)
        println(_vpad("Python version"), pyversion)
    else
        println("no Python version available")
    end
    try
        println(_vpad("SymPy version") * sympy.__version__)
    catch
        println("SymPy version unavailable")
    end
    if @isdefined(SymataSyntax) && _init_symatasyntax()
        println(_vpad("symatasyntax ver."), SymataSyntax.SYMATASYNTAX_VERSION)
        try
            println(_vpad("mathics version") * SymataSyntax.mathics.__version__)
        catch
            println("mathics version unavailable")
        end
    end
    return nothing
end

@doap function VersionInfo(s::SJSym)
    if s == :All
        _versioninfo()
        return InteractiveUtils.versioninfo()
    else
        return mx
    end
end

"""
    type Meval

Holds flags and counters for monitoring evaluation.
"""
mutable struct Meval
    entrycount::Int             # For trace
    trace_ev_flag::Bool         # Trace()
    trace_upvalues_flag::Bool   # TraceUpValues()
    trace_downvalues_flag::Bool # TraceDownValues()
    timingon::Bool              # Time() does @time on every user input
    try_downvalue_count::Int
    try_upvalue_count::Int
    localization_counter::Int
#    is_sjinteractive::Bool
end
const MEVAL = Meval(0, false, false, false, false, 0, 0, 0)

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

get_localization_counter() = MEVAL.localization_counter
increment_localization_counter() = MEVAL.localization_counter += 1

#### Tracing evaluation

# For (possibly) improved efficiency, we don't include Trace() in the KernelOptions Dict below.
# The value is checked twice on every pass through meval.

@sjdoc Trace """
    Trace(True)

enable tracing the evaluation loop.

    Trace(False)

disable tracing the evaluation loop.

The previous value is returned. `Trace()` returns the current value.
"""

@mkapprule Trace :nargs => 0:1

@doap Trace() = is_meval_trace()

@doap function Trace(v::Bool)
    oldval = is_meval_trace()
    v ? set_meval_trace() : unset_meval_trace()
    return oldval
end

#### Time  turn timing on and off

@sjdoc Time """
    Time(True)

enable printing CPU time consumed and memory allocated after each evaluation of command line input.

    Time(False)

disable printing CPU time consumed and memory allocated.

    Time()

Return the current value.
"""

@mkapprule Time  :nargs => 0:1

@doap Time() = is_timing()

@doap function Time(v::Bool)
    oldval = is_timing()
    v ? set_timing() : unset_timing()
    return oldval
end

#### Tracing Up and Down value evaluation

@sjdoc TraceDownValues """
    TraceDownValues(True)

enable tracing attempted applications of `DownRules`.

    TraceDownValues(False)

disable tracing attempted applications of `DownRules`.
"""

@mkapprule TraceDownValues  :nargs => 0:1

@doap TraceDownValues() = is_down_trace()

@doap function TraceDownValues(v::Bool)
    oldval = is_down_trace()
    v ? set_down_trace() : unset_down_trace()
    return oldval
end

@sjdoc TraceUpValues """
    TraceUpValues(True)

enable tracing attempted applications of `UpRules`.

    TraceUpValues(False)

disable tracing attempted applications of `UpRules`.
"""

@mkapprule TraceUpValues  :nargs => 0:1

@doap TraceUpValues() = is_up_trace()

@doap function TraceUpValues(v::Bool)
    oldval = is_up_trace()
    v ? set_up_trace() : unset_up_trace()
    return oldval
end

##### More evaluation things

mutable struct SavedOutput
    expr::Any
end

function clear_all_output()
    for i in 1:length(Output)
        Output[i].expr = :Null   # temporary solutions
    end
    return nothing
end

const LineNumber = Int[1]
get_line_number() = LineNumber[1]
set_line_number(n::Integer) =  (LineNumber[1] = n)
increment_line_number() = LineNumber[1] += 1

const Output = SavedOutput[]

function get_saved_output_by_index(n::Integer)
    if n > 0 && n <= length(Output)
        return Output[n].expr
    end
    return nothing
end

function push_output(expr)
    n0 = getkerneloptions(:history_length)
    while length(Output) >= n0
        popfirst!(Output)
    end
    push!(Output, SavedOutput(expr))
    return nothing
end

get_output_by_line() = get_output_by_line(-1)

function get_output_by_line(lineno::Integer)
    if lineno < 0 lineno = length(Output) + lineno + 1 end
    idx = length(Output) - (get_line_number() - lineno) + 1
    if idx <= length(Output) && idx > 0
        return get_saved_output_by_index(idx)
    else
        return Null
    end
end

global do_we_print_Out_label = true


##### Break

mutable struct FlowFlags
    breakflag::Bool
    throwflag::Bool
end

const FLOWFLAGS = FlowFlags(false, false)

# const FLOWFLAGS = Dict{Symbol, Bool}()
# FLOWFLAGS[:Break] = false

function is_break()
    return FLOWFLAGS.breakflag
#    return FLOWFLAGS[:Break]
end

function clear_break()
    return FLOWFLAGS.breakflag = false
#    FLOWFLAGS[:Break] = false
end

function set_break()
    return FLOWFLAGS.breakflag = true
#    FLOWFLAGS[:Break] = true
end

is_throw()  = FLOWFLAGS.throwflag
clear_throw() = FLOWFLAGS.throwflag = false
set_throw() = FLOWFLAGS.throwflag = true

##### User options and info

"""
    Kerneloptions

Global options for the Symata kernel.
"""
const Kerneloptions = Dict{Any,Any}(
    :unicode_output => false,
    :ijulia_latex_output => false,
    :show_sympy_docs => true,
    :return_sympy => false,
    :sympy_error => nothing,
    :compact_output => true,
    :history_length => 100,
    :bigint_input => false,
    :bigfloat_input => false,
    :isymata_inited => false,
    #:isymata_mode => false, # this state now stored in ./IJulia/handlers.jl
    :isymata_mma_mode => false,
    :output_style => :InputForm,
    :float_format => "",
    :bigfloat_format => ""
)

function getkerneloptions(sym::Symbol)
    return Kerneloptions[sym]
end

"""
    setkerneloptions(sym::Symbol, val)

Set the Symata kernel option `sym` to `val`.
"""
function setkerneloptions(sym::Symbol, val)
    oval = Kerneloptions[sym]
    Kerneloptions[sym] = val
    return oval
end

#### UnicodeOutput

@mkapprule UnicodeOutput :nargs => 0:1  :nodefault => true

@sjdoc UnicodeOutput """
    UnicodeOutput()

is obsolete. See `OutputStyle()`.
"""

@doap UnicodeOutput(args...) = println("UnicodeOutput() is obsolete. See OutputStyle()")

#### ShowSymPyDocs

@mkapprule ShowSymPyDocs  :nargs => 0:1

@sjdoc ShowSymPyDocs """
    ShowSymPyDocs(True)

(default) enable printing SymPy document strings.

    ShowSymPyDocs(False)

disable printing SymPy document strings.

    ShowSymPyDocs()

Return the current state.
"""

#### ReturnSymPy

@mkapprule ReturnSymPy  :nargs => 0:1

@sjdoc ReturnSymPy """
    ReturnSymPy(True)

disable conversion of expressions computed by SymPy to Symata.

    ReturnSympy(False)

(default) enable conversion to Symata.

    ReturnSympy()

Return the current state.
"""

#### SymPyError

@mkapprule SymPyError :nargs => 0

@sjdoc SymPyError """
    SymPyError()

Return the most recent SymPy error message. If you see a message warning that
a SymPy error has occurred, you can find the detailed error message.
"""

#### CompactOutput

@mkapprule CompactOutput  :nargs => 0:1

@sjdoc CompactOutput """
    CompactOutput(True)

(default) enable printing fewer spaces between operators.

    Compact()

Return the current state.

`CompactOutput` has an effect in `InputForm` and `UnicodeForm` output styles, but not in `JupyterForm` output style.
"""

#### BigIntInput

@mkapprule BigIntInput  :nargs => 0:1

@sjdoc BigIntInput """
    BigIntInput(True)

enable interpreting all integers as arbitrary precision `BigInt`s.

    BigIntInput(False)

(default) disable interpreting all integers as arbitrary precision `BigInts`.

    BigIntInput()

Return the current state.

You can always specify that an integer should be a `BigInt` by giving `BI(n)`.
"""


"""
    bigintinput()

Return true if the "kernel option" for converting input integers to BigInt is set.
"""
bigintinput() = getkerneloptions(:bigint_input)

#### BigFloatInput

@mkapprule BigFloatInput  :nargs => 0:1

@sjdoc BigFloatInput """
    BigFloatInput(True)

enable interpreting all floating point numbers as arbitrary precision `BigFloats`.

    BigFloatInput(False)

(default) disable interpreting all floating point numbers as arbitrary precision `BigFloat`s.

    BigFloatInput()

Return the current state.

You can always specify that a float should be a BigFloat by using BF(n).
"""

@sjseealso_group(BI, BigIntInput, BF, BigFloatInput)

for (fn, sym) in ((:ShowSymPyDocs, :show_sympy_docs), (:ReturnSymPy, :return_sympy),
                 (:CompactOutput, :compact_output), (:BigIntInput, :bigint_input),(:BigFloatInput, :bigfloat_input))
    fnf = Symbol("do_", fn)
    fns = string(fn)
    ssym = string(sym)
    @eval begin
        ($fnf)(mx::Mxpr{Symbol($fns)}, v::Bool) = v ? setkerneloptions(Symbol($ssym), true) : setkerneloptions(Symbol($ssym), false)
        ($fnf)(mx::Mxpr{Symbol($fns)}) = getkerneloptions(Symbol($ssym))
    end
end

for (fn, sym) in ((:SymPyError, :sympy_error),)
    fnf = Symbol("do_", fn)
    fns = string(fn)
    ssym = string(sym)
    @eval begin
        ($fnf)(mx::Mxpr{Symbol($fns)}) = getkerneloptions(Symbol($ssym))
    end
end

@mkapprule HistoryLength  :nargs => 0:1

@sjdoc HistoryLength """
    HistoryLength(n)

enable storing the `n` most recent output expressions.

    HistoryLength()

Return the current value.
"""

function _set_historylength(n::Integer)
    oldn = getkerneloptions(:history_length)
    setkerneloptions(:history_length, n)
    for i in 1:(length(Output)-n)
        popfirst!(Output)
    end
    oldn
end

@doap function HistoryLength(n::Integer)
    _set_historylength(n)
end

@doap HistoryLength() = getkerneloptions(:history_length)

@sjdoc FloatFormat """
    FloatFormat()

get the floating point output format

    FloatFormat(fmt)

set the floating point output format to `fmt`.

If `fmt` is `Full` or `""`, then default output format is used.

If `fmt` is `Short` or `"%.6g"`, then fewer digits are printed.

If `fmt` is `n` or `"%.ng"`, then `n` digits after the decimal are printed.
"""
@mkapprule FloatFormat
@doap FloatFormat() = getkerneloptions(:float_format)
@doap FloatFormat(s) = setkerneloptions(:float_format,_float_format(s))

_float_format(s::String) = s

function _float_format(s::Symbol)
    if s == :Full
        return ""
    elseif s == :Short
        return "%g"
    else
        symerror("Unrecognized number format")
    end
end

function _float_format(s::Integer)
    "%.$(s)g"
end

@sjdoc BigFloatFormat """
    BigFloatFormat()

get the output format for arbitrary-precision, floating-point numbers.

    BigFloatFormat(fmt::String)

set the output format for arbitrary-precision, floating-point numbers to `fmt`.

If `fmt=""`, then default output format is used

`fmt="%.6g"` is a common choice.
"""
@mkapprule BigFloatFormat
@doap BigFloatFormat() = getkerneloptions(:bigfloat_format)
@doap BigFloatFormat(s) = setkerneloptions(:bigfloat_format,_float_format(s))

#### isymata_inited

"""
    isymata_inited()
    isymata_inited(v::Bool)

true if IJulia has been configured to run Symata during this
session. This configuration is done by `init_isymata` the first time `isymata()`
is called during a session.
"""
isymata_inited() = getkerneloptions(:isymata_inited)
isymata_inited(v::Bool) = (ov = getkerneloptions(:isymata_inited); setkerneloptions(:isymata_inited,v); ov)

# """
#     isymata_mode()
#     isymata_mode(v::Bool)

# get or set the flag signifying that IJulia input should be interpreted as Symata rather than Julia.
# """
# isymata_mode() = getkerneloptions(:isymata_mode)
# isymata_mode(v::Bool) = (ov = getkerneloptions(:isymata_mode); setkerneloptions(:isymata_mode,v); ov)

"""
    isymata_mma_mode()
    isymata_mma_mode(v::Bool)

get or set the flag signifying that IJulia input should be interpreted as Symata with Mathematica syntax rather than Julia or Symata.
"""
isymata_mma_mode() = getkerneloptions(:isymata_mma_mode)
isymata_mma_mode(v::Bool) = (ov = getkerneloptions(:isymata_mma_mode); setkerneloptions(:isymata_mma_mode,v); ov)

@mkapprule OutputStyle :nargs => 0:1
@sjdoc OutputStyle """
    OutputStyle(InputForm)

print plain 1d text output.

    OutputStyle(UnicodeForm)

print 1d text output with pretty unicode characters.

    OutputStyle(JupyterForm)

in a Jupyter notebook, print in typeset mathematics style using latex.

    OutputStyle()

Return the current output style.

`InputForm` and `UnicodeForm` give output that is valid `Symata` input for the same expression.
"""

function _set_output_style(st::Symbol)
    if st != :InputForm && st != :UnicodeForm  && st != :JupyterForm
        error("OutputStyle: style must be one of InputForm, UnicodeForm, JupyterForm")
    end
    oldst = getkerneloptions(:output_style)
    setkerneloptions(:output_style, st)
    return st
end

@doap function OutputStyle(st::Symbol)
    _set_output_style(st)
end

@doap OutputStyle() = getkerneloptions(:output_style)

@sjseealso_group(OutputStyle, CompactOutput)

"""
    using_unicode_output()

Return true if unicode output style is selected. This is mutually exclusive with InputForm output and
JupyterForm output styles.
"""
using_unicode_output() = getkerneloptions(:output_style) == :UnicodeForm

# This means we are using LaTeX
using_ijulia_output() = getkerneloptions(:output_style) == :JupyterForm

### Julia

@sjdoc Julia """
    Julia()

Exit Symata mode and returns to Julia mode from within Jupyter.
Use `isymata()` from Julia to enter Symata mode again.
"""

@mkapprule Julia :nargs => 0

@doap function Julia()
    Jupyter.set_julia_mode()
    nothing
#    set_jupyter_input_prompt_color("green") # This used to apply to single cells.
end
