#### This file should contain all state variables
#### But, early_kernelstate.jl contains things that must be define first.

# Data structure for monitoring evaluation
type Meval
    entrycount::Int             # For trace
    trace_ev_flag::Bool         # TraceOn()
    trace_upvalues_flag::Bool   # TrUpOn()
    trace_downvalues_flag::Bool # TrDownOn()
    timingon::Bool              # TimeOn() does @time on every user input
    try_downvalue_count::Int
    try_upvalue_count::Int
end
const MEVAL = Meval(0,false,false,false,false,0,0)

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

##### More evaluation things

type SavedOutput
    expr::Any
end

const LineNumber = Int[0]
get_line_number() = LineNumber[1]
set_line_number(n::Int) =  (LineNumber[1] = n)
increment_line_number() = LineNumber[1] += 1


const Output = SavedOutput[]

function get_saved_output_by_index(n::Int)
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

function get_output_by_line(lineno::Int)
    idx = length(Output) - (get_line_number() - lineno)
    if idx <= length(Output) && idx > 0
        res = get_saved_output_by_index(idx)
    else
        Null
    end
end
          
global do_we_print_outstring = true


##### Break

const FLOWFLAGS = Dict{Symbol,Bool}()
FLOWFLAGS[:Break] = false

##### User options and info

const Kerneloptions = Dict{Any,Any}(
                                    :unicode_output => false,
                                    :show_sympy_docs => true,
                                    :return_sympy => false,
                                    :sympy_error => nothing,
                                    :compact_output => true,
                                    :history_length => 100
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
ReturnSymPy(True) disables conversion of expressions computed by SymPy to SJulia.
ReturnSympy(False) (default) enables conversion to SJulia.
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

@mkapprule HistoryLength  :nargs => 0:1

@sjdoc HistoryLength "
HistoryLength(n) enables storing the n most recent output expressions.
HistoryLength() returns the current value.
"

for (fn,sym) in ((:ShowSymPyDocs, :show_sympy_docs), (:UnicodeOutput, :unicode_output), (:ReturnSymPy, :return_sympy), (:CompactOutput, :compact_output),
                 (:HistoryLength, :history_length))
    fnf = symbol("do_",fn)
    fns = string(fn)
    ssym = string(sym)
    @eval begin
        ($fnf)(mx::Mxpr{symbol($fns)}, v::Bool) = v ? setkerneloptions(symbol($ssym), true) : setkerneloptions(symbol($ssym), false)
        ($fnf)(mx::Mxpr{symbol($fns)}) = getkerneloptions(symbol($ssym))
    end
end

for (fn,sym) in ((:SymPyError, :sympy_error),)
    fnf = symbol("do_",fn)
    fns = string(fn)
    ssym = string(sym)
    @eval begin
        ($fnf)(mx::Mxpr{symbol($fns)}) = getkerneloptions(symbol($ssym))
    end
end    


@doap function HistoryLength(n::Int)
    oldn = getkerneloptions(:history_length)
    setkerneloptions(:history_length, n)
    for i in 1:(length(Output)-n)
        shift!(Output)
    end
    oldn
end

@doap HistoryLength() = getkerneloptions(:history_length)


