#### RecursionLimitError

# SJulia evaluates to a fixed point. This signals too many iterations of infseval
# or meval. The recursion limit is normally around 1000. We throw the exception
# and catch it in exfunc where the evaluation starts.

type RecursionLimitError <: Exception
    msg::AbstractString
    mx::Mxpr
end

abstract SJuliaParseErr <: Exception

type NoTranslationError <: SJuliaParseErr
    head
    expr
end

Base.showerror(io::IO, e::NoTranslationError) = print(io, "extomx translation: no translation defined for Expr head: ", e.head, " in ", e.expr )
Base.showerror(io::IO, e::SJulia.NoTranslationError) = print(io, "extomx translation: no translation defined for Expr head: ", e.head, " in ", e.expr )


#### Arg checking exceptions

# We print a warning and proceed instead of throwing an exception.
# This is what Mma does.
# But, we still instantiate exceptions; we just print the error message.
# So, maybe we, or the user, can switch to using exceptions.

const NumWords = ["zero", "one", "two", "three", "four", "five", "six" ]

# This throws an error caught at the top level
#sjthrow(x) = throw(x)

# I think it is better to warn, as Mma does
function sjthrow(err)
    warn(err.msg)
end


function num_args_string(n::Int)
    if n == 1
        "one argument"
    elseif n < length(NumWords)
        return NumWords[n+1] * " arguments"
    else
        return string(n) * " arguments"
    end
end

function num_args_are_string(n::Int)
    n == 1 ? num_args_string(n) * " is" :  num_args_string(n) * " are"
end

#### Argument check warnings

abstract ArgCheckErr <: Exception

type ExactNumArgsErr <: ArgCheckErr
    msg::AbstractString
end

function ExactNumArgsErr_string(head,ngot, nexpected)
    hstr = string(head)
    if length(hstr) > 7    # Strip the package qualification
        hstr = hstr[8:end]
    end
    msg = hstr * "::argr: " * hstr * " called with " * num_args_string(ngot) * "; " *
     num_args_are_string(nexpected) * " expected."
end

function ExactNumArgsErr(head,ngot, nexpected)
    msg = ExactNumArgsErr_string(head,ngot, nexpected)
    ExactNumArgsErr(msg)
end

function checkargscode(var, head, nargsspec::Int)
    head = string(:($head))
    return :( begin
                if length(margs($var)) != $nargsspec
                  sjthrow(ExactNumArgsErr($head, length(margs($var)), $nargsspec))
               end
              end )
end

type RangeNumArgsErr <: ArgCheckErr
    msg::AbstractString
end

function RangeNumArgsErr_string(head,argrange,ngot)
    hstr = string(head)
    if length(hstr) > 7    # Strip the package qualification
        hstr = hstr[8:end]
    end
    msg = hstr * "::argb: " * hstr * " called with " * num_args_string(ngot) * "; " *
     "between " * string(argrange.start) * " and " * string(argrange.stop) * " are expected."
end

function RangeNumArgsErr(head,argrange,ngot)
    msg = RangeNumArgsErr_string(head,argrange,ngot)
    RangeNumArgsErr(msg)
end

function checkargscode(var, head, nargsspec::UnitRange)
    head = string(:($head))
    return :( begin
                if  length(margs($var))  < $(nargsspec.start) || length(margs($var)) > $(nargsspec.stop)
                  sjthrow(RangeNumArgsErr($head, $nargsspec, length(margs($var))))
               end
             end )
end

macro checknargs(var, head, nargsspec)
    code = checkargscode(var,head,nargsspec)
end
