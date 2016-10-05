#### RecursionLimitError

# Symata evaluates to a fixed point. This signals too many iterations of infseval
# or meval. The recursion limit is normally around 1000. We throw the exception
# and catch it in exfunc where the evaluation starts.

type RecursionLimitError <: Exception
    msg::AbstractString
    mx::Mxpr
end

# We don't have these working yet. We just use error for now
abstract SymataParseErr <: Exception

type NoTranslationError <: SymataParseErr
    head
    expr
end

# This is not working atm
Base.showerror(io::IO, e::NoTranslationError) = print(io, "extomx translation: no translation defined for Expr head: ", e.head, " in ", e.expr )

# type SJThrow <: Exception
#     expr
# end

#### Arg checking exceptions

# checkargscode is the interface called from rules for the various Heads

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
    if length(hstr) > 7 && hstr[1:7] == "Symata."   # Strip the package qualification
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

###### RangeNumArgsErr

type RangeNumArgsErr <: ArgCheckErr
    msg::AbstractString
end

function RangeNumArgsErr_string(head,argrange,ngot)
    hstr = string(head)
    if length(hstr) > 7  && hstr[1:7] == "Symata."  # Strip the package qualification
        hstr = hstr[8:end]
    end
    msg = hstr * "::argb: " * hstr * " called with " * num_args_string(ngot) * "; " *
     "between " * string(argrange.start) * " and " * string(argrange.stop) * " are expected."
end

function RangeNumArgsErr(head,argrange,ngot)
    msg = RangeNumArgsErr_string(head,argrange,ngot)
    RangeNumArgsErr(msg)
end

# TODO. if length(nargsspec) == 2, use TwoNumArgsErr
function checkargscode(var, head, nargsspec::UnitRange)
    head = string(:($head))
    return :( begin
                if  length(margs($var))  < $(nargsspec.start) || length(margs($var)) > $(nargsspec.stop)
                  sjthrow(RangeNumArgsErr($head, $nargsspec, length(margs($var))))
               end
             end )
end

###### TwoNumArgsErr

type TwoNumArgsErr <: ArgCheckErr
    msg::AbstractString
end

function TwoNumArgsErr_string(head,argrange::UnitRange,ngot)
    hstr = string(head)
    if length(hstr) > 7 && hstr[1:7] == "Symata."   # Strip the package qualification, Symata
        hstr = hstr[8:end]
    end
    msg = hstr * "::argt: " * hstr * " called with " * num_args_string(ngot) * "; " *
     string(argrange.start) * " or " * string(argrange.stop) * " arguments are expected."
end

function TwoNumArgsErr(head,argrange::UnitRange,ngot)
    msg = TwoNumArgsErr_string(head,argrange,ngot)
    TwoNumArgsErr(msg)
end

###### MoreNumArgsErr

type MoreNumArgsErr <: ArgCheckErr
    msg::AbstractString
end

function MoreNumArgsErr_string(head,argmin,ngot)
    hstr = string(head)
    if length(hstr) > 7 && hstr[1:7] == "Symata."    # Strip the package qualification, Symata
        hstr = hstr[8:end]
    end
    msg = hstr * "::argm: " * hstr * " called with " * num_args_string(ngot) * "; " *
     string(argmin) * " or more arguments are expected."
end

function MoreNumArgsErr(head,argrange,ngot)
    msg = MoreNumArgsErr_string(head,argrange,ngot)
    MoreNumArgsErr(msg)
end



macro checknargs(var, head, nargsspec)
    code = checkargscode(var,head,nargsspec)
end
