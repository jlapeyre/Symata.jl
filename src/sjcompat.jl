# Some comaptibility things.

# These are deprecated in v0.7 to small routines.
symsearch(s::AbstractString, c::Char) = something(findfirst(isequal(c), s), 0)
symsearchindex(s::AbstractString, t::AbstractString) = first(something(findfirst(t, s), 0:-1))
symsearch(s::AbstractString, c::Char, i::Integer) = something(findnext(isequal(c), s, i), 0)
symreplace(s::AbstractString, pat, f) = replace(s, pat => f)

# as of at least
# v"0.5.0-dev+3868"
# 'symbol' is deprecated in favor of 'Symbol'
# The compatibility code here works at least as far back as
# v"0.4.6-pre+7"
# const have_new_Symbol =
#     try
#         Symbol("a","b")
#         true
#     catch
#         false
#     end

# if ! have_new_Symbol
#     Symbol(args...) = symbol(args...)
# end

#### readstring

# if ! @isdefined(readstring)
#     function readstring(fname::AbstractString)
#        stream = open(fname, "r")
#        str = readall(stream)
#        close(stream)
#        str
#    end
# end

#### view

# if ! @isdefined(view)
#     view(args...) = slice(args...)
# end

#### bytestring_beforecursor

# for v0.7-beta
# if isdefined(Base.REPL, :bytestring_beforecursor)
#     symata_beforecursor(args...) = Base.REPL.bytestring_beforecursor(args...)
# else
import Base.REPL  # try to avoid warning
symata_beforecursor(args...) = REPL.beforecursor(args...)
#end

### denominator

## We can probably use Compat here

# FIXME: replace for v0.7-beta
# if ! @isdefined(denominator)
#     denominator(args...) = den(args...)
#     symdenominator(args...) = den(args...)
# else
symdenominator(args...) = denominator(args...)
#end

# if ! @isdefined(numerator)
#     numerator(args...) = num(args...)
#     symnumerator(args...) = num(args...)
# else
symnumerator(args...) = numerator(args...)
#end

if isdefined(Base, :invokelatest)
    invokelatest(args...) = Base.invokelatest(args...)
else
    invokelatest(args...) = args[1](args[2:end]...)
end

