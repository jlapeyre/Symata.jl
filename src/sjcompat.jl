# Some comaptibility things.

# as of at least
# v"0.5.0-dev+3868"
# 'symbol' is deprecated in favor of 'Symbol'
# The compatibility code here works at least as far back as
# v"0.4.6-pre+7"
const have_new_Symbol =
    try
        Symbol("a","b")
        true
    catch
        false
    end

if ! have_new_Symbol
    Symbol(args...) = symbol(args...)
end

#### readstring

if ! isdefined(:readstring)
    function readstring(fname::AbstractString)
       stream = open(fname, "r")
       str = readall(stream)
       close(stream)
       str
   end
end

#### view

if ! isdefined(:view)
    view(args...) = slice(args...)
end

#### bytestring_beforecursor

if isdefined(Base.REPL, :bytestring_beforecursor)
    symata_beforecursor(args...) = Base.REPL.bytestring_beforecursor(args...)
else
    symata_beforecursor(args...) = Base.REPL.beforecursor(args...)
end

### denominator

if ! isdefined(:denominator)
    denominator(args...) = den(args...)
    symdenominator(args...) = den(args...)
else
    symdenominator(args...) = denominator(args...)
end

if ! isdefined(:numerator)
    numerator(args...) = num(args...)
    symnumerator(args...) = num(args...)
else
    symnumerator(args...) = numerator(args...)
end

### takebuf_string

sjtakebuf_string(b) = String(take!(b))
