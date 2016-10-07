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

#### warn_color

if ! isdefined(Base,:warn_color)
    warn_color() = :red
else
    import Base: warn_color
end

#### sigatomic

# We may need to distinguish when to acutally use sigatomic in v0.4 in the future
# But, all occurrences now must be disabled.

if VERSION < v"0.5-"
    sigatomic_begin_v0_5() = nothing
    sigatomic_end_v0_5() = nothing
else
    sigatomic_begin_v0_5() = Base.sigatomic_begin()
    sigatomic_end_v0_5() = Base.sigatomic_end()
end
