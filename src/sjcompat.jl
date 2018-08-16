# Compatibility functions that are particular to Symata.

# The following are deprecated in v0.7 to small routines.
symrsearch(s::AbstractString, c::Union{Tuple{Vararg{Char}}, AbstractVector{Char}, Set{Char}},
           pos) = something(findprev((in)(c), s, pos), 0)
symrsearch(s::AbstractString, c::Char,
           pos) = something(findprev((in)(c), s, pos), 0)
#                                              something(findprev((in)(c), s, pos), 0)
symsearch(s::AbstractString, c::Char) = something(findfirst(isequal(c), s), 0)
symsearchindex(s::AbstractString, t::AbstractString) = first(something(findfirst(t, s), 0:-1))
symsearch(s::AbstractString, c::Char, i::Integer) = something(findnext(isequal(c), s, i), 0)
symreplace(s::AbstractString, pat, f) = replace(s, pat => f)
## Warning! The deprecation says this is a drop-in replacement.
## But, it fails in some cases. Eg. in collect_times
symfindin(a, b) = findall((in)(b), a)

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
import REPL  # try to avoid warning
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

## The suggested replacement for findin fails
## in coefficient_times, algebra.jl.
## So, we reproduce the version from v0.6.3
## We continue to get deprecation warnings even thought this is
## Not exported to Base or Main. So, we rename it to oldfindin.
"""
    Symata.oldfindin(a, b)

Returns the indices of elements in collection `a` that appear in collection `b`.

# Examples
```jldoctest
julia> a = collect(1:3:15)
5-element Array{Int64,1}:
  1
  4
  7
 10
 13

julia> b = collect(2:4:10)
3-element Array{Int64,1}:
  2
  6
 10

julia> oldfindin(a,b) # 10 is the only common element
1-element Array{Int64,1}:
 4
```
"""
function oldfindin(a, b)
    ind = Array{Int,1}(undef, 0)
    bset = Set(b)
    @inbounds for (i,ai) in enumerate(a)
        ai in bset && push!(ind, i)
    end
    ind
end

## for v0.6, we used Base.syntax_deprecation_warnings
## 1) This function is not present in v0.7
## 2) The deprecated syntax will probably be removed soon.
## So, we make this a noop for the time being
symata_syntax_deprecation_warnings(f, args...) = f()
