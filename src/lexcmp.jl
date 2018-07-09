## Try to reproduce lexless and lexcmp from version v0.6.3.
## They were removed in v0.7.
## Note that these are in the Symata name space, not Base
## In principle we can name these lexcmp and lexless.
## But, we get deprecation warnings, as if these were the Base function.
## So, we rename them by prefixing with "sym"

"""
    lexcmp(x, y)

Compare `x` and `y` lexicographically and return -1, 0, or 1 depending on whether `x` is
less than, equal to, or greater than `y`, respectively. This function should be defined for
lexicographically comparable types, and `lexless` will call `lexcmp` by default.

```jldoctest
julia> lexcmp("abc", "abd")
-1

julia> lexcmp("abc", "abc")
0
```
"""
symlexcmp(x, y) = cmp(x, y)

"""
    lexless(x, y)

Determine whether `x` is lexicographically less than `y`.

```jldoctest
julia> lexless("abc", "abd")
true
```
"""
symlexless(x, y) = symlexcmp(x,y) < 0


# This has been removed from v0.7
# https://github.com/JuliaLang/julia/issues/5290#issuecomment-324703996
# The argument for removing lexcmp for Complex given in the comment is:
#   Remove lexless and lexcmp for complex numbers. Sorting is the main
#   issue here, which can be easily expressed with sort!(complexes, by=z->(real(z), real(im))).
# But, we are sorting arrays containing more or less anything, not just complex numbers,
function symlexcmp(a::Complex, b::Complex)
    c = cmp(real(a), real(b))
    c == 0 || return c
    cmp(imag(a), imag(b))
end
