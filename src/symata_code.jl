import Symata: unprotect, protect, @sjdoc, set_attributes

unprotect(:ExpToTrig)

@ex ( ExpToTrig(ex_) := ReplaceRepeated( ex , E^(x_) => Cosh(x) + Sinh(x)) )

protect(:ExpToTrig)

@sjdoc ExpToTrig """
    ExpToTrig(expr)

replace exponentials with trigonometric functions in `expr`.
"""

@ex ( Array(f_, n_Integer) := Table(f(i), [i,n]) )
@ex ( Array(f_, [n_Integer, m_Integer]) := Table(f(i,j), [i,n], [j,m]))

protect(:Array)

# Maybe defined elsewhere
# unprotect(:StringQ)
# @ex StringQ = MatchQ(_AbstractString)
# protect(:StringQ)

# unprotect(:ListQ)
# @ex ListQ = MatchQ(_List)
# protect(:ListQ)
