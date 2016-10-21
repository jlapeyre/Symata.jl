import Symata: unprotect, protect, @sjdoc

unprotect(:ExpToTrig)

@ex ( ExpToTrig(ex_) := ReplaceRepeated( ex , E^(x_) => Cosh(x) + Sinh(x)) )

protect(:ExpToTrig)

@sjdoc ExpToTrig "
ExpToTrig(expr) replaces exponentials with trigonometric functions in expr.
But, the transformation from Cosh to Cos is not implemented.
"

# Maybe defined elsewhere
# unprotect(:StringQ)
# @ex StringQ = MatchQ(_AbstractString)
# protect(:StringQ)

# unprotect(:ListQ)
# @ex ListQ = MatchQ(_List)
# protect(:ListQ)
