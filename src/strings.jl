@sjdoc StringInterpolation """
 "string1  \$a string2" interpolates the value of `a` into the string.
"""

set_pattributes(["StringInterpolation"],[:Protected])
function apprules(mx::Mxpr{:StringInterpolation})
    symstring(margs(mx)...)
end

### StringLength

@sjdoc StringLength """
    StringLength(s)

returns the length of the string `s`.
"""
apprules(mx::Mxpr{:StringLength}) = length(mx[1])

### ToString

@sjdoc ToString """
    ToString(expr)

return a string of the printed form or `expr`.
"""
apprules(mx::Mxpr{:ToString}) = symstring(mx[1])

### StringJoin

@sjdoc StringJoin """
    StringJoin(s1,s2,...)
    StringJoin([s1,s2,...])

return the concatenated strings.
"""
@mkapprule StringJoin
@doap StringJoin(s::ListT) = join(margs(s))
@doap StringJoin(args::String...) = join(args)

### Characters

@sjdoc Characters """
    Characters(s)

return a `List` of the characters in `String` `s`.
"""
@mkapprule Characters
@doap Characters(s::String) = mxpr(:List, [string(x) for x in s]...)

### StringReverse

@sjdoc StringReverse """
    StringReverse(s)

return string `s` with characters in reverse order.
"""

@mkapprule StringReverse

@doap StringReverse(s::String) = reverse(s)
