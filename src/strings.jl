@sjdoc StringInterpolation "
 \" string1  \$a string2 \" interpolates the value of 'a' into the string.
"

set_pattributes(["StringInterpolation"],[:Protected])
function apprules(mx::Mxpr{:StringInterpolation})
    symstring(margs(mx)...)
end

@sjdoc StringLength "
StringLength(s) returns the length of the string s.
"
apprules(mx::Mxpr{:StringLength}) = length(mx[1])

@sjdoc ToString "
ToString(expr) returns the string of the printed form or expr.
"
apprules(mx::Mxpr{:ToString}) = symstring(mx[1])

@sjdoc StringJoin "
StringJoin(s1,s2,...) returns the concatendated strings.
"

apprules(mx::Mxpr{:StringJoin}) = join(margs(mx))
