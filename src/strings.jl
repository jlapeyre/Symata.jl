@sjdoc StringInterpolation "
 \" string1  \$a string2 \" interpolates the value of 'a' into the string.
"

set_pattributes(["StringInterpolation"],[:Protected])
function apprules(mx::Mxpr{:StringInterpolation})
    string(margs(mx)...)
end
