@sjdoc StringInterpolation """
 "string1  \$a string2" interpolates the value of `a` into the string.
"""

set_sysattributes(:StringInterpolation,:Protected)
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

### ToUpperCase

@mkapprule ToUpperCase :nargs => 1
@doap ToUpperCase(s::String) = uppercase(s)

### ToLowerCase

@mkapprule ToLowerCase :nargs => 1
@doap ToLowerCase(s::String) = lowercase(s)

### UpperCaseQ

@mkapprule UpperCaseQ :nargs => 1
@doap UpperCaseQ(s::String) = isupper(s)


### LowerCaseQ

@mkapprule LowerCaseQ :nargs => 1
@doap LowerCaseQ(s::String) = islower(s)

### ToCharacterCode

@mkapprule ToCharacterCode :nargs => 1
@doap ToCharacterCode(s::Char) = Int(s)
@doap ToCharacterCode(s::String) = MListA(MxprArgT[Int(c) for c in s])


### FromCharacterCode

@mkapprule FromCharacterCode
@doap FromCharacterCode(i::Int) = Char(i)

## This is where we need pattern matching for instance.
@doap function FromCharacterCode(s::ListT)
    listofpredq(s,integerq) ? _fromcharactercode_list(s) :
       listoflistsofpredq(s,integerq) ? MListA(MxprArgT[ _fromcharactercode_list(x) for x in s]) : mx
end

_fromcharactercode_list(s) = String([Char(i) for i in s])
    
### LetterNumber

@mkapprule LetterNumber :nargs => 1

@doap LetterNumber(x::String) = length(x) > 1 ? mx : letternumber(x[1])

function letternumber(c::Char)
    Int(uppercase(c)) - 64
end

### DigitQ

@mkapprule DigitQ :nargs =>1
@doap DigitQ(s::Union{Char,AbstractString}) = isdigit(s)


### StringSplit

@mkapprule StringSplit
@doap StringSplit(s::String) = MList(split(s)...)
@doap StringSplit(s::String, d::Union{String,Regex}) = MList(split(s,d)...)

@mkapprule MathematicaFullFormString  :nargs => 1

@doap MathematicaFullFormString(x) = SymataIO.symata_to_mma_fullform_string(x)
