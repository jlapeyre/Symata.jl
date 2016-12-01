T StringLength("cat") == 3
T ToString(a + b) == "a + b"
T StringJoin("cat","dog") == "catdog"
T StringJoin(["cat","dog"]) == "catdog"
T StringJoin(Characters("zebra")) == "zebra"

T ToUpperCase("cat") == "CAT"
T ToLowerCase("CAT") == "cat"

T UpperCaseQ("CAT")
T ! UpperCaseQ("Cat")
T LowerCaseQ("cat")
T ! LowerCaseQ("Cat")

T Head('A') == Char

T FromCharacterCode(ToCharacterCode("hello")) == "hello"

T FromCharacterCode([ [104,101,108,108,111], [104,101,108,108,111] ]) == ["hello","hello"]

T LetterNumber(Characters("abcxyz")) == [1,2,3,24,25,26]
T LetterNumber(Characters("AbCxYz")) == [1,2,3,24,25,26]

ClearAll(a,b)
