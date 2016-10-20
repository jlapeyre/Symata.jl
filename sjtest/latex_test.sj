T LaTeXString(x) == "x"
T LaTeXString(1/2) == "\\frac{1}{2}"
T LaTeXString(x/y) == "\\frac{x}{y}"
T LaTeXString((a+b)/(c+d)) == "\\frac{a + b}{c + d}"
T LaTeXString(x*(a+b)/(c+d)) == "\\frac{ \\left( a + b \\right)  \\ x}{c + d}"
T LaTeXString(f(x)) == "f \\!  \\left( x \\right) "
T LaTeXString((a+b)*x^2 ) == " \\left( a + b \\right)  \\ x^{2}"
T LaTeXString(((1 + x)^(-1))*y - ((2 + x)^(-1))*y) == "\\frac{y}{1 + x} + \\frac{- \\ y}{2 + x}"

ClearAll(a,b,c,d,x,y,f)
