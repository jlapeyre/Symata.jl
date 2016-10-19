T LaTeXString(x) == "x"
T LaTeXString(x/y) == "\\frac{x}{y}"
T LaTeXString((a+b)/(c+d)) == "\\frac{a + b}{c + d}"
T LaTeXString(x*(a+b)/(c+d)) == "\\frac{ \\left( a + b \\right)  \\ x}{c + d}"
T LaTeXString(f(x)) == "f \\!  \\left( x \\right) "
T LaTeXString((a+b)*x^2 ) == " \\left( a + b \\right)  \\ x^{2}"

ClearAll(a,b,c,d,x,y,f)
