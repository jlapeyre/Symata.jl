T Latex(x) == "x"
T Latex(x/y) == "\\frac{x}{y}"
T Latex((a+b)/(c+d)) == "\\frac{a + b}{c + d}"
T Latex(x*(a+b)/(c+d)) == "\\frac{ \\left( a + b \\right)  \\ x}{c + d}"

ClearAll(a,b,c,d,x,y)
