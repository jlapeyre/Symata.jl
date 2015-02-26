module JSymPy

export sympy2mxpr, mxpr2sympy
export sympy

importall SJulia
import SJulia: mxpr  # we need this even with importall

using PyCall

# Initial Author: Francesco Bonazzi

## Convert SymPy to Mxpr

@pyimport sympy
@pyimport sympy.core as sympy_core

const SympySymbol = sympy_core.symbol["Symbol"]
const SympyAdd = sympy_core.add["Add"]
const SympyMul = sympy_core.mul["Mul"]
const SympyPow = sympy_core.power["Pow"]
const SympyNumber = sympy_core.numbers["Number"]
const SympyPi  = sympy_core.numbers["Pi"]
const SympyI  = sympy_core.numbers["ImaginaryUnit"]
#const SympyI  = sympy.I    # this is something, but not the right something
const SympySin = sympy.functions["sin"]
const SympyCos = sympy.functions["cos"]
const SympyTan = sympy.functions["tan"]
const SympyExp = sympy.functions["exp"]
const SympyLog = sympy.functions["log"]
const SympySqrt = sympy.functions["sqrt"]
const SymPyInfinity = sympy.oo
const SymPyComplexInfinity = sympy.zoo

# try to fix emacs indenting at some point!
const py_to_mx_dict = Dict(
    SympyAdd => :Plus,
    SympyMul => :Times,
    SympyPow => :Power,
    SympySin => :Sin,
    SympyCos => :Cos,
                       SympyTan => :Tan,
                       SympyExp => :Exp,
#                       sympy.E => :E,
#                       SympyExp => :Exp,
                       SympyLog => :Log,
                       SympySqrt => :Sqrt,
                       SymPyInfinity => :Infinity,
                       SymPyComplexInfinity => :ComplexInfinity
)

const pymx_special_symbol_dict = Dict (
                                       SympyPi => :Pi,
                                       SympyI => complex(0,1)
                                       )

sympy2mxpr(x) = x

# Need to detect Exp here and convert it to E^x for Julia
function sympy2mxpr{T <: PyCall.PyObject}(expr::T)
#    println("annot ", typeof(expr), " ",expr )
    if (pytypeof(expr) in keys(py_to_mx_dict))
        return SJulia.mxpr(py_to_mx_dict[pytypeof(expr)], map(sympy2mxpr, expr[:args])...)
    end
    if expr[:is_Function]       # perhaps a user defined function
        # objstr = split(string(pytypeof(expr)))
        # head = symbol(objstr[end])  # The string is "PyObject bb"
        head = symbol(pytypeof(expr)[:__name__])
        return SJulia.mxpr(head, map(sympy2mxpr, expr[:args])...)
    end
    for k in keys(pymx_special_symbol_dict)
        if pyisinstance(expr,k)
            return pymx_special_symbol_dict[k]
        end 
    end
    if pytypeof(expr) == SympySymbol
        return Symbol(expr[:name])
    end
    if pyisinstance(expr, SympyNumber)
        # Big ints are wrapped up in a bunch of stuff
        # There is a function n._to_mpmath(m) (dont know what m means) that returns GMP number useable by Julia
        # We need to check what kind of integer. searching methods. no luck
        # we could try catch _to_mpmath
        if expr[:is_Integer]
            #  Between SymPy and PyCall, there is a huge variety of
            #  ways that integers are wrapped in layers of classes. None if it is documented.
            #  apparently, if the number is not really a bigint, a machine sized int is actually returned
            #  Yes, tested this with Int64 and GMP ints and the correct thing is returned
            num = expr[:_to_mpmath](-1)  #  works for some numbers at the command line
            return num
        end
        if expr[:is_Rational]
            p = expr[:p]
            q = expr[:q]
            return Rational(expr[:p],expr[:q])  # These are Int64's. Don't know when or if they are BigInts
        end
        return convert(FloatingPoint, expr)
    end
    println("sympy2mxpr: Unable to translate ", expr)
    return expr
end

#function sympy2mxpr(x::Tuple)
#    return SJulia.mxpr(:List,map(sympy2mxpr,x)...)
#end

# By default, Dict goes to Dict
function sympy2mxpr(expr::Dict)
    ndict = Dict()
    for (k,v) in expr
        ndict[sympy2mxpr(k)] = sympy2mxpr(v)
    end
    return ndict
end

function sympy2mxpr{T}(expr::Array{T,1})
#    println("array ", typeof(expr))
    return SJulia.mxpr(:List,map(sympy2mxpr, expr)...)
end

#sympy2mxpr(x) = x

# TESTS

function test_sympy2mxpr()
    x, y, z = sympy.symbols("x y z")
    add1 = sympy.Add(x, y, z, 3)
    @assert sympy2mxpr(add1) == mxpr(:Plus, 3, :x, :y, :z)
    mul1 = sympy.Mul(x, y, z, -2)
    @assert sympy2mxpr(mul1) == mxpr(:Times, -2, :x, :y, :z)
    add2 = sympy.Add(x, mul1)
    @assert sympy2mxpr(add2) == mxpr(:Plus, :x, mxpr(:Times, -2, :x, :y, :z))
end

## Convert Mxpr to SymPy

const mx_to_py_dict = Dict(
    :Plus => sympy.Add,
    :Times => sympy.Mul,
    :Power => sympy.Pow,
    :Sin => sympy.sin,
    :Cos => sympy.cos,
    :Tan => sympy.tan,
    :Exp => sympy.exp, #  This works
    :Sqrt => sympy.sqrt,
    :E => sympy.E,
    :I => SympyI,
    :Pi => SympyPi,
    :Log => sympy.log,     
    :Infinity => sympy.oo,
    :ComplexInfinity => sympy.zoo
)

function mxpr2sympy(z::Complex)
#    println("Converting $z")
    if real(z) == 0
        res = mxpr(:Times, :I, imag(z))
    else
        res = mxpr(:Plus, real(z), mxpr(:Times, :I, imag(z)))
    end
#    println("got $res")
    return mxpr2sympy(res)
end

function mxpr2sympy(mx::SJulia.Mxpr{:List})
    return [map(mxpr2sympy, mx.args)...]
end

function mxpr2sympy(mx::SJulia.Mxpr)
    if mx.head in keys(mx_to_py_dict)
        return mx_to_py_dict[mx.head](map(mxpr2sympy, mx.args)...)
    end
#    if SJulia.is_Mxpr(mx,:List)
#        return [map(mxpr2sympy, mx.args)...]
#    end
    pyfunc = sympy.Function(string(mx.head))  # Don't recognize the head, so make it a user function
    return pyfunc(map(mxpr2sympy, mx.args)...)
end

function mxpr2sympy(mx::Symbol)
    if haskey(mx_to_py_dict,mx)
        return mx_to_py_dict[mx]
    end
    return sympy.Symbol(mx)
end

function mxpr2sympy(mx::SJulia.SSJSym)
    name = symname(mx)
    if haskey(mx_to_py_dict,name)
        return conv_rev[name]
    end
    return sympy.Symbol(name)
end

function mxpr2sympy(mx::Number)
    return mx
end

function mxpr2sympy(x)
    error("Can't convert $x from SJulia to SymPy")
end

# TEST

function test_mxpr2sympy()
    me1 = mxpr(:Plus, :a, :b,  mxpr(:Times, -3, :z, mxpr(:Power, :x, 2)))
    me2 = mxpr(:Times, 2, :x, :y)
    @assert sympy2mxpr(mxpr2sympy(me1)) == me1
    @assert sympy2mxpr(mxpr2sympy(me2)) == me2
end

end  # module SJulia.JSymPy
