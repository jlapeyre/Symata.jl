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

# TODO: generate this code, like the functions below
const SympySymbol = sympy_core.symbol["Symbol"]
const SympyAdd = sympy_core.add["Add"]
const SympyMul = sympy_core.mul["Mul"]
const SympyPow = sympy_core.power["Pow"]
# Following is the strategy that I thought of at the very beginning.
# It seems bassackwards, but it is more general than searching for each
# peculiar way of encoding a head:
# 1. a python instance is returned. we have no idea in general how to check
# other objects for its type.
# 2. construct an object and find it's type and assign to a constant.
const SympyD = pytypeof(sympy_core.Derivative(:x))
const Sympyintegrate = sympy.integrals["Integral"]
const SympyTuple = sympy.containers["Tuple"]

const SympyNumber = sympy_core.numbers["Number"]
const SympyPi  = sympy_core.numbers["Pi"]
const SympyI  = sympy_core.numbers["ImaginaryUnit"]
const SymPyInfinity = sympy.oo
const SymPyComplexInfinity = sympy.zoo

#### Convert SymPy to Mxpr

# I don't like the emacs indenting. try to fix this at some point!
const py_to_mx_dict =
    Dict(
         SympyAdd => :Plus,
         SympyMul => :Times,
         SympyPow => :Power,
         SympyD => :D,
         Sympyintegrate => :Integrate,
         SympyTuple => :List,
         SymPyInfinity => :Infinity,
         SymPyComplexInfinity => :ComplexInfinity
         )

function mk_py_to_mx_funcs()
    for (fstr,capstr) in
        (("sin","Sin"),("cos","Cos"),("tan","Tan"),("atan","ArcTan"),("acos","ArcCos"),("asin","ArcSin"),
         ("exp","Exp"),("log","Log"),("sqrt","Sqrt"),
         ("hankel1","HankelH1"),("hankel2","HankelH2"),
         ("besseli","BesselI"),("besselj","BesselJ"),("besselk","BesselK"),
         ("gamma","Gamma"),("digamma","Digamma"))
        csym = symbol("SymPy" * capstr)
        @eval begin
            const $csym = sympy.functions[$fstr]
            py_to_mx_dict[$csym] =  symbol($capstr)
        end
    end
end

mk_py_to_mx_funcs()


const pymx_special_symbol_dict =
    Dict (
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
            #  Apparently, if the number is not really a bigint, a machine sized int is actually returned
            #  Yes, tested this with Int64 and GMP ints and the correct thing is returned
            num = expr[:_to_mpmath](-1)  #  works for some numbers at the command line
            return num
        end
        if expr[:is_Rational]
            p = expr[:p]
            q = expr[:q]
            return Rational(expr[:p],expr[:q])  # These are Int64's. Don't know when or if they are BigInts
        end
        return convert(FloatingPoint, expr) # Need to check for big floats
    end
    println("sympy2mxpr: Unable to translate ", expr)
    return expr
end

# By default, Dict goes to Dict
function sympy2mxpr(expr::Dict)
    ndict = Dict()
    for (k,v) in expr
        ndict[sympy2mxpr(k)] = sympy2mxpr(v)
    end
    return ndict
end

function sympy2mxpr{T}(expr::Array{T,1})
    return SJulia.mxpr(:List,map(sympy2mxpr, expr)...)
end


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

#### Convert Mxpr to SymPy

# These should be generated... And why do we use instances here and classes above ?
# (if that is what is happening. )
const mx_to_py_dict =
    Dict(
         :Plus => sympy.Add,
         :Times => sympy.Mul,
         :Power => sympy.Pow,
         :E => sympy.E,
         :I => SympyI,
         :Pi => SympyPi,
         :Log => sympy.log, 
         :Infinity => sympy.oo,
         :ComplexInfinity => sympy.zoo
         )

for (sj,sp) in (("Exp","exp"),("Sqrt","sqrt"),("Sin","sin"),("Cos","cos"),("Tan","tan"),("ArcTan","atan"),
                ("ArcCos","acos"),("ArcSin","asin"),("Cosh","cosh"),("Sinh","sinh"),("Tanh","tanh"),
                ("Gamma","gamma"),("Digamma","digamma"),
                ("BesselI","besseli"),("BesselJ","besselj"),
                ("BesselK","besselk"),("BesselY","bessely"),
                ("HankelH1","hankel1"),("HankelH2","hankel2"))
    obj = eval(parse("sympy." * sp))
    @eval begin
        mx_to_py_dict[symbol($sj)] = $obj
    end
end


function mxpr2sympy(z::Complex)
#    println("Converting $z")
    if real(z) == 0
        res = mxpr(:Times, :I, imag(z))
    else
        res = mxpr(:Plus, real(z), mxpr(:Times, :I, imag(z)))
    end
    return mxpr2sympy(res)
end

function mxpr2sympy(mx::SJulia.Mxpr{:List})
    return [map(mxpr2sympy, mx.args)...]
end

function mxpr2sympy(mx::SJulia.Mxpr)
    if mx.head in keys(mx_to_py_dict)
        return mx_to_py_dict[mx.head](map(mxpr2sympy, mx.args)...)
    end
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

# Don't error, but only warn. Then return x so that we
# can capture and inspect it.
function mxpr2sympy(x)
    warn("Can't convert $x from SJulia to SymPy")
    return x
end

# TEST

function test_mxpr2sympy()
    me1 = mxpr(:Plus, :a, :b,  mxpr(:Times, -3, :z, mxpr(:Power, :x, 2)))
    me2 = mxpr(:Times, 2, :x, :y)
    @assert sympy2mxpr(mxpr2sympy(me1)) == me1
    @assert sympy2mxpr(mxpr2sympy(me2)) == me2
end

end  # module SJulia.JSymPy
