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



const conv_dict = Dict(
    SympyAdd => :Plus,
    SympyMul => :Times,
    SympyPow => :Power,
    SympySin => :Sin,
    SympyCos => :Cos,
                       SympyTan => :Tan,
                       SympyExp => :Exp,
                       sympy.E => :E,
                       SympyLog => :Log,
                       SympySqrt => :Sqrt,
                       sympy.oo => :Infinity,
                       sympy.zoo => :ComplexInfinity
#                       SympyI => :I  # don't want these, they return functions.
)

const pymx_special_symbol_dict = Dict (
                                       SympyPi => :Pi,
                                       SympyI => complex(0,1)
                                       )

sympy2mxpr(x) = x

function sympy2mxpr{T <: PyCall.PyObject}(expr::T)
#    println("annot ", typeof(expr), " ",expr )
    if (pytypeof(expr) in keys(conv_dict))
        return SJulia.mxpr(conv_dict[pytypeof(expr)], map(sympy2mxpr, expr[:args])...)
    end
    if expr[:is_Function]       # perhaps a user defined function
        # objstr = split(string(pytypeof(expr)))
        # head = symbol(objstr[end])  # The string is "PyObject bb"
        head = symbol(pytypeof(expr)[:__name__])
        return SJulia.mxpr(head, map(sympy2mxpr, expr[:args])...)
    end
    for k in keys(pymx_special_symbol_dict)
        if pyisinstance(expr,k) return pymx_special_symbol_dict[k] end 
    end
    if pytypeof(expr) == SympySymbol
        return Symbol(expr[:name])
    end
    if pyisinstance(expr, SympyNumber)
        if expr[:is_Integer]
            return convert(BigInt, expr)
        end
        if expr[:is_Rational]
            return Rational(expr[:p],expr[:q])  # These are Int64's. Don't know when or if they are BigInts
        end
#        println("Doing floating point $expr")
        return convert(FloatingPoint, expr)
    end
    println("sympy2mxpr: Unable to translate ", expr)
    return expr
#    if isa(expr,Tuple)
#        println("tuple ", expr)
#        return SJulia.mxpr(:List,map(sympy2mxpr,expr)...)
#    end
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
    println("array ", typeof(expr))
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

const conv_rev = Dict(
    :Plus => sympy.Add,
    :Times => sympy.Mul,
    :Power => sympy.Pow,
    :Sin => sympy.sin,
    :Cos => sympy.cos,
    :Tan => sympy.tan,
    :Exp => sympy.exp,
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

function mxpr2sympy(mex::SJulia.Mxpr)
    if mex.head in keys(conv_rev)
        return conv_rev[mex.head](map(mxpr2sympy, mex.args)...)
    end
    if SJulia.is_Mxpr(mex,:List)
        return [map(mxpr2sympy, mex.args)...]
    end
    pyfunc = sympy.Function(string(mex.head))  # Don't recognize the head, so make it a user function
    return pyfunc(map(mxpr2sympy, mex.args)...)
end

function mxpr2sympy(mex)
#   println("Doing $mex")            
    if !isa(mex, SJulia.Mxpr)
        if isa(mex, Symbol)
            if haskey(conv_rev,mex)
                return conv_rev[mex]
            end
            return sympy.Symbol(mex)
        end
        if SJulia.is_type_less(mex,SJulia.SSJSym)
            name = symname(mex)
            if haskey(conv_rev,name)
                return conv_rev[name]
            end
            return sympy.Symbol(name)            
        end
        if isa(mex, Number)
            return mex
        end
    end
    # if mex.head in keys(conv_rev)
    #     return conv_rev[mex.head](map(mxpr2sympy, mex.args)...)
    # end
    # if SJulia.is_Mxpr(mex,:List)
    #     return [map(mxpr2sympy, mex.args)...]
    # end
    # pyfunc = sympy.Function(string(mex.head))  # Don't recognize the head, so make it a user function
    # return pyfunc(map(mxpr2sympy, mex.args)...)
end

# TEST

function test_mxpr2sympy()
    me1 = mxpr(:Plus, :a, :b,  mxpr(:Times, -3, :z, mxpr(:Power, :x, 2)))
    me2 = mxpr(:Times, 2, :x, :y)
    @assert sympy2mxpr(mxpr2sympy(me1)) == me1
    @assert sympy2mxpr(mxpr2sympy(me2)) == me2
end

end  # module SJulia.JSymPy
