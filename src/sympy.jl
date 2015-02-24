module JSymPy

export sympy2mxpr, mxpr2sympy
export sympy

importall SJulia

#import SymPy
import SJulia: mxpr

using PyCall

# Author: Francesco Bonazzi

## Convert SymPy to Mxpr

@pyimport sympy
@pyimport sympy.core as sympy_core

const SympySymbol = sympy_core.symbol["Symbol"]
const SympyAdd = sympy_core.add["Add"]
const SympyMul = sympy_core.mul["Mul"]
const SympyPow = sympy_core.power["Pow"]
const SympyNumber = sympy_core.numbers["Number"]
const SympyPi  = sympy_core.numbers["Pi"]
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
)


# function disablesympy2mxpr(exp_tree)
#     println("plain ", typeof(exp_tree))
#     if (pytypeof(exp_tree) in keys(conv_dict))
#         return SJulia.mxpr(conv_dict[pytypeof(exp_tree)], map(sympy2mxpr, exp_tree[:args])...)
#     end
#     if exp_tree[:is_Function]       # perhaps a user defined function
#         # objstr = split(string(pytypeof(exp_tree)))
#         # head = symbol(objstr[end])  # The string is "PyObject bb"
#         head = symbol(pytypeof(exp_tree)[:__name__])
#         return SJulia.mxpr(head, map(sympy2mxpr, exp_tree[:args])...)
#     end
#     if pyisinstance(exp_tree,SympyPi) return :Pi end
#     if pytypeof(exp_tree) == SympySymbol
#         return Symbol(exp_tree[:name])
#     end
#     if pyisinstance(exp_tree, SympyNumber)
#         if exp_tree[:is_Integer]
#             return convert(BigInt, exp_tree)
#         end
#         if exp_tree[:is_Rational]
#             return Rational(exp_tree[:p],exp_tree[:q])  # These are Int64's. Don't know when or if they are BigInts
#         end 
#         return convert(FloatingPoint, exp_tree)
#     end
#     if isa(exp_tree,Tuple)
#         println("tuple ", exp_tree)
#         return SJulia.mxpr(:List,map(sympy2mxpr,exp_tree)...)
#     end
# end

sympy2mxpr(x) = x

function sympy2mxpr{T <: PyCall.PyObject}(expr::T)
#    println("annot ", typeof(expr))
    if (pytypeof(expr) in keys(conv_dict))
        return SJulia.mxpr(conv_dict[pytypeof(expr)], map(sympy2mxpr, expr[:args])...)
    end
    if expr[:is_Function]       # perhaps a user defined function
        # objstr = split(string(pytypeof(expr)))
        # head = symbol(objstr[end])  # The string is "PyObject bb"
        head = symbol(pytypeof(expr)[:__name__])
        return SJulia.mxpr(head, map(sympy2mxpr, expr[:args])...)
    end
    if pyisinstance(expr,SympyPi) return :Pi end
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
    :Pi => SympyPi,
    :Log => sympy.log,     
    :Infinity => sympy.oo,
    :ComplexInfinity => sympy.zoo
)   

function mxpr2sympy(mex)
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
    if mex.head in keys(conv_rev)
        return conv_rev[mex.head](map(mxpr2sympy, mex.args)...)
    end
    if SJulia.is_Mxpr(mex,:List)
        return [map(mxpr2sympy, mex.args)...]
    end
    pyfunc = sympy.Function(string(mex.head))  # Don't recognize the head, so make it a user function
    return pyfunc(map(mxpr2sympy, mex.args)...)
end

# TEST

function test_mxpr2sympy()
    me1 = mxpr(:Plus, :a, :b,  mxpr(:Times, -3, :z, mxpr(:Power, :x, 2)))
    me2 = mxpr(:Times, 2, :x, :y)
    @assert sympy2mxpr(mxpr2sympy(me1)) == me1
    @assert sympy2mxpr(mxpr2sympy(me2)) == me2
end

end  # module SJulia.JSymPy
