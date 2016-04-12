module JSymPy

export sympy2mxpr, mxpr2sympy
export sympy

importall SJulia
import SJulia: mxpr  # we need this even with importall

using PyCall

# Initial Author: Francesco Bonazzi

# We do not use SymPy.jl, but rather PyCall directly

## Convert SymPy to Mxpr

function import_sympy()
    eval(parse("@pyimport sympy"))
    eval(parse("@pyimport sympy.core as sympy_core"))
end

# Some SymPy functions are encoded this way. Others, not. Eg, Add, Mul are different
const SYMPY_TO_SJULIA_FUNCTIONS = Dict{Symbol,Symbol}()
const SJULIA_TO_SYMPY_FUNCTIONS = Dict{Symbol,Symbol}()
const mx_to_py_dict =  Dict()   # Can we specify types for these Dicts ?
const pymx_special_symbol_dict = Dict()

# TODO: populate this Dict. Collect the translation into fewer Dicts.
# Use this Dict to rely more on these lines:
#    head = sympy_to_mxpr_symbol(expr[:func][:__name__])  # Maybe we can move this further up ?
#    return SJulia.mxpr(head, map(sympy2mxpr, expr[:args])...)
const py_to_mx_symbol_dict = Dict(
                                  :StrictLessThan => :<
                                  )

# sympy has erf and erf2. we need to check number of args.

function get_sympy_math(x)
    if length(x) == 1
        jf = x[1]
        st = string(jf)
        sjf = uppercase(string(st[1])) * st[2:end]
    elseif length(x) == 2
        (jf,sjf) = x
    else
        error("get_sympy_math: wrong number of args in $x")
    end
    return jf,sjf
end


# TODO: Organize these. They are largely copied from ./math_functions.jl
# These should be pulled out of the appropriate lists in ./math_functions.jl
function make_sympy_to_sjulia()
    single_arg_float_complex =
        [ (:sin,), (:cos,), (:tan,), (:sinh,),(:cosh,), (:Si, :SinIntegral), (:Ci, :CosIntegral),
          (:tanh,), (:acos,:ArcCos), (:asin,:ArcSin),(:atan,:ArcTan),(:atan2,:ArcTan2),
         (:sec,),(:csc,),(:cot,), (:exp,), (:sqrt,),(:log,),
         (:asec,:ArcSec),(:acsc,:ArcCsc),(:acot,:ArcCot),
         (:coth,),(:asinh,:ASinh),(:acosh,:ACosh),(:atanh,:ATanh),
         (:acoth,:ArcCoth),
         (:erf,),(:erfc,),(:erfi,),(:re,:Re),(:im,:Im),
         (:arg,:Arg),(:gamma,),(:loggamma,:LogGamma),
         (:digamma,),(:trigamma,),(:polygamma,), (:airyai,:AiryAi),
         (:airybi,:AiryBi),(:airyaiprime,:AiryAiPrime),(:airybiprime,:AiryBiPrime),
         (:besselj,:BesselJ),(:besseli,:BesselI),(:besselk,:BesselK),(:bessely,:BesselY),
         (:zeta,), (:LambertW, :LambertW)
         ]

    single_arg_float_int_complex =
        [
         (:conjugate,)
         ]

    single_arg_float = [(:cbrt,:CubeRoot),(:erfinv,:InverseErf),(:erfcinv,:InverseErfc)
                        ]

    single_arg_float_int = [(:factorial,),(:sign,)]

    single_arg_int = [(:integer_nthroot,:IntegerNthRoot),(:nextprime, :NextPrime), (:prevprime, :PrevPrime),
                      (:isprime,:PrimeQ)
                        ]

    two_arg_int = [(:binomial,)
                   ]

    two_arg_float_and_float_or_complex =
     [
      (:besselj,:BesselJ),(:bessely,:BesselY),
      (:hankel1,:HankelH1),
      (:hankel2,:HankelH2),(:besseli,:BesselI),
      (:besselk,:BesselK)
      ]

     two_arg_float = [ (:beta,)]

    symbolic_misc = [ (:Order, :Order), (:harmonic,), (:laplace_transform, :LaplaceTransform) ]

    for funclist in (single_arg_float_complex, single_arg_float_int_complex, single_arg_float,
                     single_arg_float_int, single_arg_int, two_arg_int, two_arg_float_and_float_or_complex, two_arg_float,
                     symbolic_misc)
        for x in funclist
            sympy_func, sjulia_func = get_sympy_math(x)
            SYMPY_TO_SJULIA_FUNCTIONS[sympy_func] = sjulia_func
        end
    end
    for (k,v) in SYMPY_TO_SJULIA_FUNCTIONS
        SJULIA_TO_SYMPY_FUNCTIONS[v] = k
    end
end

####################

#### Convert SymPy to Mxpr

# I don't like the emacs indenting. try to fix this at some point!

function populate_py_to_mx_dict()
    # If this is Dict{Any,Any}, then nothing is translated until the
    # catchall branch at the end of sympyt2mxpr. Why ?    
    eval(parse("const py_to_mx_dict = Dict{PyCall.PyObject,Symbol}()"))
    for onepair in (
         (sympy.Add, :Plus),
         (sympy.Mul, :Times),
         (sympy.Pow ,:Power),
         (sympy.Derivative, :D),
         (sympy.integrals["Integral"], :Integrate),
         (sympy.containers[:Tuple], :List),  # Problem, a List may be huge, Tuple not... but this is a sympy Tuple
         (sympy.oo, :Infinity),
         (sympy.zoo,:ComplexInfinity))
        py_to_mx_dict[onepair[1]] = onepair[2]
    end
end

## populate_py_to_mx_dict()

function mk_py_to_mx_funcs()
    for (pysym,sjsym) in SYMPY_TO_SJULIA_FUNCTIONS
        pystr = string(pysym)
        sjstr = string(sjsym)
        if haskey(sympy.functions, pystr)
            py_to_mx_dict[sympy.functions[pystr]] =  symbol(sjstr)
        end
    end
end


function populate_special_symbol_dict()
    for onepair in (
          (sympy_core.numbers["Pi"], :Pi),
          (sympy.numbers["Exp1"],  :E),
          (sympy_core.numbers["ImaginaryUnit"], complex(0,1)))
        pymx_special_symbol_dict[onepair[1]] = onepair[2]
    end
end

# Convert symbol if it is on a list
function sympy_to_mxpr_symbol(s::Symbol)
    if haskey(py_to_mx_symbol_dict, s)
        py_to_mx_symbol_dict[s]
    else
        s
    end
end

sympy_to_mxpr_symbol(s::AbstractString) = sympy_to_mxpr_symbol(Symbol(s))

sympy2mxpr(x) = x

function sympy2mxpr{T <: PyCall.PyObject}(expr::T)
    if haskey(py_to_mx_dict, pytypeof(expr))
        return SJulia.mxpr(py_to_mx_dict[pytypeof(expr)], map(sympy2mxpr, expr[:args])...)
    end
    if expr[:is_Function]       # perhaps a user defined function
        head = symbol(pytypeof(expr)[:__name__])
        return SJulia.mxpr(head, map(sympy2mxpr, expr[:args])...)
    end
    for k in keys(pymx_special_symbol_dict)
        if pyisinstance(expr,k)
            return pymx_special_symbol_dict[k]
        end
    end
    if pytypeof(expr) == sympy.Symbol
#        return Symbol(expr[:name])
        return sympy_to_mxpr_symbol(expr[:name])
    end
    if pyisinstance(expr, sympy.Number)
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
        return convert(AbstractFloat, expr) # Need to check for big floats
    end
    head = sympy_to_mxpr_symbol(expr[:func][:__name__])  # Maybe we can move this further up ?
    return SJulia.mxpr(head, map(sympy2mxpr, expr[:args])...)
#    println("sympy2mxpr: Unable to translate ", expr)
#    return expr
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

function sympy2mxpr(expr::Tuple)
    return SJulia.mxpr(:List,map(sympy2mxpr, expr)...)
end

#### Convert Mxpr to SymPy

# These should be generated... And why do we use instances here and classes above ?
# (if that is what is happening. )
function populate_mx_to_py_dict()
    for onepair in (
         (:Plus,sympy.Add),
         (:Times, sympy.Mul),
         (:Power, sympy.Pow),
         (:E, sympy.E),
         (:I,sympy_core.numbers["ImaginaryUnit"]),
         (:Pi,  sympy.pi),
         (:Log, sympy.log),
         (:Infinity, sympy.oo),
         (:ComplexInfinity, sympy.zoo))
        mx_to_py_dict[onepair[1]] = onepair[2]
    end
end

# This should be correct! Compare commented out method above
function mk_mx_to_py_funcs()
    for (sjsym,pysym) in SJULIA_TO_SYMPY_FUNCTIONS
        pystr = string(pysym)
        sjstr = string(sjsym)
        obj = eval(parse("sympy." * pystr))
        mx_to_py_dict[symbol(sjstr)] = obj
    end
end

function mxpr2sympy(z::Complex)
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

function mxpr2sympy(mx::Rational)
    return sympy.Rational(num(mx),den(mx))
end

function mxpr2sympy(mx::Number)
    return mx
end

# For our LaplaceTransform code, (etc.)
mxpr2sympy(a::Array{Any,1}) =  map(mxpr2sympy, a)

# Don't error, but only warn. Then return x so that we
# can capture and inspect it.
function mxpr2sympy(x)
    warn("mxpr2sympy: Can't convert $x from SJulia to SymPy")
    return x
end

# We call init_sympy() from __init__
function init_sympy()
    import_sympy()
    make_sympy_to_sjulia()
    populate_py_to_mx_dict()
    mk_py_to_mx_funcs()
    populate_special_symbol_dict()
    populate_mx_to_py_dict()
    mk_mx_to_py_funcs()
end

# TESTS
# function test_sympy2mxpr()
#     x, y, z = sympy.symbols("x y z")
#     add1 = sympy.Add(x, y, z, 3)
#     @assert sympy2mxpr(add1) == mxpr(:Plus, 3, :x, :y, :z)
#     mul1 = sympy.Mul(x, y, z, -2)
#     @assert sympy2mxpr(mul1) == mxpr(:Times, -2, :x, :y, :z)
#     add2 = sympy.Add(x, mul1)
#     @assert sympy2mxpr(add2) == mxpr(:Plus, :x, mxpr(:Times, -2, :x, :y, :z))
# end
# function test_mxpr2sympy()
#     me1 = mxpr(:Plus, :a, :b,  mxpr(:Times, -3, :z, mxpr(:Power, :x, 2)))
#     me2 = mxpr(:Times, 2, :x, :y)
#     @assert sympy2mxpr(mxpr2sympy(me1)) == me1
#     @assert sympy2mxpr(mxpr2sympy(me2)) == me2
# end

end  # module SJulia.JSymPy
