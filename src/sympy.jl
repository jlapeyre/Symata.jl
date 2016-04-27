using PyCall

# We no longer have a module here.
# export sympy2mxpr, mxpr2sympy
# export sympy

#importall SJulia
#import SJulia: mxpr, mxprcf  # we need this even with importall

# Notes for evenutally extracting mpz and mpf from python mpmath objects
#  julia> ex[:_mpf_][2]
#  PyObject mpz(5400274185508743)
#  julia> ex
#  PyObject mpf('0.29977543700203352')

# Initial Author: Francesco Bonazzi

# We do not use SymPy.jl, but rather PyCall directly

## Convert SymPy to Mxpr

function import_sympy()
    eval(parse("@pyimport sympy"))
    eval(parse("@pyimport sympy.core as sympy_core"))
    eval(parse("@pyimport mpmath"))
end

# Some SymPy functions are encoded this way. Others, not. Eg, Add, Mul are different
const SYMPY_TO_SJULIA_FUNCTIONS = Dict{Symbol,Symbol}()
const SJULIA_TO_SYMPY_FUNCTIONS = Dict{Symbol,Symbol}()
const mx_to_py_dict =  Dict()   # Can we specify types for these Dicts ?
const pymx_special_symbol_dict = Dict()

const MPMATH_DPS = Int[0]

get_mpmath_dps() = mpmath.mp[:dps]
function set_mpmath_dps(n)
    push!(MPMATH_DPS,mpmath.mp[:dps])
    mpmath.mp[:dps] = n
end
    
restore_mpmath_dps() = (mpmath.mp[:dps] = pop!(MPMATH_DPS))


# TODO: populate this Dict. Collect the translation into fewer Dicts.
# Use this Dict to rely more on these lines:
#    head = sympy_to_mxpr_symbol(expr[:func][:__name__])  # Maybe we can move this further up ?
#    return SJulia.mxpr(head, map(sympy2mxpr, expr[:args])...)

const py_to_mx_symbol_dict = Dict(
                                  :StrictLessThan => :<,
                                  :StrictGreaterThan => :>,
                                  :LessThan => :<=,
                                  :GreaterThan => :>=,
                                  :uppergamma => :Gamma,
                                  :Equality => :(==),
                                  :Unequality => :(!=)
                                  )

# this is not needed
const py_to_mx_function_dict = Dict(
#                                    :polar_lift => :PolarLift,
#                                    :periodic_argument => :PeriodicArgument
                                    )

# populated below
const py_to_mx_rewrite_function_dict = Dict(
                                            )

# sympy has erf and erf2. we need to check number of args.

function get_sympy_math(x)
    if length(x) == 1
        jf = x[1]
        st = string(jf)
        sjf = SJulia.capitalize_first_character(st)
    elseif length(x) == 2
        (jf,sjf) = x
    else
        error("get_sympy_math: wrong number of args in $x")
    end
    return jf,sjf
end

function make_sympy_to_sjulia()
    symbolic_misc = [ (:Order, :Order), (:LaplaceTransform, :laplace_transform),
                      ( :InverseLaplaceTransform, :inverse_laplace_transform ),
                      (:InverseFourierTransform, :inverse_fourier_transform ),
                      (:FourierTransform, :fourier_transform),
                      (:Cos, :cos), (:Log, :log), ( :Sqrt, :sqrt), (:ProductLog, :LambertW),
                      (:Exp, :exp), (:Abs, :Abs), (:MeijerG, :meijerg), (:PolarLift, :polar_lift),
                      (:ExpPolar, :exp_polar), (:LowerGamma, :lowergamma),
                      (:PeriodicArgument, :periodic_argument)
                      ]

    for funclist in (single_arg_float_complex, single_arg_float_int_complex, single_arg_float,
                     single_arg_float_int, single_arg_int, two_arg_int,
                     two_arg_float_and_float_or_complex, two_arg_float,
                     one_or_two_args1
                     )
        for x in funclist
            if length(x) != 3 continue end
            (julia_func, sjulia_func, sympy_func) = x
            SYMPY_TO_SJULIA_FUNCTIONS[sympy_func] = sjulia_func
        end
    end

    for funclist in (symbolic_misc, no_julia_function, no_julia_function_one_arg,
                     no_julia_function_one_or_two_int,
                     no_julia_function_two_args, no_julia_function_two_or_three_args,
                     no_julia_function_three_args, no_julia_function_four_args)
        for x in funclist
            sjulia_func, sympy_func = get_sympy_math(x)
            SYMPY_TO_SJULIA_FUNCTIONS[sympy_func] = sjulia_func
        end
    end

    for (k,v) in SYMPY_TO_SJULIA_FUNCTIONS
        SJULIA_TO_SYMPY_FUNCTIONS[v] = k
    end
    SYMPY_TO_SJULIA_FUNCTIONS[:uppergamma] = :Gamma  # :Gamma corresponds to two sympy funcs
    SYMPY_TO_SJULIA_FUNCTIONS[:InverseLaplaceTransform] = :InverseLaplaceTransform
#    SYMPY_TO_SJULIA_FUNCTIONS[:TupleArg] = :List does not work
end

function register_sjfunc_pyfunc{T<:Union{AbstractString,Symbol}, V<:Union{AbstractString,Symbol}}(sj::T, py::V)
    SYMPY_TO_SJULIA_FUNCTIONS[symbol(py)] = symbol(sj)
    SJULIA_TO_SYMPY_FUNCTIONS[symbol(sj)] = symbol(py)
end

function have_pyfunc_symbol(sjsym)
    haskey(SJULIA_TO_SYMPY_FUNCTIONS, sjsym)
end

function lookup_pyfunc_symbol(sjsym)
    SJULIA_TO_SYMPY_FUNCTIONS[sjsym]
end

####################

#### Convert SymPy to Mxpr

#  Note: To convert, say a hypergeometric function to a float, do this:  f1[:evalf]()

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
                    (sympy.zoo,:ComplexInfinity),
        (sympy.functions[:special][:hyper][:TupleArg], :List))
        py_to_mx_dict[onepair[1]] = onepair[2]
    end
end

function mk_py_to_mx_funcs()
    for (pysym,sjsym) in SYMPY_TO_SJULIA_FUNCTIONS
        pystr = string(pysym)
        sjstr = string(sjsym)
        if haskey(sympy.functions, pystr)
            py_to_mx_dict[sympy.functions[pystr]] =  symbol(sjstr)
        end
    end
end

function rewrite_function_sympy_to_julia(expr)
    func = py_to_mx_rewrite_function_dict[name(expr)]
    func(expr)
end


have_function_sympy_to_sjulia_translation{T <: PyCall.PyObject}(expr::T) = haskey(py_to_mx_dict, pytypeof(expr))
get_function_sympy_to_sjulia_translation{T <: PyCall.PyObject}(expr::T) = py_to_mx_dict[pytypeof(expr)]
have_rewrite_function_sympy_to_julia{T <: PyCall.PyObject}(expr::T) = haskey(py_to_mx_rewrite_function_dict, name(expr))



function populate_special_symbol_dict()
    for onepair in (
                    (sympy_core.numbers["Pi"], :Pi),
                    (sympy.numbers["Exp1"],  :E),
                    (sympy_core.numbers["ImaginaryUnit"], complex(0,1)),
                    (sympy_core.numbers["NegativeInfinity"], MinusInfinity))
        pymx_special_symbol_dict[onepair[1]] = onepair[2]
    end
end

sympy_to_mxpr_symbol(s::Symbol) = haskey(py_to_mx_symbol_dict, s) ? py_to_mx_symbol_dict[s] : s
sympy_to_mxpr_symbol{T<:AbstractString}(s::T) = sympy_to_mxpr_symbol(Symbol(s))

function maybe_sympy2mxpr{T}(x::T)
    symval(:ReturnSymPy!) == true && return x
    sympy2mxpr(x)
end

type SympyTrace
    trace::Bool
end
const SYMPYTRACE = SympyTrace(false)
const SYMPYTRACENUM = Int[0]
get_sympy2mxpr_count() = SYMPYTRACENUM[1]
increment_sympy2mxpr_count() = SYMPYTRACENUM[1] += 1
decrement_sympy2mxpr_count() = SYMPYTRACENUM[1] -= 1
is_sympy2mxpr_trace() = SYMPYTRACE.trace

function sympy2mxpr(expr)
    increment_sympy2mxpr_count()
    if is_sympy2mxpr_trace()
        ind = " " ^ (get_sympy2mxpr_count() - 1)
        println(ind,">>", get_sympy2mxpr_count(), " " , expr)
    end
    res = _sympy2mxpr(expr)
    if is_sympy2mxpr_trace()
        ind = " " ^ (get_sympy2mxpr_count() - 1)
        println(ind,"<<", get_sympy2mxpr_count(), " " , res)
    end
    decrement_sympy2mxpr_count()
    mergeargs(res)  # this may not be enough. only looks at one level, I think
    res
end

_sympy2mxpr(x) = x

function sympy2mxpr_Function(pyexpr)
    head = symbol(name(pyexpr))
#    haskey(py_to_mx_function_dict, head) ? head = py_to_mx_function_dict[head] : nothing
    targs = pyexpr[:args]
    if targs[1] == dummy_arg  # sympy does not allow functions without args, so we pass a dummy arg.
        return mxprcf(head, [])
    else
        return mxpr(head, map(sympy2mxpr, targs)...)
    end
end

macro sympy2mxpr_comparisons(fname, pyfname, sjsymbolstr)
    sfname = symbol("sympy2mxpr_" * fname)
    sjsymbol = parse(":(" * sjsymbolstr * ")")  # careful we don't insert an unquoted symbol
    esc(quote
        function ($sfname)(pyexpr)
          args = pyexpr[:args]
          return mxpr(:Comparison, _sympy2mxpr(args[1]), $sjsymbol, _sympy2mxpr(args[2]))
        end
        py_to_mx_rewrite_function_dict[$pyfname] = $sfname
       end)
end

@sympy2mxpr_comparisons("greater_than_equal", "GreaterThan", ">=")
@sympy2mxpr_comparisons("less_than_equal", "LessThan", "<=")
@sympy2mxpr_comparisons("less_than", "StrictLessThan", "<")
@sympy2mxpr_comparisons("greater_than", "StrictGreaterThan", ">")
@sympy2mxpr_comparisons("equality", "Equality", "==")
@sympy2mxpr_comparisons("unequality", "Unequality", "!=")


# function sympy2mxpr_less_than_equal(pyexpr)
#     args = pyexpr[:args]
#     return mxpr(:Comparison, sympy2mxpr(args[1]), :<=, sympy2mxpr(args[2]))
# end
# py_to_mx_rewrite_function_dict["LessThan"] = sympy2mxpr_less_than_equal


function sympy2mxpr_BooleanTrue(pyexpr)
    return true
end
py_to_mx_rewrite_function_dict["BooleanTrue"] = sympy2mxpr_BooleanTrue


function _sympy2mxpr{T <: PyCall.PyObject}(expr::T)
    if have_function_sympy_to_sjulia_translation(expr)
        return mxpr(get_function_sympy_to_sjulia_translation(expr), map(sympy2mxpr, expr[:args])...)
    end
    if have_rewrite_function_sympy_to_julia(expr)
        return rewrite_function_sympy_to_julia(expr)
    end
    if expr[:is_Function] return sympy2mxpr_Function(expr) end   # perhaps a user defined function
    for k in keys(pymx_special_symbol_dict)
        if pyisinstance(expr,k)
            return pymx_special_symbol_dict[k]
        end
    end
    if pytypeof(expr) == sympy.Symbol
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
        if ! expr[:is_finite]
            # we should check name. there are several infinities
            return Infinity  # need to see if this is maybe -Infinity
        end
# should we check for floats earlier ?
        return convert(AbstractFloat, expr) # Need to check for big floats
    end
    head = sympy_to_mxpr_symbol(expr[:func][:__name__])  # default
    return mxpr(head, map(sympy2mxpr, expr[:args])...)
end

# By default, Dict goes to Dict
function _sympy2mxpr(expr::Dict)
    ndict = Dict()
    for (k,v) in expr
        ndict[sympy2mxpr(k)] = sympy2mxpr(v)
    end
    return ndict
end

function _sympy2mxpr{T}(expr::Array{T,1})
    return mxpr(:List,map(sympy2mxpr, expr)...)
end

function _sympy2mxpr(expr::Tuple)
    return mxpr(:List,map(sympy2mxpr, expr)...)
end

#### Convert Mxpr to SymPy

# We use instances here and classes to go the other direction.
# This is because the name cannot be obtained from an instance.
function populate_mx_to_py_dict()
    for onepair in (
         (:Plus,sympy.Add),
         (:Times, sympy.Mul),
         (:Power, sympy.Pow),
         (:E, sympy.E),
         (:I, sympy.I),
         (:Pi,  sympy.pi),
         (:Log, sympy.log),
         (:Infinity, sympy.oo),
         (:ComplexInfinity, sympy.zoo))
        mx_to_py_dict[onepair[1]] = onepair[2]
    end
end

function mk_mx_to_py_funcs()
    for (sjsym,pysym) in SJULIA_TO_SYMPY_FUNCTIONS
        pystr = string(pysym)
        sjstr = string(sjsym)
        obj = eval(parse("sympy." * pystr))
        mx_to_py_dict[symbol(sjstr)] = obj
    end
end

#######################################################
##     mxpr2sympy
#######################################################

function mxpr2sympy(args...)
    if length(args) == 1
        res = _mxpr2sympy(args[1])
        return res
    end
    res = _mxpr2sympy(args)
    (res...)
end

function _mxpr2sympy(z::Complex)
    if real(z) == 0
        res = mxpr(:Times, :I, imag(z))
    else
        res = mxpr(:Plus, real(z), mxpr(:Times, :I, imag(z)))
    end
    return _mxpr2sympy(res)
end

function _mxpr2sympy(mx::Mxpr{:List})
    return [map(_mxpr2sympy, mx.args)...]
end

# This is never used. (Yes it is!)
function _mxpr2sympy(mx::Mxpr{:Gamma})
    ma = margs(mx)
    if length(ma) == 1
        sympy.gamma(_mxpr2sympy(ma[1]))
    elseif length(ma) == 2
        pyargs = map(_mxpr2sympy,ma)
        result = sympy.uppergamma(pyargs...)
        result
    else
        sympy.gamma(map(_mxpr2sympy,ma)...)
    end
end

# For now all infinities are mapped to one of two infinities
function _mxpr2sympy(mx::Mxpr{:DirectedInfinity})
    if mx == ComplexInfinity
        sympy.zoo
    elseif mx == MinusInfinity
        SymPyMinusInfinity
    else
        sympy.oo
    end
end

###### HypergeometricPFQ

do_HypergeometricPFQ{W<:AbstractFloat}(mx::Mxpr{:HypergeometricPFQ}, p::Mxpr{:List}, q::Mxpr{:List}, z::W) =
    eval_hypergeometric(mx,p,q,z)

do_HypergeometricPFQ{W<:AbstractFloat}(mx::Mxpr{:HypergeometricPFQ}, p::Mxpr{:List}, q::Mxpr{:List}, z::Complex{W}) =
    eval_hypergeometric(mx,p,q,z)

function eval_hypergeometric(mx, p, q, z)
    result = mxpr2sympy(mx)
    fresult =
        try
            result[:evalf]()
        catch
            result
        end
    sympy2mxpr(fresult)
end

@mkapprule MeijerG

function do_MeijerG(mx::Mxpr{:MeijerG}, p::Mxpr{:List}, q::Mxpr{:List}, z)
    mxc = copy(mx)
    mxc[1] = (p[1],p[2])
    mxc[2] = (q[1],q[2])
    try
        zpy = mxpr2sympy(z)
        ppy = (_mxpr2sympy(p[1]), _mxpr2sympy(p[2]))
        qpy = (_mxpr2sympy(q[1]), _mxpr2sympy(q[2]))
        pyres = sympy.meijerg(ppy,qpy,zpy)
        sjres = sympy2mxpr(pyres)
    catch
        mx
    end
end

function _mxpr2sympy(mx::Mxpr{:MeijerG})
    pyhead = mx_to_py_dict[mhead(mx)]
    p = mx[1]
    q = mx[2]
    z = mx[3]
    pyhead((_mxpr2sympy(p[1]), _mxpr2sympy(p[2])), (_mxpr2sympy(q[1]), _mxpr2sympy(q[2])), _mxpr2sympy(z))
end

do_MeijerG{W<:AbstractFloat}(mx::Mxpr{:MeijerG}, p::Mxpr{:List}, q::Mxpr{:List}, z::W) =
    eval_meijerg(mx,p,q,z)

do_MeijerG{W<:AbstractFloat}(mx::Mxpr{:MeijerG}, p::Mxpr{:List}, q::Mxpr{:List}, z::Complex{W}) =
    eval_meijerg(mx,p,q,z)

function eval_meijerg(mx, p, q, z)
    mxc = copy(mx)
    mxc[1] = (p[1],p[2])
    mxc[2] = (q[1],q[2])
    result = mxpr2sympy(mxc)
    fresult =
        try
            result[:evalf]()
        catch
            result
        end
    sympy2mxpr(fresult)
end

function _mxpr2sympy(t::Tuple)
    (map(_mxpr2sympy,t)...)
end


######

function _mxpr2sympy(mx::Mxpr)
    if mhead(mx) in keys(mx_to_py_dict)
        return mx_to_py_dict[mhead(mx)](map(_mxpr2sympy, mx.args)...)
    end
    pyfunc = sympy.Function(string(mhead(mx)))  # Don't recognize the head, so make it a user function
    mxargs = margs(mx)
    if length(mxargs) == 0
        return pyfunc(dummy_arg)
    else
        return pyfunc(map(_mxpr2sympy, mxargs)...)
    end
end

function _mxpr2sympy(mx::Symbol)
    if haskey(mx_to_py_dict,mx)
        return mx_to_py_dict[mx]
    end
    return sympy.Symbol(mx)
end

function _mxpr2sympy(mx::SSJSym)
    name = symname(mx)
    if haskey(mx_to_py_dict,name)
        return conv_rev[name]
    end
    return sympy.Symbol(name)
end

_mxpr2sympy{T<:Integer}(mx::Rational{T}) = sympy.Rational(num(mx),den(mx))

_mxpr2sympy{T<:Number}(mx::T) = mx

# For our LaplaceTransform code, (etc.)
_mxpr2sympy{T}(a::Array{T,1}) =  map(_mxpr2sympy, a)

_mxpr2sympy{T <: PyCall.PyObject}(expr::T) = expr


function _mxpr2sympy(x::AbstractString)
    x
end

# Don't error, but only warn. Then return x so that we
# can capture and inspect it.
function _mxpr2sympy(x)
    warn("mxpr2sympy: Unable to convert $x from SJulia to SymPy")
    return x
end

# We call init_sympy() from __init__
function init_sympy()
    import_sympy()
    eval(parse("const dummy_arg = sympy.Symbol(\"DUMMY\")"))
    eval(parse("const SymPyMinusInfinity = sympy.Mul(-1 , sympy.oo)"))
    make_sympy_to_sjulia()
    populate_py_to_mx_dict()
    mk_py_to_mx_funcs()
    populate_special_symbol_dict()
    populate_mx_to_py_dict()
    mk_mx_to_py_funcs()
end

#####

name{T <: PyCall.PyObject}(x::T) = pytypeof(x)[:__name__]

# Convert Mxpr to sympy, pulling out Rule(a,b) to dict of keyword args.
function mxpr2sympy_kw{T<:Mxpr}(mx::T, kws)
    args = margs(mx)
    nargs = newargs()
    for i in 1:length(args)
        if is_Mxpr(args[i], :Rule)
            kws[args[i][1]] = args[i][2]
        else
            push!(nargs, mxpr2sympy(args[i]))
        end
    end
    nargs
end

function mxpr2sympy_kw{T<:Mxpr}(mx::T)
    kws = Dict()  # type ? probably symbols
    nargs  = mxpr2sympy_kw(mx, kws)
    return (nargs, kws)
end

# Separate the Rule()'s and other arguments in an Mxpr expression
# Store keywords in a Dict so they can by passed as keword arguments.
# These do the same as above, but no conversion to sympy.
function separate_rules{T<:Mxpr}(mx::T, kws)
    args = margs(mx)
    nargs = newargs()
    for i in 1:length(args)
        if is_Mxpr(args[i], :Rule)
            length(args[i]) != 2 && error("Rule requires two arguments. " * length(args[i]) * " found.")
            kws[args[i][1]] = args[i][2]
        else
            push!(nargs, args[i])
        end
    end
    nargs
end

function separate_rules{T<:Mxpr}(mx::T)
    kws = Dict()  # type ? probably symbols
    nargs  = separate_rules(mx, kws)
    return (nargs, kws)
end

# Try the sympy function 'pycall'. If there is an error,
# give warning 'errstr' and return (from surrounding function body) 'return_val_err'
# Store the error message in the SJulia variable SymPyErr.
# On success, return the result of the function call.
macro try_sympyfunc(pycall, errstr, return_val_err)
    npycall = parse( "sympy." * string(pycall))
    return :(
             begin
             (sflag, _pyres) =
                 try
                   res = $npycall
                   (true, res)
                 catch pyerr
                  (false,pyerr)
                 end
                 if sflag == false
                   warn($errstr)
                   setsymval(:SymPyErr, _pyres)
                   return $return_val_err
                 end
                 _pyres
              end
           )
end

set_pattributes("SymPyErr", :Protected)

@sjdoc SymPyErr "
SymPyErr contains the most recent sympy error message. If you see a message warning that
a SymPy error has occurred, you can find the detailed error message in SymPyErr.
"
