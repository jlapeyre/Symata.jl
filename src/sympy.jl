using PyCall

const sympy  = PyCall.PyNULL()
const mpmath = PyCall.PyNULL()

import Base: isless

# We no longer have a module here.
# export pytosj, sjtopy
# export sympy

#importall Symata
#import Symata: mxpr, mxprcf  # we need this even with importall

# Notes for evenutally extracting mpz and mpf from python mpmath objects
#  julia> ex[:_mpf_][2]
#  PyObject mpz(5400274185508743)
#  julia> ex
#  PyObject mpf('0.29977543700203352')

# Francesco Bonazzi contributed code for an early version of this file.

# FIXME: We may want to SymPy integers to BigInts in Symata when bigint_input is set.
# SymPy integers are already BigInt, but we are currently converting them to Int (I think)
# Eg.
# symata> BigIntInput(True)
# syamta> Head(Cos(0))
#     Int64

function import_sympy()
    copy!(sympy, PyCall.pyimport_conda("sympy", "sympy"))
    copy!(mpmath, PyCall.pyimport_conda("mpmath", "mpmath"))    
end

const PYDEBUGLEVEL = -1

macro pydebug(level, a...)
    if level <= PYDEBUGLEVEL
        :((println("pydeb: ", $(a...));println()))
    else
        nothing
    end
end

const SJDEBUGLEVEL = -1

macro sjdebug(level, a...)
    if level <= SJDEBUGLEVEL
        :((symprintln("sjdeb: ", $(a...));println()))
    else
        nothing
    end
end


######################################################
#   Translation Dicts

# SYMPY_TO_SYMATA_FUNCTIONS
# 1) keys are sympy function names. values are the Symata Head
# The sympy functions are found via sympy.key and are stored
# in the dict py_to_mx_dict, with values being the Symata heads.
# Some other special cases,
# eg Add Mul are also stored in py_to_mx_dict.
# The Symata heads are looked up only at the beginning
# of the main _pytosj routine.
# An Mxpr is created by looking up the head and mapping
# _pytosj over the args.
#
# 2) A reversed dict SYMATA_TO_SYMPY_FUNCTIONS is created from SYMPY_TO_SYMATA_FUNCTIONS.
# This dict is used for two things a) to automatically lookup sympy docstrings associated
# with Symata heads in doc.jl. b) to populated mx_to_py_dict. This dict stores callable references
# to sympy functions with keys being the Symata heads. A few other sympy functions are put into
# mx_to_py_dict "by hand" in populate_mx_to_py_dict().(does it need to be done this way ?)

const SYMPY_TO_SYMATA_FUNCTIONS = Dict{Symbol,Symbol}()
const SYMATA_TO_SYMPY_FUNCTIONS = Dict{Symbol,Symbol}()

# Dict containing symbols created via sympy.Symbol("...")
const SYMPY_USER_SYMBOLS = Dict{Symbol,Any}()

function set_pytosj(py,sj)
    spy = Symbol(py)
    ssj = Symbol(sj)
    if haskey(SYMPY_TO_SYMATA_FUNCTIONS,spy)
        warn("*** set_pytosj ", spy, " already has value ", SYMPY_TO_SYMATA_FUNCTIONS[spy], " can't set it to ", ssj)
        return
    end
    SYMPY_TO_SYMATA_FUNCTIONS[spy] = ssj
end

get_pytosj(py) = SYMPY_TO_SYMATA_FUNCTIONS[py]

function set_sjtopy(sj,py)
    spy = Symbol(py)
    ssj = Symbol(sj)
    if haskey(SYMATA_TO_SYMPY_FUNCTIONS,ssj)
        symwarn("!!! set_sjtopy ", sj, " already has value ", SYMATA_TO_SYMPY_FUNCTIONS[ssj], " can't set it to ", py)
        return
    end
    SYMATA_TO_SYMPY_FUNCTIONS[ssj] = spy
end

get_sjtopy(sj) = SYMATA_TO_SYMPY_FUNCTIONS[sj]

#################################################################

# described above
const mx_to_py_dict =  Dict()   # Can we specify types for these Dicts ?

# pymx_special_symbol_dict
# If sympy returns an instance of Pi, Ep1, ImaginaryUnit, etc. it
# is found in this table. This is currently neccesary. Without it:
# * Pi causes an error exception because the default translation tries to call it.
#    i.e. it is not a symbol, function, number ...
# * E is returned as the function Exp1() it is caught by the
#   is_Function check, and returns Exp1()
# * I is caught by the default at the end, is callable and returns
#   ImaginaryUnit(). It is not a symbol nor is_Function.
const pymx_special_symbol_dict = Dict()

# py_to_mx_symbol_dict
# These are perhaps sympy 'symbols' rather than functions or instance.
# Use this Dict to rely more on these lines:
# head = sympy_to_mxpr_symbol(expr[:func][:__name__])
#    return Symata.mxpr(head, map(pytosj, expr[:args])...)
# Gamma( a < b) returns the function gamma,
# with one argument Comparison(a, > , b), which is caught
# by the is_Function check. a, <, and b are caught by the
# Symbol check. However, the symbols in this table do occur
# sometimes.

# NOTE: The test suite passes with this dict empty
# So, we leave it empty until we see something we don't like.
const py_to_mx_symbol_dict = Dict()

# const py_to_mx_symbol_dict = Dict(
#                                   :StrictLessThan => :<,
#                                   :StrictGreaterThan => :>,
#                                   :LessThan => :<=,
#                                   :GreaterThan => :>=,
#                                   :uppergamma => :Gamma,
#                                   :Equality => :(==),
#                                   :Unequality => :(!=)
#                                   )


# This does not refer the sympy 'rewrite' capability.
# This refers to some kind of rewriting of functions or arguments
# during sympy to symata translation.
# This dict is populated below.
const py_to_mx_rewrite_function_dict = Dict(
                                            )
# End translation Dicts
######################################################

#### Digits of precision for Sympy

const MPMATH_DPS = Int[0]

get_mpmath_dps() = mpmath[:mp][:dps]

function set_mpmath_dps(n)
    push!(MPMATH_DPS,mpmath[:mp][:dps])
    mpmath[:mp][:dps] = n
end

restore_mpmath_dps() = (mpmath[:mp][:dps] = pop!(MPMATH_DPS))

# This translates key value pairs that we use to
# describe the mapping from sj to py.
function get_sympy_math(x)
    if length(x) == 1
        jf = x[1]
        st = string(jf)
        sjf = ucfirst(st)
    elseif length(x) == 2
        (jf,sjf) = x
    else
        error("get_sympy_math: wrong number of args in $x")
    end
    return jf,sjf
end

# These are used for rewriting in both directions and for calling functions
# via sympy.symbol
function make_sympy_to_symata()
    symbolic_misc = [ (:Order, :Order), (:LaplaceTransform, :laplace_transform),
                      ( :InverseLaplaceTransform, :inverse_laplace_transform ),
                      (:InverseFourierTransform, :inverse_fourier_transform ),
                      (:FourierTransform, :fourier_transform),
                      (:Log, :log), ( :Sqrt, :sqrt), (:ProductLog, :LambertW),
                      (:Exp, :exp), (:Abs, :Abs), (:MeijerG, :meijerg), (:PolarLift, :polar_lift),
                      (:ExpPolar, :exp_polar), (:LowerGamma, :lowergamma), (:Sign,:sign),
                      (:PeriodicArgument, :periodic_argument), (:Max, :Max), (:Min, :Min)
                      ]

    for funclist in (single_arg_float_complex, single_arg_float_int_complex, single_arg_float,
                     single_arg_float_int, single_arg_int, two_arg_int,
                     two_arg_float_and_float_or_complex, two_arg_float,
                     one_or_two_args1
                     )
        for x in funclist
            if length(x) != 3 continue end
            (julia_func, symata_func, sympy_func) = x
            set_pytosj(sympy_func, symata_func)
            set_sjtopy(symata_func, sympy_func)
        end
    end

    for funclist in (symbolic_misc, no_julia_function, no_julia_function_one_arg,
                     no_julia_function_one_or_two_int,
                     no_julia_function_two_args, no_julia_function_two_or_three_args,
                     no_julia_function_three_args, no_julia_function_four_args)
        for x in funclist
            symata_func, sympy_func = get_sympy_math(x)
            set_pytosj(sympy_func, symata_func)
            set_sjtopy(symata_func, sympy_func)
        end
    end
    set_pytosj(:InverseLaplaceTransform,:InverseLaplaceTransform)
end

function register_sjfunc_pyfunc{T<:Union{AbstractString,Symbol}, V<:Union{AbstractString,Symbol}}(sj::T, py::V)
    set_pytosj(Symbol(py), Symbol(sj))
    set_sjtopy(Symbol(sj), Symbol(py))
end

# Watch the order
function register_only_pyfunc_to_sjfunc{T<:Union{AbstractString,Symbol}, V<:Union{AbstractString,Symbol}}(sj::T, py::V)
    set_pytosj(py,sj)
end

## These two functions are only used in doc.jl to look up the
# docstring corresponding to the symp function called by an Symata head.
function have_pyfunc_symbol(sjsym)
    haskey(SYMATA_TO_SYMPY_FUNCTIONS, sjsym)
end
function lookup_pyfunc_symbol(sjsym)
    get_sjtopy(sjsym)
end

#### Convert SymPy to Mxpr

#  Note: To convert, say a hypergeometric function to a float, do this:  f1[:evalf]()

function populate_py_to_mx_dict()
    # If this is Dict{Any,Any}, then nothing is translated until the
    # catchall branch at the end of sympyt2mxpr. Why ?
    eval(parse("const py_to_mx_dict = Dict{PyCall.PyObject,Symbol}()"))
    for onepair in (
                    (sympy[:Add], :Plus),
                    (sympy[:Mul], :Times),
                    (sympy[:Pow] ,:Power),
                    (sympy[:Derivative], :D),
                    (sympy[:integrals]["Integral"], :Integrate),
                    (sympy[:containers][:Tuple], :List),  # Problem, a List may be huge, Tuple not... but this is a sympy Tuple
                    (sympy[:oo], :Infinity),
                    (sympy[:zoo],:ComplexInfinity))
        py_to_mx_dict[onepair[1]] = onepair[2]
        if haskey(sympy[:functions][:special][:hyper], :TupleArg)  # This is missing in older versions of SymPy. (But so are many other symbols)
            py_to_mx_dict[sympy[:functions][:special][:hyper][:TupleArg]] = :List
        end
    end
end

# These functions are also contained in sympy.C (but sympy.C has been deprecated)
function mk_py_to_mx_funcs()
    for (pysym,sjsym) in SYMPY_TO_SYMATA_FUNCTIONS
        pystr = string(pysym)
        sjstr = string(sjsym)
        if haskey(sympy[:functions], pystr)
            py_to_mx_dict[sympy[:functions][pystr]] =  Symbol(sjstr)
        end
    end
end

function rewrite_function_sympy_to_julia(expr)
    func = py_to_mx_rewrite_function_dict[name(expr)]
    func(expr)
end


have_function_sympy_to_symata_translation{T <: PyCall.PyObject}(expr::T) = haskey(py_to_mx_dict, pytypeof(expr))
get_function_sympy_to_symata_translation{T <: PyCall.PyObject}(expr::T) = py_to_mx_dict[pytypeof(expr)]
have_rewrite_function_sympy_to_julia{T <: PyCall.PyObject}(expr::T) = haskey(py_to_mx_rewrite_function_dict, name(expr))

# May 2016. Added ComplexInfinity here. We added an mxpr method for
# using Mxpr for head. sympy.zoo had been caught by the default method
# for converting, and this resulted in an Mxpr for the head.
function populate_special_symbol_dict()
    for onepair in (
                    (sympy[:numbers]["ComplexInfinity"], :ComplexInfinity),
                    (sympy[:numbers]["Pi"], :Pi),
                    (sympy[:numbers]["EulerGamma"], :EulerGamma),
                    (sympy[:numbers]["Exp1"],  :E),
                    (sympy[:numbers]["ImaginaryUnit"], complex(0,1)),
                    (sympy[:numbers]["NegativeInfinity"], MinusInfinity))
        pymx_special_symbol_dict[onepair[1]] = onepair[2]
    end
end

sympy_to_mxpr_symbol(s::Symbol) = haskey(py_to_mx_symbol_dict, s) ? py_to_mx_symbol_dict[s] : s
sympy_to_mxpr_symbol{T<:AbstractString}(s::T) = sympy_to_mxpr_symbol(Symbol(s))

function pytosj{T}(x::T)
    getkerneloptions(:return_sympy) && return x  # The user can disable conversion. Eg. for debugging.
    __pytosj(x)
end

type SympyTrace
    trace::Bool
end
const SYMPYTRACE = SympyTrace(false)
const SYMPYTRACENUM = Int[0]
get_pytosj_count() = SYMPYTRACENUM[1]
increment_pytosj_count() = SYMPYTRACENUM[1] += 1
decrement_pytosj_count() = SYMPYTRACENUM[1] -= 1
is_pytosj_trace() = SYMPYTRACE.trace

## Note there are two underscores __pytosj
function __pytosj(expr)
    increment_pytosj_count()
    if is_pytosj_trace()
        ind = " " ^ (get_pytosj_count() - 1)
        println(ind,">>", get_pytosj_count(), " " , expr)
    end
    res = _pytosj(expr)
    if is_pytosj_trace()
        ind = " " ^ (get_pytosj_count() - 1)
        println(ind,"<<", get_pytosj_count(), " " , res)
    end
    decrement_pytosj_count()
    mergeargs(res)  # this may not be enough. only looks at one level, I think
    res
end

_pytosj(x) = x

function pytosj_Function(pyexpr)
    head = Symbol(name(pyexpr))
    targs = pyexpr[:args]
    if  head == :_context  return Qsym(pytosj(targs[1]),pytosj(targs[2])) end
    if targs[1] == dummy_arg  # sympy does not allow functions without args, so we pass a dummy arg.
        return mxprcf(head, [])
    else
        return mxpr(head, map(pytosj, targs)...)
    end
end

macro pytosj_comparisons(fname, pyfname, sjsymbolstr)
    sfname = Symbol("pytosj_" * fname)
    sjsymbol = parse(":(" * sjsymbolstr * ")")  # careful we don't insert an unquoted symbol
    esc(quote
        function ($sfname)(pyexpr)
          args = pyexpr[:args]
          return mxpr(:Comparison, _pytosj(args[1]), $sjsymbol, _pytosj(args[2]))
        end
        py_to_mx_rewrite_function_dict[$pyfname] = $sfname
       end)
end

@pytosj_comparisons("greater_than_equal", "GreaterThan", ">=")
@pytosj_comparisons("less_than_equal", "LessThan", "<=")
@pytosj_comparisons("less_than", "StrictLessThan", "<")
@pytosj_comparisons("greater_than", "StrictGreaterThan", ">")
@pytosj_comparisons("equality", "Equality", "==")
@pytosj_comparisons("unequality", "Unequality", "!=")


pytosj_BooleanTrue(pyexpr) = true

# Needed for: Integrate(Exp(-x^2),  [x,0,Infinity]) == (1/2)*(Pi^(1/2))
py_to_mx_rewrite_function_dict["BooleanTrue"] = pytosj_BooleanTrue

####
####   Main _pytosj method
####
function _pytosj{T <: PyCall.PyObject}(expr::T)
    @pydebug(3, "Entering with ", expr)
    if have_function_sympy_to_symata_translation(expr)
        @pydebug(3, "function lookup trans. ", expr)
        return mxpr(get_function_sympy_to_symata_translation(expr), map(pytosj, expr[:args])...)
    end
    if have_rewrite_function_sympy_to_julia(expr)
        @pydebug(3, "rewrite trans. ", expr)
        return rewrite_function_sympy_to_julia(expr)
    end
    if expr[:is_Function]
        @pydebug(3, "is_Function trans. ", expr)
        return pytosj_Function(expr)
    end   # perhaps a user defined function
    for k in keys(pymx_special_symbol_dict)
        if pyisinstance(expr,k)
            @pydebug(3, "special_symbol trans. ", expr)
            return pymx_special_symbol_dict[k]
        end
    end
    if pytypeof(expr) == sympy[:Symbol]
        @pydebug(3, "pytype Symbol trans. ", expr)
        return sympy_to_mxpr_symbol(expr[:name])
    end
    if pyisinstance(expr, sympy[:Number])
        @pydebug(3, "number trans. ", expr)
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
        return convert(AbstractFloat, expr) # Need to check for BigFloat
    end
    head = sympy_to_mxpr_symbol(expr[:func][:__name__])  # default
    @pydebug(3, "default trans. ", expr, " new head ", head)
    @pydebug(3, "typeof head ", typeof(head))
    @pydebug(3, "args  ", expr[:args])
    mxout =  mxpr(head, map(pytosj, expr[:args])...)
    @pydebug(3, "mxout ", mxout)
    mxout
end

# By default, Dict goes to Dict
function _pytosj(expr::Dict)
    ndict = Dict()
    for (k,v) in expr
        ndict[pytosj(k)] = pytosj(v)
    end
    return ndict
end

function _pytosj{T}(expr::Array{T,1})
    return mxpr(:List,map(pytosj, expr)...)
end

function _pytosj(expr::Tuple)
    return mxpr(:List,map(pytosj, expr)...)
end

#### Convert Mxpr to SymPy

# We use instances here and classes to go the other direction.
# This is because the name cannot be obtained from an instance.
function populate_mx_to_py_dict()
    for onepair in (
         (:Plus,sympy[:Add]),
         (:Times, sympy[:Mul]),
         (:Power, sympy[:Pow]),
         (:E, sympy[:E]),
         (:EulerGamma, sympy[:EulerGamma]),
         (:I, sympy[:I]),
         (:Pi,  sympy[:pi]),
         (:Log, sympy[:log]),
         (:Infinity, sympy[:oo]),
         (:ComplexInfinity, sympy[:zoo]))
        mx_to_py_dict[onepair[1]] = onepair[2]
    end
end

function mk_mx_to_py_funcs()
    for (sjsym,pysym) in SYMATA_TO_SYMPY_FUNCTIONS
        pystr = string(pysym)
        sjstr = string(sjsym)
        local obj
        if length(pystr) > 6 && pystr[1:6] == "sympy_"  # These are wrapper function around sympy functions
            try
                obj = eval(pysym)
            catch
                continue
            end
        else
            try
                obj = eval(parse("sympy[:" * pystr * "]"))   # These call the sympy functions directly
            catch
                continue
            end
        end
        mx_to_py_dict[Symbol(sjstr)] = obj
    end
end

#######################################################
##     sjtopy
#######################################################

# NB wrapper to call _sjtopy
function sjtopy(args...)
    if length(args) == 1
        res = _sjtopy(args[1])
        return res
    end
    res = _sjtopy(args)
    (res...)
end

function _sjtopy(z::Complex)
    @sjdebug(3,"complex ", z)
    if real(z) == 0
        res = mxpr(:Times, :I, imag(z))
    else
        res = mxpr(:Plus, real(z), mxpr(:Times, :I, imag(z)))
    end
    return _sjtopy(res)
end

function _sjtopy(mx::Mxpr{:List})
    @sjdebug(3,"List ", mx)
    a = Array(Any,0)
    rs = map(_sjtopy, mx.args)
    for el in rs
        push!(a,el)
    end
    return a
#    return [map(_sjtopy, mx.args)...]  This fails for some versions of Julia
end

# For now all infinities are mapped to one of two infinities
function _sjtopy(mx::Mxpr{:DirectedInfinity})
    @sjdebug(3,"Infinity ", mx)
    if mx == ComplexInfinity
        sympy[:zoo]
    elseif mx == MinusInfinity
        SymPyMinusInfinity
    elseif mx[1] == I
        sympy[:Mul](sympy[:I],sympy[:oo])
    else
        sympy[:oo]
    end
end

#### Rewrite

const RewriteDict = Dict( :Gamma => :gamma,
                          :HypergeometricPFQ => :hyper,
                          :Binomial => :binomial,
                          :Sin => :sin)

@mkapprule Rewrite :nargs => 2

@sjdoc Rewrite "
Rewrites(expr,form) rewrites expr in term of form. This is
implemented for some functions that call SymPy. The second argument is
translated, but you may give the SymPy symbol, as well

Examples

Rewrite(CatalanNumber(n), Gamma)
Rewrite(CatalanNumber(1/2), Gamma)
Rewrite(CatalanNumber(n), HypergeometricPFQ)
"

@doap function Rewrite(expr, form::Symbol)
    arg1 = _sjtopy(mx[1])
    transform = get(RewriteDict, form, form)
    pyres = arg1[:rewrite](transform)
    pytosj(pyres)
end

#### HypergeometricPFQ

do_HypergeometricPFQ{W<:AbstractFloat}(mx::Mxpr{:HypergeometricPFQ}, p::Mxpr{:List}, q::Mxpr{:List}, z::W) =
    eval_hypergeometric(mx,p,q,z)

do_HypergeometricPFQ{W<:AbstractFloat}(mx::Mxpr{:HypergeometricPFQ}, p::Mxpr{:List}, q::Mxpr{:List}, z::Complex{W}) =
    eval_hypergeometric(mx,p,q,z)

function eval_hypergeometric(mx, p, q, z)
    result = sjtopy(mx)
    fresult =
        try
            result[:evalf]()
        catch
            result
        end
    pytosj(fresult)
end

#### MeijerG

@mkapprule MeijerG

function do_MeijerG(mx::Mxpr{:MeijerG}, p::Mxpr{:List}, q::Mxpr{:List}, z)
    mxc = copy(mx)
    mxc[1] = (p[1],p[2])
    mxc[2] = (q[1],q[2])
    try
        zpy = sjtopy(z)
        ppy = (_sjtopy(p[1]), _sjtopy(p[2]))
        qpy = (_sjtopy(q[1]), _sjtopy(q[2]))
        pyres = sympy[:meijerg](ppy,qpy,zpy)
        sjres = pytosj(pyres)
    catch
        mx
    end
end

function _sjtopy(mx::Mxpr{:MeijerG})
    @sjdebug(3,"MeijerG ", mx)
    pyhead = mx_to_py_dict[mhead(mx)]
    p = mx[1]
    q = mx[2]
    z = mx[3]
    pyhead((_sjtopy(p[1]), _sjtopy(p[2])), (_sjtopy(q[1]), _sjtopy(q[2])), _sjtopy(z))
end

do_MeijerG{W<:AbstractFloat}(mx::Mxpr{:MeijerG}, p::Mxpr{:List}, q::Mxpr{:List}, z::W) =
    eval_meijerg(mx,p,q,z)

do_MeijerG{W<:AbstractFloat}(mx::Mxpr{:MeijerG}, p::Mxpr{:List}, q::Mxpr{:List}, z::Complex{W}) =
    eval_meijerg(mx,p,q,z)

function eval_meijerg(mx, p, q, z)
    mxc = copy(mx)
    mxc[1] = (p[1],p[2])
    mxc[2] = (q[1],q[2])
    result = sjtopy(mxc)
    fresult =
        try
            result[:evalf]()
        catch
            result
        end
    pytosj(fresult)
end

#### Tuple

function _sjtopy(t::Tuple)
    @sjdebug(3,"Tuple ", mx)
    (map(_sjtopy,t)...)
end


## Sympy functions are called here.
function _sjtopy(mx::Mxpr)
    @sjdebug(3,"Mxpr ", mx)
    if mhead(mx) in keys(mx_to_py_dict)
        @sjdebug(1,"In mx_to_py_dict ", mx, " pyhead ", mx_to_py_dict[mhead(mx)] )
        return mx_to_py_dict[mhead(mx)](map(_sjtopy, mx.args)...) # calling a function in our dictionary
    end
    @sjdebug(1,"Make function ", mx)
    pyfunc = sympy[:Function](string(mhead(mx)))  # Don't recognize the head, so make it a user function
    mxargs = margs(mx)
    if length(mxargs) == 0
        return pyfunc(dummy_arg)  # sympy functions must have at least one argument
    else
        return pyfunc(map(_sjtopy, mxargs)...)
    end
end

#### Symbol

# mx is a julia Symbol.
# We create a sympy symbol, unless it has already been created
function _sjtopy(mx::Symbol)
    @sjdebug(3,"Symbol ", mx)
    if haskey(mx_to_py_dict,mx)
        @sjdebug(1,"In mx_to_py_dixt ", mx)
        return mx_to_py_dict[mx]
    end
    if haskey(SYMPY_USER_SYMBOLS,mx)
        sym = SYMPY_USER_SYMBOLS[mx]
        return sym
    else
        sym = sympy[:Symbol](mx)
        SYMPY_USER_SYMBOLS[mx] = sym
        return sym
    end
end

#### Assume

@mkapprule Assume

@sjdoc Assume "
Assume(sym,prop1,prop2,...) sets properties prop1, prop2,... for symbols sym.
These properties are used in simplifying relations, `Refine`, integral transforms, etc.

Properties are: commutative, complex, imaginary, real, integer, odd, even,
prime, composite, zero, nonzero, rational, algebraic, transcendental, irrational, finite, infinite, negative, nonnegative, positive, nonpositive, hermitian, antihermitian.

These property symbols may contain uppercase characters. For example `Positive` and `positive` are the same. Use of
properties with lowercase initial is deprecated.
"

# TODO. We implemented this to use, e.g. the inequality solvers
# Can't find a better way to create the symbol
# prop is not evaluated, rather the symbol 'prop' is set to true
# This uses the 'old' SymPy assumptions system
@doap function Assume(s::Symbol, inprops...)
    ss = string(s)
    props = [ Symbol(lowercase(string(x))) for x in inprops]
    propstrs = map( (x) -> " $x=true ", props)
    propstr = join(propstrs, ", ")
    evalstr = "sympy[:Symbol](\"$ss\", $propstr)"
    sym = eval(parse(evalstr))
    SYMPY_USER_SYMBOLS[s] = sym
    s
end

#### Max

# TODO Max(x,y) == Max(y,x)
# TODO Do numerical arguments in Julia
# TODO Flatten lists
@mkapprule Max :nodefault => true
@doap function Max(args...)
    args = margs(flatten_recursive!(mxpr(:List,args...)))
    return pytosj(sympy[:Max](map(sjtopy, args)...))
end

@doap Max() = MinusInfinity

#### Min

@mkapprule Min :nodefault => true
@doap function Min(args...)
    args = margs(flatten_recursive!(mxpr(:List,args...)))
    return pytosj(sympy[:Min](map(sjtopy, args)...))
end

@doap Min() = Infinity

_sjtopy{T<:Integer}(mx::Rational{T}) = sympy[:Rational](num(mx),den(mx))

#_sjtopy{T<:Number}(mx::T) = mx
_sjtopy(mx::Number) = mx

function _sjtopy{T<:Qsym}(s::T)
    f = sympy[:Function]("_context")
    f(_sjtopy(s.context),_sjtopy(s.name))
end

# For our LaplaceTransform code, (etc.)
_sjtopy{T}(a::Array{T,1}) =  map(_sjtopy, a)

_sjtopy{T <: PyCall.PyObject}(expr::T) = expr


_sjtopy(x::AbstractString) =  x

# Default conversion. Don't error, but only warn. Then return x so that we
# can capture and inspect it.
function _sjtopy(x)
    symwarn("sjtopy: Unable to convert $x from Symata to SymPy")
    return x
end

"""
    init_sympy()

Initialize SymPy. SymPy must be loaded at runtime. `init_sympy()` is called
in the Symata module ` __init__` function.
"""
function init_sympy()
    import_sympy()
    eval(parse("const dummy_arg = sympy[:Symbol](\"DUMMY\")"))
    eval(parse("const SymPyMinusInfinity = sympy[:Mul](-1 , sympy[:oo])"))
    make_sympy_to_symata()
    populate_py_to_mx_dict()
    mk_py_to_mx_funcs()
    populate_special_symbol_dict()
    populate_mx_to_py_dict()
    mk_mx_to_py_funcs()
end

#####

name{T <: PyCall.PyObject}(x::T) = pytypeof(x)[:__name__]

#  Try the sympy function 'pycall'. If there is an error, give warning
#  'errstr' and return (from surrounding function body)
#  'return_val_err' Store the error message in the kernel state.
#  On success, return the result of the function call.
macro try_sympyfunc(pycall, errstr, return_val_err)
    qpycall = QuoteNode(pycall)
    return :(
             begin
             (sflag, _pyres) =
                 try
                   (true, $pycall)
                 catch pyerr
                  (false,pyerr)
                 end
                 if sflag == false
                 warn($errstr)
                   setkerneloptions(:sympy_error, _pyres)
                   return $return_val_err
                 end
                 _pyres
              end
           )
end

#### PyDoc

@mkapprule PyDoc :nargs => 1

function do_PyDoc(mx::Mxpr{:PyDoc},sym)
    try eval(parse("println(sympy[:$(string(sym))][:__doc__])"))
    catch
        Null
    end
end

@sjdoc PyDoc "
PyDoc(sym) prints the documentation for the symbol sym, if available.
"
