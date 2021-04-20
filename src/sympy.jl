using PyCall

## Translate Symata to SymPy and vice versa.
##
## Symata -> SymPy is in general much faster than the reverse, 10x or more

## We don't need to define all of these.
const sympysyms1 = [:Symbol,:Number,:Integral, :Sum, :StrictLessThan, :StrictGreaterThan,
                    :LessThan, :GreaterThan, :Unequality,
                    :Integer, :Rational, :Float]

## These are buried in arbitrary class hierarchies
const sympysyms2 = [:True,:False]

const sympy  = PyCall.PyNULL()
const mpmath = PyCall.PyNULL()

for s in (sympysyms1..., sympysyms2...)
    @eval const $(Symbol("sympy_", s)) = PyCall.PyNULL()
end

import Base: isless

const PyObject = PyCall.PyObject

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

# PYDEBUGLEVEL and SJDEBUGLEVEL defined in debug.jl
macro pydebug(level, a...)
    if level <= PYDEBUGLEVEL
        :((println("pydeb: ", $(a...));println()))
    else
        nothing
    end
end

macro sjdebug(level, a...)
    if level <= SJDEBUGLEVEL
        :((symprintln("sjdeb: ", $(a...));println()))
    else
        nothing
    end
end


######################################################
##   Translation Dicts
##   Following is somewhat out of data.

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
# with Symata heads in doc.jl. b) to populate mx_to_py_dict. This dict stores callable references
# to sympy functions with keys being the Symata heads. A few other sympy functions are put into
# mx_to_py_dict "by hand" in populate_mx_to_py_dict().(does it need to be done this way ?)

const SYMPY_TO_SYMATA_FUNCTIONS = Dict{Symbol,Symbol}()
const SYMATA_TO_SYMPY_FUNCTIONS = Dict{Symbol,Symbol}()

# Dict containing symbols created via sympy.Symbol("...")
# This maps Sympy symbols to Symata (Julia) Symbols
const SYMPY_USER_SYMBOLS = Dict{Symbol,Any}()

# This maps in the other direction, Julia to Sympy symbols.
# But, we put more than user symbols in the following
const SYMPY_USER_SYMBOLS_REVERSE = Dict{PyCall.PyObject,Any}()  # type must be Any, we want more than symbols

function set_pytosj(py, sj)
    spy = Symbol(py)
    ssj = Symbol(sj)
    if haskey(SYMPY_TO_SYMATA_FUNCTIONS, spy)
        @warn("*** set_pytosj ", spy, " already has value ", SYMPY_TO_SYMATA_FUNCTIONS[spy], " can't set it to ", ssj)
        return
    end
    SYMPY_TO_SYMATA_FUNCTIONS[spy] = ssj
end

function set_sjtopy(sj,py)
    spy = Symbol(py)
    ssj = Symbol(sj)
    if haskey(SYMATA_TO_SYMPY_FUNCTIONS,ssj)
        @symwarn("!!! set_sjtopy ", sj, " already has value ", SYMATA_TO_SYMPY_FUNCTIONS[ssj], " can't set it to ", py)
        return
    end
    SYMATA_TO_SYMPY_FUNCTIONS[ssj] = spy
end

get_sjtopy(sj) = SYMATA_TO_SYMPY_FUNCTIONS[sj]

#######################################################

# described above
const mx_to_py_dict = Dict()   # Can we specify types for these Dicts ?

# This does not refer the sympy 'rewrite' capability.
# This refers to some kind of rewriting of functions or arguments
# during sympy to symata translation.
# This dict is populated below.
const py_to_mx_rewrite_function_dict = Dict{Any,Any}()

# End translation Dicts
######################################################

## Digits of precision for Sympy
const MPMATH_DPS = Int[0]

"""
    get_mpmath_dps()

Get the number of digits of precision used by mpmath.
"""
get_mpmath_dps() = mpmath.mp.dps

"""
    set_mpmath_dps(n)

Set the number of digits of precision used by mpmath.
"""
function set_mpmath_dps(n)
    push!(MPMATH_DPS, mpmath.mp.dps)
    mpmath.mp.dps = n
end

"""
    restore_mpmath_dps()

Pop the most recent number of digits of precision used by mpmath
from a stack, and set the number of DPS.
"""
restore_mpmath_dps() = (mpmath.mp.dps = pop!(MPMATH_DPS))

# This translates key value pairs that we use to describe the mapping from sj to py.
function get_sympy_math(x)
    if length(x) == 1
        jf = x[1]
        st = string(jf)
        sjf = uppercasefirst(st)
    elseif length(x) == 2
        (jf,sjf) = x
    else
        error("get_sympy_math: wrong number of args in $x")
    end
    return jf,sjf
end

# These are used for rewriting in both directions and for calling functions via sympy.symbol
# Functions whose rules are written by hand, ie. that are not handled by
# the lists single_arg_float_complex, etc. must be entered here.
function make_sympy_to_symata()
    symbolic_misc = [ (:Order, :Order), (:LaplaceTransform, :laplace_transform),
                      ( :InverseLaplaceTransform, :inverse_laplace_transform ),
                      (:InverseFourierTransform, :inverse_fourier_transform ),
                      (:FourierTransform, :fourier_transform),
                      (:Log, :log), ( :Sqrt, :sqrt),
                      (:ProductLog, :LambertW),
                      (:Exp, :exp), (:Abs, :Abs), (:MeijerG, :meijerg), (:PolarLift, :polar_lift),
                      (:ExpPolar, :exp_polar), (:LowerGamma, :lowergamma), (:Sign,:sign),
                      (:PeriodicArgument, :periodic_argument), (:Max, :Max), (:Min, :Min),
                      (:DirichletEta, :dirichlet_eta),(:PolyLog, :polylog),
                      (:Conjugate, :conjugate), (:Factorial, :factorial)
                      ]

##  Cannot put Equality here. It is not a symbol. It is not a function. It is a something else.
## (:Equal,:Equality)

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
    set_pytosj(:InverseLaplaceTransform, :InverseLaplaceTransform) # FIXME. Do this elsewhere ?
end

function register_sjfunc_pyfunc(sj::SymString, py::SymString)
    set_pytosj(Symbol(py), Symbol(sj))
    set_sjtopy(Symbol(sj), Symbol(py))
end

function register_only_pyfunc_to_sjfunc(sj::SymString, py::SymString)
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

### Convert SymPy to Mxpr
#  Note: To convert, say a hypergeometric function to a float, do this:  f1[:evalf]()
function populate_py_to_mx_dict()
    # If this is Dict{Any,Any}, then nothing is translated until the
    # catchall branch at the end of sympyt2mxpr. Why ?
    eval(Meta.parse("const py_to_mx_dict = Dict{PyCall.PyObject,Symbol}()"))
    for onepair in (
                    (sympy.Add, :Plus),
                    (sympy.Mul, :Times),
                    (sympy.Pow ,:Power),
                    (sympy.Derivative, :D),  ## We need to implement or handle Derivative
                    (sympy.Tuple, :List),
                    (sympy.oo, :Infinity),
                    (sympy.zoo,:ComplexInfinity),
                    (sympy.Eq, :Equal),
                    (sympy.functions.elementary.piecewise.ExprCondPair, :ConditionalExpression))
        py_to_mx_dict[onepair[1]] = onepair[2]
        if hasproperty(sympy.functions.special.hyper, :TupleArg)  # This is missing in older versions of SymPy. (But so are many other symbols)
            py_to_mx_dict[sympy.functions.special.hyper.TupleArg] = :List
        end
    end
end

# These functions are also contained in sympy.C (but sympy.C has been deprecated)
function mk_py_to_mx_funcs()
    for (pysym, sjsym) in SYMPY_TO_SYMATA_FUNCTIONS
        pystr = string(pysym)
        sjstr = string(sjsym)
        if hasproperty(sympy.functions, pysym)
            py_to_mx_dict[getproperty(sympy.functions, pysym)] = Symbol(sjstr)
        end
    end
end

function rewrite_function_sympy_to_julia(expr, pytype)
    func = py_to_mx_rewrite_function_dict[pytype]
    return func(expr)
end

have_function_sympy_to_symata_translation(pytype::PyCall.PyObject) = haskey(py_to_mx_dict, pytype)
get_function_sympy_to_symata_translation(pytype::PyCall.PyObject) = py_to_mx_dict[pytype]
have_rewrite_function_sympy_to_julia(pytype::PyCall.PyObject) = haskey(py_to_mx_rewrite_function_dict, pytype)

## We put more than user symbols here
function populate_user_symbol_dict()
    for onepair in (
                    (sympy.S.ComplexInfinity, :ComplexInfinity),
                    (sympy.S.Pi, :Pi),
                    (sympy.S.EulerGamma, :EulerGamma),
                    (sympy.S.Exp1, :E),
                    (sympy.S.ImaginaryUnit, complex(0,1)),
                    (sympy.S.NegativeInfinity, MinusInfinity))
        SYMPY_USER_SYMBOLS_REVERSE[onepair[1]] = onepair[2]
    end
end

sympy_to_mxpr_symbol(s::Symbol) = get(py_to_mx_symbol_dict, s, s)
sympy_to_mxpr_symbol(s::String) = sympy_to_mxpr_symbol(Symbol(s))

function pytosj(x)
    getkerneloptions(:return_sympy) && return x  # The user can disable conversion. Eg. for debugging.
    maybetrace_pytosj(x)
end

mutable struct SympyTrace
    trace::Bool
end
const SYMPYTRACE = SympyTrace(false)
const SYMPYTRACENUM = Int[0]
get_pytosj_count() = SYMPYTRACENUM[1]
increment_pytosj_count() = SYMPYTRACENUM[1] += 1
decrement_pytosj_count() = SYMPYTRACENUM[1] -= 1
is_pytosj_trace() = SYMPYTRACE.trace

## This probably does not work, because we call the inner function _pytosj when doing recursion
##
function maybetrace_pytosj(expr)
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

function pytosj_map(head,args)
    nargs = newargs()
    for a in args
        push!(nargs,pytosj(a))
#        push!(nargs,_pytosj(a))  ## Using underscore here breaks some things. But pytosj_map is only called when _pytosj should be safe.
    end
    mxpra(head,nargs)
end

function pytosj_map2(head,args)
    nargs = newargs()
    for a in args
        push!(nargs,_pytosj(a))  ## Using underscore here
    end
    mxpra(head,nargs)
end

function pytosj_Function(pyexpr,pytype)
    head = Symbol(pytype.__name__)
    targs = pyexpr.args
    if  head == :_context  return Qsym(pytosj(targs[1]),pytosj(targs[2])) end
    if targs[1] == dummy_arg  # sympy does not allow functions without args, so we pass a dummy arg.
        return mxprcf(head, [])
    else
        return pytosj_map2(head, targs)
    end
end


## TODO: we probably want to get rid of all of these.
## We are currently not translating !=, etc. correctly. At the moment it does not trigger any bugs.
##  Do not use Comparison.
##    Sympy        Symata
##    ==           SameQ     '==' is probably just python '=='. I can't convert this to sympy
##   Eq,Equality   Equal     Eq and Equality are aliases. In sympy Eq neither as symbol, nor a function. It is its own class
##                           This makes Eq more like Add and not Cos.
function populate_rewrite_dict()
    py_to_mx_rewrite_function_dict[sympy_True] = x -> true
    py_to_mx_rewrite_function_dict[sympy_False] = x -> false
    py_to_mx_rewrite_function_dict[sympy_Sum] = pyexpr -> deepsetfixed(mxpr(:Sum, mapmargs(pytosj, pyexpr.args)...))
    py_to_mx_rewrite_function_dict[sympy_Integral] = pyexpr -> deepsetfixed(mxpr(:Integrate, mapmargs(pytosj, pyexpr.args)...))

    for (fname,pyftype, sjsymbolstr) in (("greater_than_equal", :sympy_GreaterThan, ">="),
                                         ("less_than_equal", :sympy_LessThan, "<="),
                                         ("less_than", :sympy_StrictLessThan, "<"),
                                         ("greater_than", :sympy_StrictGreaterThan, ">"),
#                                         ("equality", :sympy_Equality, "=="),
                                         ("unequality", :sympy_Unequality, "!="))

        sfname = Symbol("pytosj_" * fname)
        sjsymbol = QuoteNode(Symbol(sjsymbolstr))
        @eval begin
            function ($sfname)(pyexpr)
                args = pyexpr.args
                return mxpr(:Comparison, _pytosj(args[1]), $sjsymbol, _pytosj(args[2]))
            end
            py_to_mx_rewrite_function_dict[$(eval(pyftype))] = $sfname
        end
    end
end

####
####   Main _pytosj method
####
## expr[:__class__] is a bit faster and does fewer allocations
function _pytosj(expr::T) where T <: PyCall.PyObject
    res = get(SYMPY_USER_SYMBOLS_REVERSE, expr,false) ## This is much faster, but may not be significant
    res !== false && return res
    pytype = expr.__class__
    if pytype == sympy_Symbol
        @pydebug(3, "pytype Symbol trans. ", expr)
        return Symbol(expr.name)
    end
    @pydebug(3, "Entering with ", expr)
#    if pytype == sympy_Integer  ## This does not work. 1, 1/2, etc, are in their own classes.
    ## Many things apparently cannot be checked via the 'pytype' or class.
    ## There are paraphyletic groups whose membership is tested with functions (well, class member functions)
    if expr.is_Integer
        #  returns Int or BigInt. It just returns a member of a python object. It would be nice to get this faster.
        # `_to_mpath` in evalf.py checks again expr.is_Integer.
#        num = expr[:_to_mpmath](-1)  #  precision '-1' is ignored for integers.
        num = expr.p  ## This is 5 or so times faster and does the same thing.
        return num
    end
#    if pytype == sympy_Rational
    if expr.is_Rational
        p = expr.p
        q = expr.q
        return Rational(expr.p,expr.q)  # These are Int64's. Don't know when or if they are BigInts
    end
#    if pytype == sympy_Float
    if expr.is_Float
        return convert(AbstractFloat, expr) # Need to check for BigFloat
    end
    if pyisinstance(expr, sympy.Number) && (! expr.is_finite)
            # we should check name. there are several infinities
        return Infinity  # need to see if this is maybe -Infinity
    end
    if have_function_sympy_to_symata_translation(pytype)
        @pydebug(3, "function lookup trans. ", expr)
        return pytosj_map(get_function_sympy_to_symata_translation(pytype), expr.args)
    end

    if expr.is_Function ## User defined functions whose pytpe is the name of the function
        @pydebug(3, "is_Function trans. ", expr)
        return pytosj_Function(expr,pytype)
    end   # perhaps a user defined function
    if have_rewrite_function_sympy_to_julia(pytype)
        @pydebug(3, "rewrite trans. ", expr)
        return rewrite_function_sympy_to_julia(expr,pytype)
    end
    head = Symbol(expr.func.__name__) # this is the default if not handled above
    @pydebug(3, "default trans. ", expr, " new head ", head)
    @pydebug(3, "typeof head ", typeof(head))
    @pydebug(3, "args  ", expr.args)
    mxout =  pytosj_map(head,  expr.args)
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

function _pytosj(expr::Array{T,1}) where T
    return pytosj_map(:List, expr)
end

function _pytosj(expr::Tuple)
    return pytosj_map(:List, expr)
end

#### Convert Mxpr to SymPy

## We use instances here and classes to go the other direction.
## This is because the name cannot be obtained from an instance.
## TODO: I think the above is not true. We can merge this with another dict
function populate_mx_to_py_dict()
    for onepair in (
         (:Plus,sympy.Add),
         (:Times, sympy.Mul),
         (:Power, sympy.Pow),
         (:E, sympy.E),
         (:EulerGamma, sympy.EulerGamma),
         (:I, sympy.I),
         (:Pi,  sympy.pi),
         (:Log, sympy.log),
         (:Infinity, sympy.oo),
         (:ComplexInfinity, sympy.zoo))
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
                obj = eval(Meta.parse("sympy." * pystr * ""))   # These call the sympy functions directly
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
    (res...,)
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

## FIXME: this can probably be simplified, but we we would need to test it.
function _sjtopy(mx::Mxpr{:List})
    @sjdebug(3,"List ", mx)
    a = Array{Any}(undef, 0)
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
        sympy.zoo
    elseif mx == MinusInfinity
        SymPyMinusInfinity
    elseif mx[1] == I
        sympy.Mul(sympy.I,sympy.oo)
    else
        sympy.oo
    end
end

### Equal

function _sjtopy(mx::Mxpr{:Equal})
    if length(mx) == 2
        sympy.Eq(_sjtopy(mx[1]),_sjtopy(mx[2]))
    else
        @symwarn("Unimplemented translation to Sympy", mx)
    end
end

### Part  (defined in parts.jl)

function get_part_one_ind(pyobj::PyObject, ind)
    if pyhead(pyobj) == :Tuple
        return pyobj[ind]
    end
    symerror(string("Can't find part number $ind of ", pyobj))
end

### Args, This function is defined in lists.jl

@doap Args(pyobj::PyCall.PyObject) = pyobj.args

### PyHead

function pyhead(pyobj::PyCall.PyObject)
    return Symbol(pyobj.func.__name__)
end

@mkapprule PyHead :nargs => 1

@sjdoc PyHead """
    PyHead(pyobj)

return the sympy "Head" of `pyobj` as a Symata (Julia) symbol.
"""

@doap PyHead(pyobj::PyCall.PyObject) = pyhead(pyobj)

### PyClass

@sjdoc PyClass """
    PyClass(pyobj)

return the python class of the python object `pyobj`.
"""

@mkapprule PyClass :nargs => 1
@doap PyClass(pyobj::PyCall.PyObject) = pyobj.__class__

### Rewrite

const RewriteDict = Dict( :Gamma => :gamma,
                          :HypergeometricPFQ => :hyper,
                          :Binomial => :binomial,
                          :Sin => :sin)

@mkapprule Rewrite :nargs => 2

@sjdoc Rewrite """
    Rewrite(expr,form)

rewrite `expr` in term of `form`. This is
implemented for some functions that call SymPy. The second argument is
translated, but you may give the SymPy symbol, as well

## Examples

```
Rewrite(CatalanNumber(n), Gamma)
Rewrite(CatalanNumber(1/2), Gamma)
Rewrite(CatalanNumber(n), HypergeometricPFQ)
```
"""

@doap function Rewrite(expr, form::Symbol)
    arg1 = _sjtopy(mx[1])
    transform = get(RewriteDict, form, form)
    pyres = arg1.rewrite(transform)
    pytosj(pyres)
end

### HypergeometricPFQ

do_HypergeometricPFQ(mx::Mxpr{:HypergeometricPFQ}, p::Mxpr{:List}, q::Mxpr{:List}, z::W) where {W<:AbstractFloat} =
    eval_hypergeometric(mx,p,q,z)

do_HypergeometricPFQ(mx::Mxpr{:HypergeometricPFQ}, p::Mxpr{:List}, q::Mxpr{:List}, z::Complex{W}) where {W<:AbstractFloat} =
    eval_hypergeometric(mx,p,q,z)

function eval_hypergeometric(mx, p, q, z)
    result = sjtopy(mx)
    fresult =
        try
            result.evalf()
        catch
            result
        end
    pytosj(fresult)
end

#### MeijerG

@mkapprule MeijerG

@sjdoc MeijerG """
    MeijerG

Meijer G function.
"""

function do_MeijerG(mx::Mxpr{:MeijerG}, p::Mxpr{:List}, q::Mxpr{:List}, z)
    mxc = copy(mx)
    mxc[1] = (p[1],p[2])
    mxc[2] = (q[1],q[2])
    try
        zpy = sjtopy(z)
        ppy = (_sjtopy(p[1]), _sjtopy(p[2]))
        qpy = (_sjtopy(q[1]), _sjtopy(q[2]))
        pyres = sympy.meijerg(ppy,qpy,zpy)
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

do_MeijerG(mx::Mxpr{:MeijerG}, p::Mxpr{:List}, q::Mxpr{:List}, z::W) where {W<:AbstractFloat} =
    eval_meijerg(mx,p,q,z)

do_MeijerG(mx::Mxpr{:MeijerG}, p::Mxpr{:List}, q::Mxpr{:List}, z::Complex{W}) where {W<:AbstractFloat} =
    eval_meijerg(mx,p,q,z)

function eval_meijerg(mx, p, q, z)
    mxc = copy(mx)
    mxc[1] = (p[1],p[2])
    mxc[2] = (q[1],q[2])
    result = sjtopy(mxc)
    fresult =
        try
            result.evalf()
        catch
            result
        end
    pytosj(fresult)
end

#### Tuple

function _sjtopy(t::Tuple)
    @sjdebug(3,"Tuple ", mx)
    (map(_sjtopy,t)...,)
end


## Sympy functions are called here.
function _sjtopy(mx::Mxpr)
    @sjdebug(3,"Mxpr ", mx)
    if mhead(mx) in keys(mx_to_py_dict)
        @sjdebug(1,"In mx_to_py_dict ", mx, " pyhead ", mx_to_py_dict[mhead(mx)] )
        return mx_to_py_dict[mhead(mx)](map(_sjtopy, mx.args)...) # calling a function in our dictionary
    end
    @sjdebug(1,"Make function ", mx)
    pyfunc = sympy.Function(string(mhead(mx)))  # Don't recognize the head, so make it a user function
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
        sym = sympy.Symbol(mx)
        SYMPY_USER_SYMBOLS[mx] = sym
        SYMPY_USER_SYMBOLS_REVERSE[sym] = mx
        return sym
    end
end

#### Assume

@mkapprule Assume

@sjdoc Assume """
    Assume(sym,prop1,prop2,...)

sets properties `prop1, prop2,...` for symbols `sym`.

These properties are used in simplifying relations, `Refine`, integral transforms, etc.

Properties are: commutative, complex, imaginary, real, integer, odd,
even, prime, composite, zero, nonzero, rational, algebraic,
transcendental, irrational, finite, infinite, negative, nonnegative,
positive, nonpositive, hermitian, antihermitian.

These property symbols may contain uppercase characters. For example
`Positive` and `positive` are the same. Use of properties with
lowercase initial is deprecated.
"""

# TODO. We implemented this to use, e.g. the inequality solvers
# Can't find a better way to create the symbol
# prop is not evaluated, rather the symbol 'prop' is set to true
# This uses the 'old' SymPy assumptions system
@doap function Assume(s::Symbol, inprops...)
    ss = string(s)
    props = [ Symbol(lowercase(string(x))) for x in inprops]
    propstrs = map( (x) -> " $x=true ", props)
    propstr = join(propstrs, ", ")
    evalstr = "sympy.Symbol(\"$ss\", $propstr)"
    sym = eval(Meta.parse(evalstr))
    SYMPY_USER_SYMBOLS[s] = sym
    s
end

#### Max

# TODO Max(x,y) == Max(y,x)
# TODO Do numerical arguments in Julia
# TODO Flatten lists
@mkapprule Max :nodefault => true
@doap function Max(args...)
    args = margs(flatten_recursive(mxpr(:List,args...)))
    return pytosj(sympy.Max(mapmargs(sjtopy, args)...))
end

@doap Max() = MinusInfinity

#### Min

@mkapprule Min :nodefault => true
@doap function Min(args...)
    args = margs(flatten_recursive(mxpr(:List,args...)))
    return pytosj(sympy.Min(mapmargs(sjtopy, args)...))
end

@doap Min() = Infinity

_sjtopy(mx::Rational{T}) where {T<:Integer} = sympy.Rational(numerator(mx),denominator(mx))

_sjtopy(mx::Number) = mx

function _sjtopy(s::Qsym)
    f = sympy.Function("_context")
    f(_sjtopy(s.context),_sjtopy(s.name))
end

# For our LaplaceTransform code, (etc.)
_sjtopy(a::Array{T,1}) where {T} =  map(_sjtopy, a)
_sjtopy(expr::PyCall.PyObject) = expr
_sjtopy(x::AbstractString) =  x

# Default conversion. Don't error, but only warn. Then return x so that we
# can capture and inspect it.
function _sjtopy(x)
    @symwarn("sjtopy: Unable to convert $x from Symata to SymPy")
    return x
end

"""
    init_sympy()

Initialize SymPy. SymPy must be loaded at runtime. `init_sympy()` is called
in the Symata module ` __init__` function.
"""
function init_sympy()
    import_sympy()
    eval(Meta.parse("const dummy_arg = sympy.Symbol(\"DUMMY\")"))
    eval(Meta.parse("const SymPyMinusInfinity = sympy.Mul.(-1 , sympy.oo)"))
    make_sympy_to_symata()
    populate_py_to_mx_dict()
    mk_py_to_mx_funcs()
    populate_user_symbol_dict()
    populate_mx_to_py_dict()
    mk_mx_to_py_funcs()
    for s in sympysyms1
        @eval copy!($(Symbol("sympy_", s)) , sympy.$s)
#        @eval copy!($(Symbol("sympy_", s)) , sympy[$(QuoteNode(s))])
    end
    copy!(sympy_True,sympy.logic.boolalg.BooleanTrue)
    copy!(sympy_False,sympy.logic.boolalg.BooleanFalse)
    populate_rewrite_dict()
    init_isympy()
end

const symatapydir = joinpath(dirname(@__FILE__), "..", "pysrc")

const symatapy = PyCall.PyNULL()

function init_isympy()
    try
        pushfirst!(PyVector(pyimport("sys")."path"), symatapydir)
        copy!(symatapy, pyimport("isympy"))
    catch
        nothing
    end
end

"""
    isympy()

enters the ipython shell with sympy imported.
"""
function isympy()
    try
        symatapy.main()
    catch
        nothing
    end
end

## These are not used anywhere
# typename(x::PyCall.PyObject) = pytypeof(x)[:__name__]
# name(x::PyCall.PyObject) = x[:__name__]

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
                   (true, $(esc(pycall)))
                 catch pyerr
                  (false,pyerr)
                 end
                 if sflag == false
                 @warn($errstr)
                   setkerneloptions(:sympy_error, _pyres)
                   return $(esc(return_val_err))
                 end
                 _pyres
              end
           )
end

#### PyDoc

@mkapprule PyDoc :nargs => 1

function do_PyDoc(mx::Mxpr{:PyDoc},sym)
    try eval(Meta.parse("println(sympy.$(string(sym)).__doc__)"))
    catch
        Null
    end
end

@sjdoc PyDoc """
    PyDoc(sym)

prints the documentation for the symbol `sym`, if available.
"""

#### Arithmetic

## Define methods for Sympy objects.

## These break the ability to include sympy objects in Symata expressions with
## heads such as :Times, via `*`. It's not clear which, if either is more useful

## PyCall.jl has now implemented these.
## I comment out the definitions here, so they don't conflict. GJL 11MAY2018
# for sympair in ( (:*, :Mul), (:+, :Add), (:^, :Pow) )
#     jop = sympair[1]
#     qj = QuoteNode(sympair[1])
#     qsp = QuoteNode(sympair[2])
#     @eval Base.$(jop)(a::PyCall.PyObject, b::PyCall.PyObject) = sympy[$qsp](a,b)
#     @eval Base.$(jop)(a::PyCall.PyObject, b) = sympy[$qsp](a,b)
#     @eval Base.$(jop)(a, b::PyCall.PyObject) = sympy[$qsp](a,b)
#     @eval Base.$(jop)(a::PyCall.PyObject, b::Number) = sympy[$qsp](a,b)
#     @eval Base.$(jop)(a::PyCall.PyObject, b::Integer) = sympy[$qsp](a,b)
# end

# Base.:-(a::PyCall.PyObject) = -1 * a
# Base.:-(a::PyCall.PyObject, b::PyCall.PyObject) = a + (-b)
# Base.:-(a, b::PyCall.PyObject) = a + (-b)
# Base.:-(a::PyCall.PyObject, b) = a + (-b)
