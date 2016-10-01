# This file is a legacy from the start of the project, when all the
# rules were in one file.  We are migrating everything here to other
# files.
#
#  Some 'apprules' definitions.
#   These evaluate expressions with builtin (protected) heads.
#   They are called from meval.

#### Increment

@sjdoc Increment "
Increment(n) increments the value of n by 1 and returns the old value.
"

@mkapprule Increment :nargs => 1

@doap function Increment(x::SJSym)
    @checkunbound(mx,x,xval)
    do_increment1(mx,x,xval)
end

function do_increment1{T<:Number}(mx,x,xval::T)
    setsymval(x,mplus(xval,1))  # maybe + is ok here.
    return xval
end

function do_increment1(mx,x,val)
    setsymval(x,doeval(mxpr(:Plus,val,1)))
    return val
end

#### Decrement

@sjdoc Decrement "
Decrement(n) decrements the value of n by 1 and returns the old value.
"

@mkapprule Decrement :nargs => 1

#function do_Decrement(mx, x::SJSym)
@doap function Decrement(x::SJSym)
    @checkunbound(mx,x,xval)
    do_decrement1(mx,x,xval)
end

function do_decrement1{T<:Number}(mx,x,xval::T)
    setsymval(x,mplus(xval,-1))  # maybe + is ok here.
    return xval
end

function do_decrement1(mx,x,val)
    setsymval(x,doeval(mxpr(:Plus,val,-1)))
    return val
end

#### TimesBy

@sjdoc TimesBy "
TimesBy(a,b), or a *= b, sets a to a * b and returns the new value. This is currently
faster than a = a * b for numbers.
"

@mkapprule TimesBy :nargs => 2

function do_TimesBy(mx::Mxpr{:TimesBy}, x::SJSym,val)
    @checkunbound(mx,x,xval)
    do_TimesBy1(mx,x,xval,val)
end

function do_TimesBy1{T<:Number,V<:Number}(mx,x,xval::T, val::V)
    r = mmul(xval,val)
    setsymval(x,r)
    return r
end
function do_TimesBy1(mx,x,xval,val)
    setsymval(x,doeval(mxpr(:Set,x, mxpr(:Times,xval,val))))
    return symval(x)
end


#### AddTo

@sjdoc AddTo "
AddTo(a,b), or a += b, sets a to a + b and returns the new value. This is currently
faster than a = a + b for numbers.
"

@mkapprule AddTo :nargs => 2

function do_AddTo(mx::Mxpr{:AddTo},x::SJSym,val)
    @checkunbound(mx,x,xval)
    do_AddTo1(mx,x,xval,val)
end

function do_AddTo1{T<:Number,V<:Number}(mx,x,xval::T, val::V)
    r = mplus(xval,val)
    setsymval(x,r)
    return r
end
function do_AddTo1(mx,x,xval,val)
    setsymval(x,doeval(mxpr(:Set,x, mxpr(:Plus,xval,val))))
    return symval(x)
end


#### Dump and DumpHold

@sjdoc Dump "
Dump(expr) prints an internal representation of expr. This is similar to
Julia `dump'.
"


@sjdoc DumpHold "
DumpHold(expr) prints an internal representation of expr. This is similar to
Julia `dump'. In contrast to `Dump', expr is not evaluated before it's internal
representation is printed.
"

@sjseealso_group(Dump,DumpHold)

# DumpHold does not evaluate args before dumping
apprules{T<:Union{Mxpr{:Dump},Mxpr{:DumpHold}}}(mx::T) = for a in margs(mx) is_SJSym(a) ? dump(getssym(a)) : dump(a) end

#### Length

@sjdoc Length "
Length(expr) prints the length of SJulia expressions and Julia objects. For
SJulia expressions, the length is the number or arguments. For scalar Julia
types, the length is zero. For Array's and Dict's the length is the same as
Julia `length'.
"

apprules(mx::Mxpr{:Length}) = symjlength(mx[1])
symjlength(mx::Mxpr) = length(margs(mx))
symjlength(x) = length(x)

#### Part

@sjdoc Part "
Part(expr,n) or expr[n], returns the nth element of expression expr.
Part(expr,n1,n2,...) or expr[n1,n2,...] returns a nested part.
The same can be achieved less efficiently with expr[n1][n2]...
expr[n] = val sets the nth part of expr to val. n and val are evaluated
normally. expr is evaluated once.
expr[n] also returns the nth element of instances of several
Julia types such as Array, or the element with key 'n' for Dict's.
"

# Get part of expression. Julia :ref is mapped to :Part
# a[i] parses to Part(a,i), and a[i][j] to Part(Part(a,i),j)
# a[i,j] parses to Part(a,i,j).

@mkapprule Part

function do_Part(mx::Mxpr{:Part},texpr,tinds...)
    for j in 1:length(tinds)
        texpr = get_part_one_ind(texpr,tinds[j])
    end
    return texpr
end

function get_part_one_ind{T<:Integer, V<:Union{Mxpr,Array}}(texpr::V,ind::T)
#    ind::Int = tind
    ind = ind < 0 ? length(texpr)+ind+1 : ind
    texpr = ind == 0 ? mhead(texpr) : texpr[ind]
    return texpr
end

get_part_one_ind{T<:Dict}(texpr::T,tind) = texpr[tind]  # Part 0 can't return "Dict" because it could be a key.
get_part_one_ind{T<:Tuple}(texpr::T,tind) = texpr[tind]

function get_part_one_ind(texpr::Mxpr,tind::Mxpr{:Span})
    spanargs = margs(tind)
    lsp = length(spanargs)
    if lsp == 2
        nargs = view(margs(texpr),spanargs[1]:spanargs[2]) # need function to do this translation
    elseif lsp == 3
        nargs = view(margs(texpr),spanargs[1]:spanargs[3]:spanargs[2])
    end
    texpr = mxpr(mhead(texpr),nargs...) # we need splice to copy Int Array to Any Array
    return texpr
end

#### Span

@sjdoc Span "
Span(a,b) or a:b represents elements a through b.
Span(a,b,c) or a:b:c represents elements a through b in steps of c.
expr[a:b] returns elements a through b of expr, with the same head as expr.
"

#### DownValues

@sjdoc DownValues "
DownValues(s) returns a List of DownValues associated with symbol s. These are values
that are typically set with the declarative \"function definition\".
"

@sjexamp( DownValues,
         ("ClearAll(f)",""),
         ("f(x_) := x^2",""),
         ("DownValues(f)", "[HoldPattern(f(x_))->(x^2)]"))
apprules(mx::Mxpr{:DownValues}) = sjlistdownvalues(mx[1])


#### UpValues

@sjdoc UpValues "
UpValues(s) returns a List of UpValues associated with symbol s. These are values
that are typically set with UpSet.
"

apprules(mx::Mxpr{:UpValues}) = sjlistupvalues(mx[1])

#### Example

@sjdoc Example "
Example(s)
runs (evaluates) all examples for the symbol s, typically a function or variable.
Input, output, and comments are displayed. Input strings
for the example are pushed onto the terminal history so they can be retrieved and
edited and re-evaluated.

Example(s,n)
runs the nth example for symbol s. When viewing
documentation strings via ? SomeHead, the examples are printed along with the
documentation string, but are not evaluated.

Example()
Returns a list of all example topics.
"

@mkapprule Example  :nargs => 0:2

do_Example(mx::Mxpr{:Example}) = mxprcf(:List,Any[sort(collect(keys(SJEXAMPLES)))...])
do_Example(mx::Mxpr{:Example}, topic) = do_examples(mx[1])
do_Example(mx::Mxpr{:Example}, topic, n::Int) = do_example_n(mx[1],n)

#### Replace

@sjdoc Replace "
Replace(expr,rule) replaces parts in expr according to Rule rule.
"
@sjseealso_group(Replace,ReplaceAll)
@sjexamp(Replace,
         ("Clear(a,b,c)",""),
         ("Replace( Cos(a+b)^2 + Sin(a+c)^2, Cos(x_)^2 + Sin(x_)^2 => 1)",
          "Cos(a + b)^2 + Sin(a + c)^2", "This expression does not match the pattern."),
         ("Replace( Cos(a+b)^2 + Sin(a+b)^2, Cos(x_)^2 + Sin(x_)^2 => 1)",
          "1", "This expression does match the pattern."))
function apprules(mx::Mxpr{:Replace})
    doreplace(mx,margs(mx)...)
end

function doreplace(mx,expr,r::Rules)
    (success, result) = replace(expr,r)
    result
end

function doreplace(mx,expr,r::Rules,inlevelspec)
    levelspec = make_level_specification(expr,inlevelspec)
    (success, result) = replace(levelspec,expr,r)
    result
end

doreplace(mx,a,b) = mx

doreplace(mx,args...) = mx

#### ReplaceAll

@sjdoc ReplaceAll "
ReplaceAll(expr,rule) replaces parts at all levels in expr according to Rule rule. This includes
the Head of expr, and the entire expr.
ReplaceAll(expr,List(rule1,rule2,...)) replaces parts at all levels in expr according to the
list or rules. If given explicitly, the rules should be given as List(...) rather than
[...] because of a parsing error.
"

apprules(mx::Mxpr{:ReplaceAll}) = doreplaceall(mx,mx[1],mx[2])

function doreplaceall{T<:Rules}(mx,expr,r::T)
    replaceall(expr,r)
end

function doreplaceall(mx,expr,rs::Mxpr{:List})
    rsa = Array(Any,0)
    for i in 1:length(rs)
        if isa(rs[i],Rules)
            push!(rsa, rs[i])
        else
            warn("ReplaceRepeated expected Rule  got ", rs[i])
            nothing  # do something better here, like return mx
        end
    end
    replaceall(expr,rsa)
end
doreplaceall(mx,a,b) = mx

@sjexamp( ReplaceAll,
         ("ClearAll(zz,b,c)",""),
         ("zz = 10 * b^2 * (c+d)","zz = 10 * b^2 * (c+d)"),
         ("ReplaceAll(zz, List(c => 3,d => 2) )", "50*b^2"))

@mkapprule ReplaceRepeated :options => Dict( :MaxIterations => 65536 )

@sjdoc ReplaceRepeated "
ReplaceRepeated(expr,rules) performs ReplaceAll(expr,rules) repeatedly until expr no longer changes.
"

do_ReplaceRepeated{T<:Rules}(mx::Mxpr{:ReplaceRepeated},expr,r::T; kws...) = replacerepeated(expr,r; kws...)

function do_ReplaceRepeated(mx::Mxpr{:ReplaceRepeated},expr,rs::Mxpr{:List}; kws...)
    rsa = Array(Any,0)
    for i in 1:length(rs)
        if isa(rs[i],Rules)
            push!(rsa, rs[i])
        else
            warn("ReplaceRepeated expected Rule, got ", rs[i])
            nothing  # do something better here, like return mx
        end
    end
    replacerepeated(expr,rsa; kws...)
end

do_ReplaceRepeated(mx::Mxpr{:ReplaceRepeated},a,b; kws...) = mx

#### MatchQ

@sjdoc MatchQ "
MatchQ(expr,pattern) returns true if expr matches pattern. MatchQ can
be used in operator form. For example, myintq = MatchQ(_Integer).
"
@sjexamp( MatchQ,
         ("MatchQ( 1, _Integer)", "true"),
         ("ClearAll(gg,xx,b)",""),
         ("MatchQ( gg(xx) , _gg)", "true"),
         ("MatchQ( b^2, _^2)","true"))

function apprules(mx::Mxpr{:MatchQ})
    do_MatchQ(mx,margs(mx)...)
end

function do_MatchQ(mx,expr,pat)
    matchq(expr,pat)
#    (gotmatch,cap) = match_and_capt(expr,patterntoBlank(pat))
#    gotmatch
end

function matchq(expr,pat)
    (gotmatch,cap) = match_and_capt(expr,patterntoBlank(pat))
    gotmatch
end

function do_MatchQ(mx,pat)
    mx
end

#### GenHead
# for operator form of MatchQ
# do_GenHead in evaluation.jl curries the first argument
function do_GenHead(mx,head::Mxpr{:MatchQ})
    mxpr(mhead(head),copy(margs(mx))...,margs(head)...)
end

#### FullForm

@sjdoc FullForm "
FullForm(expr) prints the internal representation of expr and all sub-expressions as
Head(arg1,arg2,...). Normal output may use infix notation instead. But, expressions
may always be entered in 'FullForm'.
"
@sjexamp( FullForm,
          ("Clear(a,b)",""),
          ("a+b","a+b"),
          ("FullForm(a+b)","Plus(a,b)"))

@sjexamp( FullForm,
          ("Sqrt(2)", "Power(2,1//2)", "Internally, square roots are represented as powers."),
          ("Sqrt(2)[2]", "1//2", "The second part of the expression is '1//2'"))

# FullForm is implemented in io.jl

## A few Number rules

# These may not all be necessary.

apprules(mx::Mxpr{://}) = makerat(mx,mx[1],mx[2])
makerat{T<:Number}(mx::Mxpr{://},n::T,d::T) = n//d
makerat(mx,n,d) = mx

#### Power

@mkapprule Power :nargs => 2

# function apprules(mx::Mxpr{:Power})
#     do_Power(mx,mx[1],mx[2])
# end

do_Power{T<:Integer, V<:Symbolic}(mx::Mxpr{:Power}, b::V, n::T) = n == 1 ? b : n == 0 ? one(n) : mx

@doap Power(b::SJSym, expt) = b == :E ? dopowerE(mx, expt) : mx
dopowerE{T<:AbstractFloat}(mx, expt::T) = exp(expt)
dopowerE{T<:AbstractFloat}(mx, expt::Complex{T}) = exp(expt)

function dopowerE(mx, expt)
    syexpt = sjtopy(expt)
    syres = sympy[:exp](syexpt)
    res = pytosj(syres)
    if is_Mxpr(res, :Exp)
        res[1] == expt && return mx
        return mxpr(:Power, :E, margs(res)...)
    end
    res
end

# Don't handle this yet.
do_Power{T<:Integer,V<:Integer}(mx::Mxpr{:Power},   b::Complex{T},expt::Rational{V}) = mx
do_Power{T<:Integer,V<:Integer}(mx::Mxpr{:Power},   b::Complex{T},expt::Complex{Rational{V}}) = mx

do_Power{T<:Number,V<:Number}(mx::Mxpr{:Power},   b::T,expt::V) = mpow(b,expt)

# For some reason, we need this integer rule. For instance for (a^2)^2 --> a^4
do_Power{T<:Integer}(mx::Mxpr{:Power}, b::Mxpr{:Power}, exp::T) = mpow(base(b), mmul(exp,expt(b)))
do_Power{T<:Real}(   mx::Mxpr{:Power}, b::Mxpr{:Power}, exp::T) = mpow(base(b), mmul(exp,expt(b)))

do_Power(mx::Mxpr{:Power},   b::Mxpr{:Power}, exp) = is_Number(expt(b)) ? mpow(base(b), mmul(expt(b),exp)) : mx

do_Power{T<:AbstractFloat}(mx::Mxpr{:Power},   b::SJSym,expt::Complex{T}) = b == :E ? exp(expt) : mx

do_Power{T<:Integer}(mx::Mxpr{:Power},   b::Mxpr{:DirectedInfinity},expt::T) = mpow(b,expt)
do_Power{T<:Number}(mx::Mxpr{:Power},   b::Mxpr{:DirectedInfinity},expt::T) = mpow(b,expt)

# Check if the exact answer is an integer.
function do_Power{T<:Integer, V<:Rational}(mx::Mxpr{:Power},b::T,expt::V)
    mpow(b,expt)
end

# function disabledo_Power{T<:Integer, V<:Rational}(mx::Mxpr{:Power},b::T,exp::V)
#     gotneg::Bool = false
#     b == -1 && return mx
#     if b < 0
#         gotneg = true
#         b *= -1
#     end
#     res = b^exp
#     ires = round(T,res)
#     nrat = Rational(den(exp),num(exp))
#     if ires^nrat == b
#         if gotneg
#             return ires == 1 ? mxprcf(:Power, -1, exp) : mxprcf(:Times, ires, mxprcf(:Power, -1, exp))
#         else
#             return ires
#         end
#     else
#         return mx
#     end
# end

do_Power(mx,b,e) = mx

#### convert to BigInt or BigFloat. We cannot yet do this automatically

@sjdoc BI "
BI(n) converts the number n to a BigInt. SJulia currently neither
detects integer overflow, nor automatically promote integers to BigInts.
But, a literal integer will automatically be given a large enough storage type without using
BI.
"

@sjseealso_group(BI,BF,Big)
@sjdoc BF "
BF(n), or BF\"n\", converts the number, or string n to a BigFloat. SJulia currently neither
detects overflow, nor automatically promotes types from fixed to arbitrary precision. The form
BF\"n\" is more efficient, being a julia macro that converts the string \"n\" upon parsing.
"

@sjdoc Big "
Convert a number to a maximum precision representation (typically
'BigInt' or 'BigFloat')
"

apprules(mx::Mxpr{:BI}) = dobigint(mx,mx[1])
dobigint(mx,x) = mx
dobigint{T<:Number}(mx,x::T) = BigInt(x)
dobigint{T<:AbstractString}(mx,x::T) = parse(BigInt,x)

apprules(mx::Mxpr{:BF}) = dobigfloat(mx,mx[1])
dobigfloat(mx,x) = mx
dobigfloat{T<:Number}(mx,x::T) = BigFloat(x)
dobigfloat{T<:AbstractString}(mx,x::T) = parse(BigFloat,x)

apprules(mx::Mxpr{:Big}) = do_Big(mx,mx[1])
do_Big(mx,x) = mx
do_Big{T<:Number}(mx,x::T) = big(x)

apprules(mx::Mxpr{:Minus}) = is_Number(mx[1]) ? -mx[1] : -1 * mx[1]

#### Timing evaluation

@sjdoc Timing "
Timing(expr) evaluates expr and returns a list of the elapsed CPU time
and the result.
"

@sjseealso_group(Timing,Allocated,Time,Trace)
function apprules(mxt::Mxpr{:Timing})
    t = @elapsed begin
        reset_meval_count()
        mx = doeval(mxt[1])
        setsymval(:ans,mx)
    end
    mxpr(:List,t,mx)
end

#### Allocated

@sjdoc Allocated "
Allocated(expr) evaluates expr and returns a list of the memory allocated
and the result of the evaluation.
"
function apprules(mxt::Mxpr{:Allocated})
    local mx
    a = @allocated begin
        reset_meval_count()
        mx = doeval(mxt[1])
        setsymval(:ans,mx)  ## why here ?
    end
    mxpr(:List,a,mx)
end

#### HAge, FixedQ and UnFix

@sjdoc HAge "
HAge(s) returns the timestamp for the expression or symbol s.
Using this timestamp to avoid unnecessary evaluation is a partially
implemented feature.
"
@sjseealso_group(HAge,Age,FixedQ,Syms,DirtyQ,Unfix)
# Get the last-altered timestamp of an expression or symbol
apprules(mx::Mxpr{:HAge}) = hdo_getage(mx,mx[1])
hdo_getage(mx,s::SJSym) = Int(symage(s))
hdo_getage(mx,s::Mxpr) = Int(getage(s))
hdo_getage(mx,x) = mx

apprules(mx::Mxpr{:Age}) = do_getage(mx,mx[1])
do_getage(mx,s::SJSym) = Int(symage(s))
do_getage(mx,s::Mxpr) = Int(getage(s))
do_getage(mx,x) = mx

@sjdoc FixedQ "
FixedQ(expr) returns the status of the fixed point bit, which tells whether expr
is expected to evaluate to itself in the current environment. This is partially
implemented.
"
# Get fixed-point bit. Idea is to set it if expr evaluates to itself.
apprules(mx::Mxpr{:FixedQ}) = is_fixed(mx[1])

@sjdoc Unfix "
Unfix(expr) unsets the fixed flag on expr, causing it to be evaluated.
This is a workaround for bugs that cause an expression to be marked fixed
before it is completely evaluated.
"
function apprules(mx::Mxpr{:Unfix})
    unsetfixed(mx[1])
    mx[1]
end

#### Syms

@sjdoc Syms "
Syms(m) returns a List of the symbols that the expression m 'depends' on. The
list is wrapped in HoldForm in order to prevent evaluation of the symbols.
"

# Syms has HoldAll
function apprules(mx::Mxpr{:Syms})
    mxpr(:HoldForm,do_syms(mx[1]))
end

@sjdoc BuiltIns "
BuiltIns() returns a List of all \"builtin\" symbols. These are in fact all symbols that
have the Protected attribute.
"
apprules(mx::Mxpr{:BuiltIns}) = protectedsymbols()

@sjdoc UserSyms "
UserSyms() returns a symbols that have not been imported from the System namespace.
This is all user defined symbols (unless you have imported symbols from elsewhere).
"

@mkapprule UserSyms  :nargs => 0

@doap UserSyms() = usersymbolsList()

# This is the old version
@mkapprule UserSyms2 :nargs => 0

@doap UserSyms2() = usersymbolsListold()

@sjdoc CurrentContext "
CurrentContext() returns the name of the current context.
"

@mkapprule CurrentContext :nargs => 0

# This does not return a context or module type, because we need to
# keep types out of the language as much as possible. Everything is
# an expression! In fact, we should probably return a string.

@doap CurrentContext() = string(get_current_context_name())



#### ExpandA, only a bit is implemented. Sympy Expand is more capable.

@sjdoc ExpandA "
ExpandA(expr) expands products in expr. This is only partially implemented,
mostly to test the efficiency of evaluation and evaluation control. The
sympy version Expand() is more capable, but slower.
"
apprules(mx::Mxpr{:ExpandA}) = _doexpand(mx[1])

#### RandomReal

@mkapprule RandomReal

do_RandomReal(mx::Mxpr{:RandomReal}) = return rand()

#### Random

@mkapprule Random

do_Random(mx::Mxpr{:Random}) = rand()

function do_Random(mx::Mxpr{:Random}, sym::SJSym)
    if sym == :Integer
        rand(0:1)
    elseif sym == :Real
        rand()
    elseif sym == :Complex
        complex(rand(),rand())
    else
        mx
    end
end

#### Counts

@mkapprule Counts

@sjdoc Counts "
Counts(list) returns a dictionary of the number of times each distinct element of list
occurs.
"

# We should use an ordered dict, but the package is broken
function do_Counts(mx::Mxpr{:Counts}, list::Mxpr{:List})
#    d = OrderedDict{Any,Any}()  # We need to canonicalize this
    d = Dict{Any,Any}()  # We need to canonicalize this
    for el in margs(list)
        val = get!(d,el,0)
        d[el] = val + 1
    end
    d
end

#### Pause

@sjdoc Pause "
Pause(x) pauses (i.e.sleeps) for x seconds.
"

@mkapprule Pause  :nargs => 1
@doap Pause{T<:Real}(x::T) = sleep(x)
