##  Some 'apprules' definitions.
#   These evaluate expressions with builtin (protected) heads.
#   They are called from meval.

# One or more arguments
# type OneOrMore end

### This file is a legacy from the start of the project.
### We are migrating everything here to other files.

# Check if we are trying to add to a symbol bound to itself. If
# so, we warn and return the unevaluated expression.
macro checkunbound(mx,x,sv)
    esc( quote
           $sv = symval($x)
           if $sv == $x
             warn("The symbol " * string($x) * " does not have a value, so it's value cannot be changed")
             setfixed($mx)
             return $mx
          end
    end)
end


#### Set and SetDelayed

function checkprotect(s::SJSym)
    get_attribute(symname(s),:Protected) &&
    error("Symbol '",symname(s), "' is protected.")
end
checkprotect(mx::Mxpr) = checkprotect(mhead(mx))

function warncheckprotect(s::SJSym)
    if get_attribute(symname(s),:Protected)
        warn(string("Symbol '",symname(s), "' is protected."))
        return false
    else
        return true
    end
end
warncheckprotect(mx::Mxpr) = warncheckprotect(mhead(mx))

@sjdoc Set "
Set(a,b), a = b
Sets the value of a to b. b is evaluated only once, when `a=b' is evaluated.
obj[i,j,...] = val sets a part of obj to val. obj can be an SJulia expression
or a Julia object, such as an Array or Dict.
"

@sjseealso_group( Set, SetDelayed, UpSet, DownValues, UpValues )

@sjexamp( Set,
         ("Clear(a,b,c)",""),
         ("b = a", "a"),
         ("a = 1", "1"),
         ("c = a", "1"),
         ("a = 2", "2"),
         ("b", "2"),
         ("c", "1"))

@sjdoc SetDelayed "
SetDelayed(a,b), a := b
Whenever a is evaluated, b is evaluated and the result is assigned to a.
So a is not set to the value of b at the time a := b is evaluated, but
rather to the current value of b every time a is evaluated.
"

# Set SJSym value.
# Set has HoldFirst, SetDelayed has HoldAll.

#@mkapprule Set :nargs => OneOrMore
@mkapprule Set

function do_Set(mx::Mxpr{:Set})
    warn("Set called with 0 arguments; 1 or more arguments are expected.")
    setfixed(mx)
    mx
end

function do_Set(mx::Mxpr{:Set}, lhs::SJSym)
    checkprotect(lhs)
    rhs = mxprcf(:Sequence)
    setsymval(lhs,rhs)
    setdefinition(lhs, mx)
    rhs
end

apprules(mx::Mxpr{:SetDelayed}) = setdelayed(mx,mx[1],mx[2])

# getsym(symname(lhs)) is because a copy of symbol is being made somewhere
# so we look up the original in the table
# SetDelayed is not correct. rhs is also evaluated because
# lhs is evaluated twice in order to find fixed point.
# we have treat this specially somehow, not ordinary evaluation.
#
# Note added (Apr 2016): This is working AFAICT
# For evaluation, we can use setsymval for both set and setdelayed
# The only difference is whether we return the rhs.
# But, to save the definitions to a file, we need to record whether we had set or setdelayed,
# so we use setdelayedval
function setdelayed(mx,lhs::SJSym, rhs)
    checkprotect(lhs)
    setsymval(lhs,rhs)   # Using this works just as well. But, for printing, we don't whether Set or SetDelayed
    setdefinition(lhs, mx)
    Null
end

function do_Set(mx::Mxpr{:Set},lhs::SJSym, rhs)
    checkprotect(lhs)
    setsymval(lhs,rhs)
    setdefinition(lhs, mx)
    rhs
end

# Create DownValue. "function" definition
# eg f(x_) := x  defines a DownValue for the SJSym f
function setdelayed(mx,lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    set_downvalue(mx,mhead(lhs),rule) # push DownValue
    rule
    Null
end

function do_Set(mx::Mxpr{:Set},lhs::Mxpr{:Part}, rhs::Mxpr{:Module})
    error("$mx is not implemented")
end

# Mma is not clear but seems to evaluate the first arg to the lhs (the expression
# whose part we want) exactly once. We should document what we do.
# We check is_Number several times, because we may have a Dict.
function do_Set(mx::Mxpr{:Set},lhs::Mxpr{:Part}, rhs)
    ex0 = meval(expr(lhs))  # evaluate once, eg, to get expr from symbol.
    tinds = inds(lhs)
    ex = ex0
    for j in 1:length(tinds)-1
        ind = doeval(tinds[j])
        ind = is_Number(ind) && ind < 0 ? length(ex)+ind+1 : ind
        ex = is_Number(ind) && ind == 0 ? mhead(ex) : ex[ind]
    end
    val = doeval(rhs)
    ind = doeval(tinds[end])
    ind = is_Number(ind) && ind < 0 ? length(ex)+ind+1 : ind
    if is_Number(ind) && ind == 0
        ex.head = val  #  TODO violation of abstraction
    else
        ex[ind] = val
    end
    unsetfixed(ex0) # maybe we can optimize this
    val
end

# Optimize a bit. Localize variables once, not every time pattern is evaluated
setdelayed(mx,lhs::Mxpr, rhs::Mxpr{:Module}) = setdelayed(mx,lhs,localize_module!(rhs))
do_Set(mx,lhs::Mxpr, rhs::Mxpr{:Module}) = do_Set(mx,lhs,localize_module!(rhs))

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

#### UpSet

@sjdoc UpSet "
UpSet(a(g(x_)),b), or a(g(x_)) ^= b  associates the transformation rule with g.
"

apprules(mx::Mxpr{:UpSet}) = upset(mx,mx[1],mx[2])

function upset(mx,lhs::Mxpr, rhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    for i in 1:length(lhs)
        m = lhs[i]
        if is_Mxpr(m) && warncheckprotect(m)
            set_upvalue(mx,mhead(m),rule)
        elseif is_SJSym(m) && warncheckprotect(m)
            set_upvalue(mx, m,rule)
        end
    end
    return rhs
end

apprules(mx::Mxpr{:UpSetDelayed}) = upsetdelayed(mx,mx[1],mx[2])

# I think the only difference with UpSet again, is we don't return the
# rhs.
function upsetdelayed(mx,lhs::Mxpr, rhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    for i in 1:length(lhs)
        m = lhs[i]
        if is_Mxpr(m) && warncheckprotect(m)
            set_upvalue(mx,mhead(m),rule)
        elseif is_SJSym(m) && warncheckprotect(m)
            set_upvalue(mx, m,rule)
        end
    end
    Null
end



function do_Set(mx::Mxpr{:Set},lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    set_downvalue(mx, mhead(lhs),rule) # push DownValue
    rule
    nothing
end


#### Symbol

@sjdoc Symbol "
Symbol(str) converts the string str to a symbol. For example if a is 1,
then Symbol(\"a\") returns 1.
"

function apprules(mx::Mxpr{:Symbol})
    dosymbol(mx,mx[1])
end
dosymbol(mx,s::AbstractString) = getsym(symbol(s))
dosymbol(mx,x) = (warn("Symbol: expected a string"); mx)

#### Clear

@sjdoc Clear "
Clear(x,y,z) removes the values associated with x,y,z. It does not remove
their DownValues.

Clear(Out) deletes all of the saved Output lines. It actually replaces them with
the value `Null'.
"

@sjseealso_group(Clear, ClearAll)

# 'Clear' a value. ie. set symbol's value to its name
function apprules(mx::Mxpr{:Clear})  # This will be threaded over anyway
    @inbounds for a in margs(mx)  # no inbounds does not work here
        if a == :Out
            for i in 1:length(Output)
                Output[i] = :Null   # temporary solutions
            end
            return Null
        end
        checkprotect(a)
        setsymval(a,symname(a))
    end
    Null
end

#### ClearAll

@sjdoc ClearAll "
ClearAll(x,y,z) removes all values and DownValues associated with x,y,z. The
symbols are removed from the symbol table and will not appear in the list returned
by UserSyms().
"

# Remove all values associate with SJSym. values and DownValues
function apprules(mx::Mxpr{:ClearAll})  # already threaded
    for a in margs(mx)
        checkprotect(a)
#        if is_type_less(a,String)  TODO implement globing, etc.
        #        else
        delete_sym(a)
    end
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

#### LeafCount

@sjdoc LeafCount "
LeafCount(expr) gives the number of indivisible (Part can't be taken) elements in expr.
This amounts to counting all the Heads and all of the arguments that are not of type Mxpr.
A more accurate name is NodeCount.
"
apprules(mx::Mxpr{:LeafCount}) = leaf_count(mx[1])

#### ByteCount

@sjdoc ByteCount "
ByteCount(expr) gives number of bytes in expr.
"
apprules(mx::Mxpr{:ByteCount}) = byte_count(mx[1])

#### Depth

@sjdoc Depth "
Depth(expr) gives the maximum number of indices required to specify
any part of expr, plus 1.
"
apprules(mx::Mxpr{:Depth}) = depth(mx[1])

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
        nargs = slice(margs(texpr),spanargs[1]:spanargs[2]) # need function to do this translation
    elseif lsp == 3
        nargs = slice(margs(texpr),spanargs[1]:spanargs[3]:spanargs[2])
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

# @sjexamp( UpValues,
#          ("ClearAll(f)",""),
#          ("f(x_) := x^2",""),
#          ("UpValues(f)", "[HoldPattern(f(x_))->(x^2)]"))
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
apprules(mx::Mxpr{:Replace}) = doreplace(mx,mx[1],mx[2])

typealias Rules Union{Mxpr{:Rule},Mxpr{:RuleDelayed}}
doreplace{T<:Rules}(mx,expr,r::T) = replace(expr,Rule_to_PRule(r))

doreplace(mx,a,b) = mx

#### ReplaceAll

@sjdoc ReplaceAll "
ReplaceAll(expr,rule) replaces parts at all levels in expr according to Rule rule.
ReplaceAll(expr,List(rule1,rule2,...)) replaces parts at all levels in expr according to the
list or rules. If given explicitly, the rules should be given as List(...) rather than
[...] because of a parsing error.
"

apprules(mx::Mxpr{:ReplaceAll}) = doreplaceall(mx,mx[1],mx[2])

doreplaceall{T<:Rules}(mx,expr,r::T) = replaceall(expr,Rule_to_PRule(r))

function doreplaceall(mx,expr,rs::Mxpr{:List})
    rsa = Array(PRule,0)
    for i in 1:length(rs)
        if typeof(rs[i]) <: Rules
            push!(rsa, Rule_to_PRule(rs[i]))
        else
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


# These are post option removal args
#  ReplaceRepeated::argrx:
#   ReplaceRepeated called with 0 arguments; 2 arguments are expected.

@mkapprule ReplaceRepeated :options => Dict( :MaxIterations => 65536 )

#@mkapprule ReplaceRepeated

#apprules(mx::Mxpr{:ReplaceRepeated}) = do_ReplaceRepeated(mx,mx[1],mx[2])

do_ReplaceRepeated{T<:Rules}(mx::Mxpr{:ReplaceRepeated},expr,r::T; kws...) = replacerepeated(expr,Rule_to_PRule(r); kws...)

function do_ReplaceRepeated(mx::Mxpr{:ReplaceRepeated},expr,rs::Mxpr{:List}; kws...)
    rsa = Array(PRule,0)
    for i in 1:length(rs)
        if typeof(rs[i]) <: Rules
            push!(rsa, Rule_to_PRule(rs[i]))
        else
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
    (gotmatch,cap) = cmppat(expr,just_pattern(pat))
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

# Probably faster to handle this in
# canonicalization code. Some is done there. Some incorrectly.
function apprules(mx::Mxpr{:Power})
    res = do_Power(mx,mx[1],mx[2])
    res
end

# Don't handle this yet.
do_Power{T<:Integer,V<:Integer}(mx::Mxpr{:Power},   b::Complex{T},expt::Rational{V}) = mx
do_Power{T<:Integer,V<:Integer}(mx::Mxpr{:Power},   b::Complex{T},expt::Complex{Rational{V}}) = mx

do_Power{T<:Number,V<:Number}(mx::Mxpr{:Power},   b::T,expt::V) = mpow(b,expt)

do_Power{T<:Integer, V<:Symbolic}(mx::Mxpr{:Power}   ,b::V,n::T) = n == 1 ? b : n == 0 ? one(n) : mx
do_Power{T<:Integer}(mx::Mxpr{:Power},   b::Mxpr{:Power},exp::T) = mpow(base(b), mmul(exp,expt(b)))

do_Power{T<:Real}(mx::Mxpr{:Power},   b::Mxpr{:Power},exp::T) = mpow(base(b), mmul(exp,expt(b)))

do_Power(mx::Mxpr{:Power},   b::Mxpr{:Power},exp) = is_Number(expt(b)) ? mpow(base(b), mmul(expt(b),exp)) : mx

do_Power{T<:AbstractFloat}(mx::Mxpr{:Power},   b::SJSym,expt::T) = b == :E ? exp(expt) : mx
do_Power{T<:AbstractFloat}(mx::Mxpr{:Power},   b::SJSym,expt::Complex{T}) = b == :E ? exp(expt) : mx

do_Power{T<:Integer}(mx::Mxpr{:Power},   b::Mxpr{:DirectedInfinity},expt::T) = mpow(b,expt)
do_Power{T<:Number}(mx::Mxpr{:Power},   b::Mxpr{:DirectedInfinity},expt::T) = mpow(b,expt)

# Finish this sometime
# do_Power{T<:Real,V<:AbstractFloat}(mx::Mxpr{:Power}, b::T, expt::V)

# This conflicts with mpow in arithmetics.jl
#
# Check if the exact answer is an integer.
function do_Power{T<:Integer, V<:Rational}(mx::Mxpr{:Power},b::T,expt::V)
    mpow(b,expt)
end

function disabledo_Power{T<:Integer, V<:Rational}(mx::Mxpr{:Power},b::T,exp::V)
    gotneg::Bool = false
    b == -1 && return mx
    if b < 0
        gotneg = true
        b *= -1
    end
    res = b^exp
    ires = round(T,res)
    nrat = Rational(den(exp),num(exp))
    if ires^nrat == b
        if gotneg
            return ires == 1 ? mxprcf(:Power, -1, exp) : mxprcf(:Times, ires, mxprcf(:Power, -1, exp))
        else
            return ires
        end
    else
        return mx
    end
end

do_Power(mx,b,e) = mx

#### Abs

@sjdoc Abs "
Abs(z) represents the absolute value of z.
"

function apprules(mx::Mxpr{:Abs})
    doabs(mx,mx[1])
end

doabs{T<:Number}(mx,n::T) = mabs(n)

# Abs(x^n) --> Abs(x)^n  for Real n
function doabs(mx,pow::Mxpr{:Power})
    doabs_pow(mx,base(pow),expt(pow))
end
doabs_pow(mx,b,e) = mx
doabs_pow{T<:Real}(mx,b,e::T) = mxpr(:Power,mxpr(:Abs,b),e)


doabs(mx,prod::Mxpr{:Times}) = doabs(mx,prod,prod[1])

#doabs(mx,prod,s::Symbol)

function doabs{T<:Number}(mx,prod,f::T)
    f >=0 && return mx
    if f == -1
        return doabsmone(mx,prod,f)
    end
    args = copy(margs(prod))
    args[1] = -args[1]
    return mxpr(:Abs,mxpr(:Times,args))
end

function doabsmone{T<:Integer}(mx,prod,f::T)
    args = copy(margs(prod))
    shift!(args)
    if length(args) == 1
        return mxpr(:Abs,args)
    else
        return mxpr(:Abs,mxpr(:Times,args))
    end
end

# TODO Fix canonical routines so that 1.0 * a is not simplifed to a
function doabsmone{T<:Real}(mx,prod,f::T)
    args = copy(margs(prod))
    shift!(args)
    if length(args) == 1
        res = mxpr(:Times,one(f),mxpr(:Abs,args))
    else
        res = mxpr(:Times,one(f),mxpr(:Abs,mxpr(:Times,args)))
    end
    return res
end

doabs(mx,x) = mx


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

# This appears to be done by canonicalizer. So it is a waste of time.
# function apprules(mx::Mxpr{:Plus})
#     if length(mx) == 2
#         doplus(mx,mx[1],mx[2])
#     else
#         mx
#     end
# end
#doplus(mx,a::Number,b::Number) = mplus(a,b)
#doplus(mx,b,e) = mx

apprules(mx::Mxpr{:Minus}) = is_Number(mx[1]) ? -mx[1] : -1 * mx[1]

#### Tracing evaluation

@sjdoc TraceOn "
TraceOn() turns on the tracing of SJulia evaluation.
"
@sjdoc TraceOff "
TraceOff() turns off the tracing of SJulia evaluation.
"
@sjseealso_group(TraceOn,TraceOff)
apprules(mx::Mxpr{:TraceOn}) = (set_meval_trace() ; nothing)
apprules(mx::Mxpr{:TraceOff}) = (unset_meval_trace() ; nothing)

#### Timing evaluation

@sjdoc Timing "
Timing(expr) evaluates expr and returns a list of the elapsed CPU time
and the result.
"

@sjseealso_group(Timing,Allocated,TimeOn,TimeOff,TrDownOn,TrDownOff,TrUpOn,TrUpOff)
function apprules(mxt::Mxpr{:Timing})
    t = @elapsed begin
        reset_meval_count()
        mx = doeval(mxt[1])
        setsymval(:ans,mx)
    end
    mxpr(:List,t,mx)
end

#### TimeOn and TimeOff

@sjdoc TimeOn "
TimeOn() enables printing CPU time consumed and memory allocated
after each evaluation of command line input.
"

@sjdoc TimeOff "
TimeOff() disables printing CPU time consumed and memory allocated
after each evaluation of command line input.
"

@mkapprule TimeOn  :nargs => 0

do_TimeOn(mx::Mxpr{:TimeOn}) = (set_timing() ; nothing)

@mkapprule TimeOff  :nargs => 0

do_TimeOff(mx::Mxpr{:TimeOff}) = (unset_timing(); nothing)

#### Tracing Evaluation

@sjdoc TrDownOn "
TrDownOn() enables tracing attempted applications of DownRules.
"

@sjdoc TrDownOff "
TrDownOff() disables tracing attempted applications of DownRules.
"

@sjdoc TrUpOn "
TrUpOn() enables tracing attempted applications of UpRules.
"

@sjdoc TrUpOff "
TrUpOff() disables tracing attempted applications of UpRules.
"

apprules(mx::Mxpr{:TrUpOn}) = (set_up_trace() ; nothing)
apprules(mx::Mxpr{:TrUpOff}) = (unset_up_trace(); nothing)

apprules(mx::Mxpr{:TrDownOn}) = (set_down_trace() ; nothing)
apprules(mx::Mxpr{:TrDownOff}) = (unset_down_trace(); nothing)

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

#### HAge, Fixed and UnFix

@sjdoc HAge "
HAge(s) returns the timestamp for the expression or symbol s.
Using this timestamp to avoid unnecessary evaluation is a partially
implemented feature.
"
@sjseealso_group(HAge,Age,Fixed,Syms,DirtyQ,Unfix)
# Get the last-altered timestamp of an expression or symbol
apprules(mx::Mxpr{:HAge}) = hdo_getage(mx,mx[1])
hdo_getage(mx,s::Symbol) = Int(symage(s))
hdo_getage(mx,s::Mxpr) = Int(getage(s))
#do_getage(mx,s::Mxpr) = do_getage(mx,meval(s))
hdo_getage(mx,x) = mx

apprules(mx::Mxpr{:Age}) = do_getage(mx,mx[1])
do_getage(mx,s::Symbol) = Int(symage(s))
do_getage(mx,s::Mxpr) = Int(getage(s))
#do_getage(mx,s::Mxpr) = do_getage(mx,meval(s))
do_getage(mx,x) = mx

@sjdoc Fixed "
Fixed(expr) returns the status of the fixed point bit, which tells whether expr
is expected to evaluate to itself in the current environment. This is partially
implemented.
"
# Get fixed-point bit. Idea is to set it if expr evaluates to itself.
#apprules(mx::Mxpr{:Fixed}) = is_fixed(symval(mx[1]))
apprules(mx::Mxpr{:Fixed}) = is_fixed(mx[1])

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
UserSyms() returns a List of non-Protected symbols, which is approximately
all user defined symbols.
"
apprules(mx::Mxpr{:UserSyms}) = usersymbols()

@sjdoc Help "
Help(sym) or Help(\"sym\") prints documentation for the symbol sym. Eg: Help(Expand).
Help() lists most of the documented symbols. Due to parsing restrictions at the repl, for some
topics, the input must be a string.

h\"topic\" gives a case-insensitive regular expression search.

In the REPL, hit TAB to see all the available completions.
\"?, topic\" is equivalent to Help(topic).

Help(All => True) prints all of the documentation.

Help(regex) prints a list of topics whose documentation text matches the
regular expression regex. For example Help(r\"Set\"i) lists all topics that
match \"Set\" case-independently.
"

apprules(mx::Mxpr{:Help}) = do_Help(mx,margs(mx)...)

@doap Help() = print_doc("Help")

function do_Help(mx::Mxpr{:Help})
    print_doc("Help")
end


function do_Help(mx::Mxpr{:Help}, r::Mxpr{:Rule})
    if r[1] == :All && r[2] == true
        print_all_docs()
    end
    Null
end

do_Help(mx::Mxpr{:Help},args...) =  print_doc(args...)

function do_Help{T<:Regex}(mx::Mxpr{:Help},r::T)
    print_matching_topics(r)
end


# function do_Help(mx,args...)
#     if length(mx) > 0 && mx[1] == mxpr(:RuleDelayed, :All,true)
#         print_all_docs()
#     else
#         if length(margs(mx)) == 0

#         end
#         print_doc(margs(mx)...)
#     end
# end


#### Module

@sjdoc Module "
Module creates a lexical scope block for variables. Warning, this is broken
in the sense that nested calls to a Module are not supported.
"
@sjexamp( Module,
         ("ClearAll(f,a)",""),
         ("f(x_) := Module([a],(a=1, a+x))","","This module has local variable 'a'"),
         ("f(3)","4"),
         ("a","a","The global variable 'a' is not affected."))


@mkapprule Module  :nargs => 1:2

do_Module(mx::Mxpr{:Module}, vars::Mxpr{:List}, body::Mxpr{:CompoundExpression}) = localize_module!(mx)

do_Module(mx::Mxpr{:Module}, vars::Mxpr{:List}, body) = localize_module!(mxprcf(:Module,vars,mxprcf(:CompoundExpression, body)))

# localizing is done above during setting the rule.
# LModule is "localized module"
# This is a quick way to implement Modules
# The localization happens when they are set and they are
# transformed into LModules. The LModule is evaluated here
# and local syms are removed afterwards.
#
# TODO: Its probably better to have and apprule for Module which
# does the conversion to LModule, this is more robust than doing
# it during Set and SetDelay... and then later, an even better
# implementation.
function apprules(mx::Mxpr{:LModule})
    body = mx[1]
    vars = margs(body[1])
    res = doeval(body)
    for v in vars
        delete_sym(v)
    end
    if  is_Mxpr(res,:Return) # TODO: check somewhere for excess args
        return length(res) == 0 ? Null : res[1]
    end
    return res
end

## ExpandA, only a bit is implemented

@sjdoc ExpandA "
ExpandA(expr) expands products in expr. This is only partially implemented,
mostly to test the efficiency of evaluation and evaluation control. The
sympy version Expand() is more capable, but slower.
"
apprules(mx::Mxpr{:ExpandA}) = _doexpand(mx[1])


@mkapprule RandomReal

do_RandomReal(mx::Mxpr{:RandomReal}) = return rand()

@mkapprule Random

do_Random(mx::Mxpr{:Random}) = rand()

function do_Random(mx::Mxpr{:Random}, sym::Symbol)
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


@mkapprule Counts

@sjdoc Counts "
Counts(list) returns a dictionary of the number of times each distinct element of list
occurs.
"

# Broken at the moment
# import DataStructures: OrderedDict

# We should use an ordered dict
function do_Counts(mx::Mxpr{:Counts}, list::Mxpr{:List})
#    d = OrderedDict{Any,Any}()  # We need to canonicalize this
    d = Dict{Any,Any}()  # We need to canonicalize this
    for el in margs(list)
        val = get!(d,el,0)
        d[el] = val + 1
    end
    d
end
