##  Some 'apprules' definitions.
# These evaluate expressions with builtin (protected) heads.
# They are called from meval.

# Would be nice to get something like this working to get
# rid of boiler plate or allow changing how dispatch is done
# And checking number of args.
#macro apphead(sym,body...)
#    name = :apprules
#    :(function $(esc(name))(mx::Mxpr{$sym}) $body end)
#end

## Application of Rules for many heads of Mxpr

## Head with no builtin or user defined evaluation code.
# That is, no user defined Julia level code. There may be SJulia rules for x.
apprules(x) = x

#### Set and SetDelayed

function checkprotect(s::SJSym)
    get_attribute(symname(s),:Protected) &&
    error("Symbol '",symname(s), "' is protected.")
end
checkprotect(mx::Mxpr) = checkprotect(mhead(mx))

@sjdoc Set "
Set(a,b), a = b
Sets the value of a to b. b is evaluated only once, when `a=b' is evaluated.
"

@sjseealso_group( Set, SetDelayed )

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
function apprules(mx::Mxpr{:Set})
    set_only(mx,mx[1],mx[2])
end

function apprules(mx::Mxpr{:SetDelayed})
    setdelayed(mx,margs(mx,1),margs(mx,2))
end

# getsym(symname(lhs)) is because a copy of symbol is being made somewhere
# so we look up the original in the table
# SetDelayed is not correct. rhs is also evaluated because
# lhs is evaluated twice in order to find fixed point.
# we have treat this specially somehow, not ordinary evaluation.
function setdelayed(mx,lhs::SJSym, rhs)
    checkprotect(lhs)
    setsymval(lhs,rhs)
    nothing
end

function set_only(mx,lhs::SJSym, rhs)
    checkprotect(lhs)
    setsymval(lhs,rhs)
    rhs
end

# Create DownValue. "function" definition
# eg f(x_) := x  defines a DownValue for the SJSym f
function setdelayed(mx,lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    push_downvalue(mhead(lhs),rule) # push DownValue
    rule
    nothing
end

function set_only(mx,lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    push_downvalue(mhead(lhs),rule) # push DownValue
    rule
    nothing
end

# Optimize a bit. Localize variables once, not every time pattern is evaluated
setdelayed(mx,lhs::Mxpr, rhs::Mxpr{:Module}) = setdelayed(mx,lhs,localize_module(rhs))
set_only(mx,lhs::Mxpr, rhs::Mxpr{:Module}) = set_only(mx,lhs,localize_module(rhs))

# We renamed stuff and the Module code above calls the old things. We need to fix this.
set_and_setdelayed(mx,y,z) = mx

#### SetJ

@sjdoc SetJ "
SetJ(x,val) sets the Julia symbol x to val. Variables and functions in SJulia
are separate from those in Julia, ie, their table of bindings to symbols are separate.
"

# This can also be done with a Jxpr
# Bind a Julia symbol to the rhs
function apprules(mx::Mxpr{:SetJ})
    lhs = margs(mx,1)
    rhs = margs(mx,2)
    eval(Expr(:(=),symname(lhs),rhs))
end


#### Jxpr

@sjdoc Jxpr "
Jxpr allows embedding Julia expressions.
A Jxpr is entered like this :( expr ) . expr is interpreted as a Julia expression and
it is wrapped expression with head Jxpr, which is then evaluated when
Jxpr is evaluated. You never see the head Jxpr. For example
 m = :( [1:10] )  creates a Julia array and binds it to the SJulia symbol m
"

@sjexamp( Jxpr,
         "This creates a Julia Array{Int,1} and \"binds\" it to the SJulia symbol m.",
         ("m = :( [1:3] )",
          "3-element Array{Int64,1}:\n 1\n 2\n 3"))

# quote, i.e. :( expr ) is parsed as a Julia expression and is wrapped as
# Mxpr with head Jxpr. It is evaluated here.
# Eg.  m = :( [1:10] )  creates a Julia array and assigns to SJulia symbol m
apprules(mx::Mxpr{:Jxpr}) = eval(mx[1])

#### Unpack

@sjdoc Unpack "
Unpack(a) unpacks a Julia typed array into an SJulia List expression.
Only 1-d is supported.
"

@sjexamp( Unpack,
         "This creates a List of three random Float64's.",
         ("Unpack( :(rand(3)) )", "[0.5548766917324894,0.034964001133465095,0.9122052258982192]"))

function apprules(mx::Mxpr{:Unpack})
    obj = mx[1]
    args = do_unpack(obj)
    mx = mxpr(:List,args)
    setfixed(mx)
    setcanon(mx)
    return mx
end

function do_unpack(obj)
    args = newargs(length(obj))
    @inbounds for i in 1:length(obj)
        args[i] = obj[i]
    end
    return args
end

#### Pack

@sjdoc Pack "
Pack(mx) packs the args of the SJulia expression mx into a typed Julia array.
The type of the array is the same as the first element in mx.
"

@sjexamp( Pack,
         "This returns a Julia array of element type Int [1,2,3].",
         ("ClearAll(f)",""),
         ("Pack(f(1,2,3))","3-element Array{Int64,1}: [1,2,3]"))

@sjseealso_group(Pack,Unpack)

# 1-d unpack
function apprules(mx::Mxpr{:Pack})
    sjobj = margs(margs(mx,1))
    T = typeof(sjobj[1]) # hope one exists
    args = do_pack(T,sjobj)
    return args
end

function do_pack(T,sjobj)
    args = Array(T,length(sjobj))
    @inbounds for i in 1:length(sjobj)
        args[i] = sjobj[i]
    end
    return args
end


#### Symbol

@sjdoc Symbol "
Symbol(str) converts the string str to a symbol. For example if a is 1,
then Symbol(\"a\") returns 1.
"

function apprules(mx::Mxpr{:Symbol})
    dosymbol(mx,mx[1])
end
dosymbol(mx,s::String) = getsym(symbol(s))
dosymbol(mx,x) = error("Symbol: expected a string")

@sjdoc Clear "
Clear(x,y,z) removes the values associated with x,y,z. It does not remove
their DownValues.
"

@sjseealso_group(Clear, ClearAll)

# 'Clear' a value. ie. set symbol's value to its name
function apprules(mx::Mxpr{:Clear})  # This will be threaded over anyway
    @inbounds for a in margs(mx)  # makes sense here ?
        checkprotect(a)
        setsymval(a,symname(a))
    end
end

@sjdoc ClearAll "
ClearAll(x,y,z) removes all values and DownValues associated with x,y,z.
"

# Remove all values associate with SJSym. values and DownValues
function apprules(mx::Mxpr{:ClearAll})  # already threaded
    for a in margs(mx)
        checkprotect(a)
        delete!(SYMTAB,a)
#        setsymval(a,symname(a))
#        clear_downvalues(a)
    end
end


@sjdoc Dump "
Dump(expr) prints an internal representation of expr. This is similar to
Julia `dump'.
"

@sjdoc DumpHold "
DumpHold(expr) prints an internal representation of expr. This is similar to
Julia `dump'. In constrast to `Dump', expr is not evaluated before it's internal
representation is printed.
"

@sjseealso_group(Dump,DumpHold)

# DumpHold does not evaluate args before dumping
apprules(mx::Union(Mxpr{:Dump},Mxpr{:DumpHold})) = for a in margs(mx) is_SJSym(a) ? dump(getssym(a)) : dump(a) end

@sjdoc Length "
Length(expr) prints the length of SJulia expressions and Julia objects. For
SJulia expressions, the length is the number or arguments. For scalar Julia
types, the length is zero. For Array's and Dict's the length is the same as
Julia `length'.
"

apprules(mx::Mxpr{:Length}) = symjlength(margs(mx,1))
symjlength(mx::Mxpr) = length(margs(mx))
symjlength(x) = length(x)


@sjdoc Part "
Part(expr,n) or expr[n], returns the nth element of expression expr.
expr[n1][n2] returns the n2th part of the n1th part. To assign, you
must use SetPart. expr[n] also returns the nth element of instances of several
Julia types such as Array and Dict.
"

# Get part of expression. Julia :ref is mapped to :Part
# This won't work for setting a part.
# You must nest this to go down more than one level.
# a[i] parses to Part(a,i), and a[i][j] to Part(Part(a,i),j)
function apprules(mx::Mxpr{:Part})
    a = margs(mx)
    arr = a[1]
    i = a[2]
    i = i < 0 ? length(arr)+i+1 : i
    arr[i]
end

@sjdoc SetPart "
SetPart(expr,n,val) sets the nth part of expr to val. Only one level of depth is supported
at the moment.
"

@sjseealso_group(Set,SetPart)

## crude implementation.
# We don't have syntax to set a part yet.
# This only works at one level.
function apprules(mx::Mxpr{:SetPart})
    a = margs(mx)
    x = a[1]
    ind = a[2]
    val = a[3]
    x[ind] = val
end

@sjdoc Head "
Head(expr) returns the head of expr, which may be an SJulia expression or object of any
Julia type. The head of a Julia expression is Expr, eg.
Head( :( :( a = 1) )) returns Expr. Note we have to quote twice, because one level of
a quoted Julia expression is evaluated so that we can embed Julia code.
"

apprules(mx::Mxpr{:Head}) = gethead(margs(mx,1))
gethead(mx::Mxpr) = mhead(mx)
gethead(s::SJSym) = getsym(:Symbol)
#gethead(s::Symbol) = getsym(:JuliaSymbol)  # out dated
gethead(ex) = typeof(ex)

@sjdoc JVar "
JVar(x) returns the value of the Julia identifier x. This is
identical to :(x).
"
@sjseealso_group(Jxpr,JVar)
apprules(mx::Mxpr{:JVar}) = eval(symname(margs(mx,1)))

@sjdoc AtomQ "
AtomQ(expr), in principle, returns true if expr has no parts accesible with Part.
However, currently, Julia Arrays can be accessed with Part, and return true under AtomQ.
"
apprules(mx::Mxpr{:AtomQ}) = atomq(mx[1])

@sjdoc Attributes "
Attributes(s) returns attributes associated with symbol s. Builtin symbols have
the attribute Protected, and may have others.
"
apprules(mx::Mxpr{:Attributes}) = get_attributes(margs(mx,1))

@sjdoc DownValues "
DownValues(s) returns a List of DownValues associated with symbol s. These are values
that are typically set with the declarative \"function definition\".
"

@sjexamp( DownValues,
         ("ClearAll(f)",""),
         ("f(x_) := x^2",""),
         ("DownValues(f)", "[HoldPattern(f(x_))->(x^2)]"))
apprules(mx::Mxpr{:DownValues}) = listdownvalues(margs(mx,1))

@sjdoc Example "
Example(s) runs (evaluates) the first example for the symbol s, which is typically
a BuiltIn symbol. The input, output, and comments are displayed. The input strings
for the example are pushed onto the terminal history so they can be retrieved and 
edited and re-evaluated. Example(s,n) runs the nth example for symbol s. When viewing
documentation strings via ? SomeHead, the examples are printed along with the
documentation string, but are not evaluated.
"
function apprules(mx::Mxpr{:Example})
    if length(mx) == 1
        do_example_n(mx[1],1)
    else
        do_example_n(mx[1],mx[2])
    end
end

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
doreplace(mx,expr,r::Mxpr{:Rule}) = replace(expr,Rule_to_PRule(r))
doreplace(mx,a,b) = mx

@sjdoc ReplaceAll "
ReplaceAll(expr,rule) replaces parts at all levels in expr according to Rule rule.
"
apprules(mx::Mxpr{:ReplaceAll}) = doreplaceall(mx,mx[1],mx[2])
doreplaceall(mx,expr,r::Mxpr{:Rule}) = replaceall(expr,Rule_to_PRule(r))
doreplaceall(mx,a,b) = mx

@sjdoc MatchQ "
MatchQ(expr,pattern) returns true if expr matches pattern.
"
@sjexamp( MatchQ,
         ("MatchQ( 1, _Integer)", "true"),
         ("ClearAll(gg,xx,b)",""),
         ("MatchQ( gg(xx) , _gg)", "true"),
         ("MatchQ( b^2, _^2)","true"))
function apprules(mx::Mxpr{:MatchQ})
    (gotmatch,cap) = cmppat(mx[1],just_pattern(mx[2]))
    gotmatch
end

@sjdoc FullForm "
FullForm(expr) prints expr and all sub-expressions as
Head(arg1,arg2,...). Normal output may use infix notation instead.
"
@sjexamp( FullForm,
         ("Clear(a,b)",""),
         ("a+b","a+b"),
         ("FullForm(a+b)","Plus(a,b)"))
# FullForm is handled in io.jl

## Comparison

@sjdoc Comparison "
Comparison(expr1,c1,expr2,c2,expr3,...) performs or represents a
chain of comparisons. Comparison expressions are usually input and
displayed using infix notation.
"
@sjexamp(Comparison,
         ("Clear(a,b,c)",""),
         ("a == a","true"),
         ("a == b","false"),
         ("a < b <= c","a < b <= c"),
         ("(a=1,b=2,c=2)","2"),
         ("a < b <= c","true"))
# We do this the Julia- and mma4max way, not the Mma way.
function apprules(mx::Mxpr{:Comparison})
    nargs1 = newargs()
    i = 1
    while i <= length(mx)  # do all the != and ==
        if is_SJSym(mx[i])
            if symname(mx[i]) == :(==)
                if mx[i-1] == mx[i+1]
                    i += 1
                else
                    return false
                end
            elseif symname(mx[i]) == :(!=)
                if mx[i-1] != mx[i+1]
                    i += 1
                else
                    return false
                end
            else
                push!(nargs1,mx[i])
            end
        else
            push!(nargs1,mx[i])
        end
        i += 1
    end
    length(nargs1) == 1  && return true
    nargs = newargs()    
    for x in nargs1   # Do numeric inequalities
        if is_Number(x)
            push!(nargs,x)
        elseif is_comparison_symbol(x)
            push!(nargs,symname(x))
        else
            return mx
        end
    end
    eval(Expr(:comparison,nargs...))
end

## A few Number rules

# These may not all be necessary.

apprules(mx::Mxpr{://}) = makerat(mx,mx[1],mx[2])
makerat{T<:Number}(mx::Mxpr{://},n::T,d::T) = n//d
makerat(mx,n,d) = mx
apprules(mx::Mxpr{:complex}) = makecomplex(mx,mx[1],mx[2])
makecomplex(mx::Mxpr{:complex},n::Real,d::Real) = complex(n,d)
makecomplex(mx,n,d) = mx

# Probably faster to handle this in
# canonicalization code
function apprules(mx::Mxpr{:Power})
    dopower(mx,mx[1],mx[2])
end
    
dopower(mx::Mxpr{:Power},b::Number,e::Number) = mpow(b,e)
dopower(mx::Mxpr{:Power},b::Symbolic,n::Integer) = n == 1 ? b : n == 0 ? one(n) : mx
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp::Integer) = mpow(base(b), (exp*expt(b)))
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp::Real) = mpow(base(b), (exp*expt(b)))
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp) = is_Number(expt(b)) ? mpow(base(b), (expt(b)*exp)) : mx
dopower(mx,b,e) = mx

## Not sure where to put this kind of thing. Make reading code easier.
Base.base(p::Mxpr{:Power}) = p[1]
expt(p::Mxpr{:Power}) = p[2]

## convert to BigInt or BigFloat. We cannot yet do this automatically
@sjdoc BI "
BI(n) converts the number n to a BigInt. SJulia currently neither
detects integer overflow, nor automatically promote integers to BigInts.
"
@sjseealso_group(BI,BF)
@sjdoc BF "
BF(n) converts the number n to a BigFloat. SJulia currently neither
detects overflow, nor automatically promotes types from fixed to arbitrary precision.
"
apprules(mx::Mxpr{:BI}) = dobigint(mx,mx[1])
dobigint(mx,x) = mx
dobigint{T<:Number}(mx,x::T) = BigInt(x)
apprules(mx::Mxpr{:BF}) = dobigfloat(mx,mx[1])
dobigfloat(mx,x) = mx
dobigfloat{T<:Number}(mx,x::T) = BigFloat(x)

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

## Tracing evaluation

@sjdoc TraceOn "
TraceOn() turns on the tracing of SJulia evaluation.
"
@sjdoc TraceOff "
TraceOff() turns off the tracing of SJulia evaluation.
"
@sjseealso_group(TraceOn,TraceOff)
apprules(mx::Mxpr{:TraceOn}) = (set_meval_trace() ; nothing)
apprules(mx::Mxpr{:TraceOff}) = (unset_meval_trace() ; nothing)

@sjdoc Timing "
Timing(expr) evaluates expr and returns a list of the elapsed CPU time
and the result.
"
@sjseealso_group(Timing,Allocated,TimeOn,TimeOff)
function apprules(mxt::Mxpr{:Timing})
    t = @elapsed begin
        reset_meval_count()
        mx = doeval(mxt[1])
        setsymval(:ans,mx)
    end
    mxpr(:List,t,mx)
end

@sjdoc TimeOn "
TimeOn() enables printing CPU time consumed and memory allocated
after each evaluation of command line input.
"

@sjdoc TimeOff "
TimeOff() disables printing CPU time consumed and memory allocated
after each evaluation of command line input.
"
apprules(mx::Mxpr{:TimeOn}) = (MEVAL.timingon = true ; nothing)
apprules(mx::Mxpr{:TimeOff}) = (MEVAL.timingon = false; nothing)

# This does not work. Does not report correct time and allocation
# We have to do Allocted and Timing separately
function apprules(mxt::Mxpr{:Timing2})
    begin
        reset_meval_count()
        mx = @time(doeval(mxt[1]))
        setsymval(:ans,mx)
    end
    mx
end

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

@sjdoc CompoundExpression "
CompoundExpression(expr1,expr2,...) or (expr1,expr2,...) evaluates each expression in turn and
returns the result of only the final evaluation.
"
function apprules(mx::Mxpr{:CompoundExpression})
    local res
        @inbounds for i in 1:length(mx)
            res = doeval(mx[i])
        end
    res
end

@sjdoc Age "
Age(s) returns the timestamp for the expression or symbol s.
Using this timestamp to avoid unnecessary evaluation is a very partially
implemented feature.
"
@sjseealso_group(Age,Fixed,Syms,DirtyQ)
# Get the last-altered timestamp of an expression or symbol
apprules(mx::Mxpr{:Age}) = do_getage(mx,mx[1])
do_getage(mx,s::Symbol) = int(symage(s))
do_getage(mx,s::Mxpr) = int(getage(symval(s)))
do_getage(mx,x) = mx

@sjdoc Fixed "
Fixed(expr) returns the status of the fixed point bit, which tells whether expr
is expected to evaluate to itself in the current environment. This is very partially
implemented. For instance it works with Expand((a+b)^10).
"
# Get fixed-point bit. Idea is to set it if expr evaluates to itself.
#apprules(mx::Mxpr{:Fixed}) = is_fixed(symval(mx[1]))
apprules(mx::Mxpr{:Fixed}) = is_fixed(mx[1])

@sjdoc Syms "
Syms(m) returns a List of the symbols that the expression m 'depends' on. The
list is wrapped in HoldForm in order to prevent evaluation of the symbols.
"

# Syms has HoldAll
function apprules(mx::Mxpr{:Syms})
    mxpr(:HoldForm,do_syms(mx[1]))
end

@sjdoc DirtyQ "
DirtyQ(m) returns true if the timestamp of any symbol that m depends on
is more recent than the timestamp of m. This is for diagnostics.
"
apprules(mx::Mxpr{:DirtyQ}) = checkdirtysyms(mx[1])
do_syms(mx::Mxpr) = mxpr(:List,listsyms(mx)...)
do_syms(s) = mxpr(:List,)

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

@sjdoc EvenQ "
EvenQ(expr) returns true if expr is an even integer.
"
@sjdoc OddQ "
OddQ(expr) returns true if expr is an odd integer.
"

@sjseealso_group(AtomQ,EvenQ,OddQ)
apprules(mx::Mxpr{:EvenQ}) = is_type_less(mx[1],Integer) && iseven(mx[1])
apprules(mx::Mxpr{:OddQ}) = is_type_less(mx[1],Integer) &&  ! iseven(mx[1])

@sjdoc StringLength "
StringLength(s) returns the length of the string s.
"
apprules(mx::Mxpr{:StringLength}) = length(mx[1])

@sjdoc Module "
Module creates a lexical scope block for variables. Warning, this is broken
in the sense that nested calls to a Module are not supported.
"
@sjexamp( Module,
         ("ClearAll(f,a)",""),
         ("f(x_) := Module([a],(a=1, a+x))",""),
         ("f(3)","4"),
         ("a","a"))
apprules(mx::Mxpr{:Module}) = localize_module(mx)

@sjdoc Println "
Println(expr1,expr2,...) prints the expressions and a newline.
"
apprules(mx::Mxpr{:Println}) = println(margs(mx)...)

## Expand, only a bit is implemented

@sjdoc Expand "
Expand(expr) expands products in expr. This is only partially implemented,
mostly to test the efficiency of evaluation and evaluation control.
"
apprules(mx::Mxpr{:Expand}) = doexpand(mx[1])

# Only some of Range implemented for testing other things.
# From the command line, all the time evaluating Range(n) for n>10000 or so
# is spent in the inner loop here. Mma v3 is about 4-6 times or so faster.
# So this seems to be purely a Julia vs. low-level Mma difference.
# This is used to test summing numbers.
# Maxima is faster than Mma v3 at Apply(Plus,l) , where l is a big list of numbers
# Maxima is about 10x faster than this code (SJulia).

@sjdoc Range "
Range(n) returns the List of integers from 1 through n.
Range(n1,n2) returns the List of integers from n1 through n2.
Range(n1,n2,di) returns the List of integers from n1 through n2 in steps of di
di may be negative. Range is only partially implemented. Eg, floats and symbols
are not supported. However, you can get other SJulia lists, eg. floats, by
using Unpack(:([1.0:10^5])). This uses emebedded Julia to create a typed Array
and then unpacks it to a List.
"
function apprules(mx::Mxpr{:Range})
    if length(mx) == 1
        n = mx[1]
        args = range_args1(n) # use function for optimization on type
    elseif length(mx) == 2
        n0 = mx[1] - 1
        n = mx[2]
        args = range_args2(n0,n)
    elseif length(mx) == 3
        n0 = mx[1]
        n = mx[2]
        di = mx[3]
        off = n > n0 ? 1 : -1
        args = range_args3(n0,n,di,off)
    else
        return mx
    end
    r = mxpr(:List,args)
    setfixed(r)
    setcanon(r)
    mergesyms(r,:nothing)
    return r    
end

# separate functions are *essential* for type stability and efficiency.
function range_args1{T<:Integer}(n::T)
    args = newargs(n);
    @inbounds for i in one(n):n
        args[i] = i
    end
    return args
end

function range_args1{T<:FloatingPoint}(n::T)
    ni = floor(Int,n)
    args = newargs(ni);
    @inbounds for i in 1:ni
        args[i] = convert(T,i)
    end
    return args
end

function range_args2(n0,n)
    nd = n - n0
    args = newargs(nd);
    @inbounds for i in one(n0):nd
        args[i] = i+n0
    end
    return args
end

function range_args3(n0,n,di,off)
    args = newargs(div(n-n0+off,di));
    len = length(args) # cheap
    s = n0
    @inbounds for i in one(n0):len
        args[i] = s
        s += di
    end
    args
end

## quickly hacked Table, just for testing other parts of evaluation

# Create lexical scope for Table.
# Replace symbol os with ns in ex
function replsym(ex,os,ns)
    if is_Mxpr(ex)
        args = margs(ex)
        @inbounds for i in 1:length(args)
            args[i] = replsym(args[i],os,ns)
        end
    end
    if ex == os
        return ns
    else
        return ex
    end
end

@sjdoc Table "
Table(expr,[i,imax]) returns a list of expr evaluated imax times with
i set successively to 1 through imax. Other Table features are not implemented.
"
# We only do Table(expr,[i,imax])
# Our Table is rather slow. Slower than Maxima makelist.
# Table(a(i),[i,10000]) is 2 to 3 x slower than makelist( i, i, 1, 10000)
# If we cheat and only do a single evaluation,
# and setfixed() then the speed is the same.
# But, the 3rd party 'table' for Maxima is 4 times faster than makelist
# in this example.
# Hmmm, well Mma 3.0 is pretty slow, too.
function apprules(mx::Mxpr{:Table})
    expr = mx[1]
    iter = mx[2]
    isym = gensym(string(iter[1]))
    imax = meval(iter[2])
#    issym = createssym(isym,Int)  ## Trying out Typed variable
    ex = replsym(deepcopy(expr),iter[1],isym) # takes no time, for simple expression
    args = do_table(imax,isym,ex) # creating a symbol is pretty slow
    mx1 = mxpr(:List,args) # takes no time
    mergesyms(mx1,:nothing) # not correct, but stops the merging
    setcanon(mx1)
    setfixed(mx1)
    return mx1
end

function do_table(imax::Int,isym,ex)
    args = newargs(imax)
#    dump(ex)
    @inbounds for i in 1:imax
        setsymval(isym,i) #  = i # very slow if field 'val' is Any. very fast if it is Int
#        v = i
        v = meval(ex)
#        v = getssym(ex).val
#        v = doeval(ex)  # this is extremely slow, even when ex is only a symbol
        args[i] = v
        setfixed(args[i])        
    end
    return args
end

# The speed penalty for Any instead of Int, is factor of 10^4.
# Setting isym.val is by far the slowest step in Table(i,[i,10^6])
# Much slower than Mma and SBCL
type NSYM
    val::Any
end

function testset()
    ss = NSYM(0)
    sum0 = 0
    for i in 1:10^6
        ss.val = i
        sum0 += ss.val
    end
    sum0
end
