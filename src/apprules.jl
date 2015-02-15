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
function apprules(mx::Mxpr{:Set})
    do_set(mx,mx[1],mx[2])
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

function do_set(mx,lhs::SJSym, rhs)
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

function do_set(mx::Mxpr{:Set},lhs::Mxpr{:Part}, rhs::Mxpr{:Module})
    error("$mx is unimplemented")
end

# Mma is not clear but seems to evaluate the first arg to the lhs (the expression
# whose part we want) exactly once. We should document what we do.
# We check is_Number several times, because we may have a Dict.
function do_set(mx::Mxpr{:Set},lhs::Mxpr{:Part}, rhs)
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

# we are assuming this is a "function" definition
function do_set(mx,lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    push_downvalue(mhead(lhs),rule) # push DownValue
    rule
    nothing
end

# Optimize a bit. Localize variables once, not every time pattern is evaluated
setdelayed(mx,lhs::Mxpr, rhs::Mxpr{:Module}) = setdelayed(mx,lhs,localize_module!(rhs))
do_set(mx,lhs::Mxpr, rhs::Mxpr{:Module}) = do_set(mx,lhs,localize_module!(rhs))

# We renamed stuff and the Module code above calls the old things. We need to fix this.
set_and_setdelayed(mx,y,z) = mx


#### UpSet

@sjdoc UpSet "
UPSet(a(g(x_)),b), or a(g(x_)) ^= b  associates the transformation rule with g.
"

function apprules(mx::Mxpr{:UpSet})
    upset(mx,margs(mx,1),margs(mx,2))
end

function upset(mx,lhs::Mxpr, rhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    for i in 1:length(lhs)
        m = lhs[i]
        if is_Mxpr(m) && warncheckprotect(m)
            push_upvalue(mhead(m),rule)
        end
    end
    return rhs
end


#### SetAttributes

apprules(mx::Mxpr{:SetAttributes}) = do_set_attributes(mx[1],mx[2])

function do_set_attributes(lhs::SJSym, rhs::SJSym)
    checkprotect(lhs)
    set_attribute(lhs,rhs)
    nothing
end


#### Unprotect

@sjdoc Unprotect "
Unprotect(z1,z2,...) removes the Protected attribute from the symbols z1, z2, ...
"

function apprules(mx::Mxpr{:Unprotect})
    for i in 1:length(mx)
        do_unprotect(mx,mx[i])
    end
end
do_unprotect(mx,a::Symbol) = (unset_attribute(a,:Protected) ; nothing)
do_unprotect(mx,a) = error("Can't unprotect $mx")

function do_set(mx,lhs::Mxpr, rhs)
    checkprotect(lhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
    push_downvalue(mhead(lhs),rule) # push DownValue
    rule
    nothing
end


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
function apprules(mx::Mxpr{:Jxpr})
    do_jxpr(mx,mx[1])
end

function do_jxpr(mx::Mxpr{:Jxpr}, ex::Union(Expr,Symbol))
    return eval(ex)
end

function do_jxpr(mx::Mxpr{:Jxpr}, x)
    error("Jxpr: Can't execute Julia code of type ", typeof(x))
end


#### Unpack

@sjdoc Unpack "
Unpack(a) unpacks a Julia typed array into an SJulia List expression.
Only 1-d is supported. If a is a Julia Dict, then a list of lists of
key,value pairs is returned.
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

function do_unpack(dict::Dict)
    args = newargs(length(dict))
    i = 0
    for (k,v) in dict
        i += 1
        args[i] = mxpr(:List,k,v)
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


@sjdoc Keys "
Keys(d) returns a list of the keys in Dict d
"
apprules(mx::Mxpr{:Keys}) = do_keys(mx,mx[1])
do_keys(mx,d::Dict) = mxpr(:List,collect(keys(d)))
do_keys(mx,x) = (warn("Can't return keys of $mx"); mx)

#### Symbol

@sjdoc Symbol "
Symbol(str) converts the string str to a symbol. For example if a is 1,
then Symbol(\"a\") returns 1.
"

function apprules(mx::Mxpr{:Symbol})
    dosymbol(mx,mx[1])
end
dosymbol(mx,s::String) = getsym(symbol(s))
dosymbol(mx,x) = (warn("Symbol: expected a string"); mx)

@sjdoc Clear "
Clear(x,y,z) removes the values associated with x,y,z. It does not remove
their DownValues.
"

@sjseealso_group(Clear, ClearAll)

# 'Clear' a value. ie. set symbol's value to its name
function apprules(mx::Mxpr{:Clear})  # This will be threaded over anyway
    @inbounds for a in margs(mx)  # no inbounds does not work here
        checkprotect(a)
        setsymval(a,symname(a))
    end
end

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
        removesym(a)
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

@sjdoc LeafCount "
LeafCount(expr) gives the number of indivisible (Part can't be taken) elements in expr.
This amounts to counting all the Heads and all of the arguments that are not of type Mxpr.
"
apprules(mx::Mxpr{:LeafCount}) = leaf_count(mx[1])


@sjdoc ByteCount "
ByteCount(expr) gives number of bytes in expr. Not everything is counted
correctly at the moment.
"
apprules(mx::Mxpr{:ByteCount}) = byte_count(mx[1])

@sjdoc Depth "
Depth(expr) gives the maximum number of indicies required to specify
any part of expr, plus 1.
"
apprules(mx::Mxpr{:Depth}) = depth(mx[1])


@sjdoc Part "
Part(expr,n) or expr[n], returns the nth element of expression expr.
Part(expr,n1,n2,...) or expr[n1,n2,...] returns a nested part.
The same can be acheived less efficiently with expr[n1][n2]...
expr[n] = val sets the nth part of expr to val. n and val are evaluated
normally. expr is evaluated once.
expr[n] also returns the nth element of instances of several
Julia types such as Array and Dict.
"

# Get part of expression. Julia :ref is mapped to :Part
# a[i] parses to Part(a,i), and a[i][j] to Part(Part(a,i),j)
# a[i,j] parses to Part(a,i,j).
function apprules(mx::Mxpr{:Part})
    texpr = expr(mx)
    tinds = inds(mx)
    for j in 1:length(tinds)
        ind::Int = tinds[j]
        ind = ind < 0 ? length(texpr)+ind+1 : ind
        texpr = ind == 0 ? mhead(texpr) : texpr[ind]
    end
    return texpr
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
JVar(x) returns the Julia value of the Symbol that x evaluates to. For example,
if a = 1 in Julia and b = a in SJulia, then JVar(b) evaluates to 1.
"

@sjseealso_group(Jxpr,JVar)
apprules(mx::Mxpr{:JVar}) = eval(symname(mx[1]))



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

function get_attributes(sj::SJSym)
    ks = sort!(collect(keys(symattr(sj))))
    mxpr(:List,ks...) # need to splat because array ks is not of type Any
end

#### DownValues

@sjdoc DownValues "
DownValues(s) returns a List of DownValues associated with symbol s. These are values
that are typically set with the declarative \"function definition\".
"

@sjexamp( DownValues,
         ("ClearAll(f)",""),
         ("f(x_) := x^2",""),
         ("DownValues(f)", "[HoldPattern(f(x_))->(x^2)]"))
apprules(mx::Mxpr{:DownValues}) = listdownvalues(margs(mx,1))


#### UpValues

@sjdoc UpValues "
UpValues(s) returns a List of UpValues associated with symbol s. These are values
that are typically set with UpSet.
"

# @sjexamp( UpValues,
#          ("ClearAll(f)",""),
#          ("f(x_) := x^2",""),
#          ("UpValues(f)", "[HoldPattern(f(x_))->(x^2)]"))
apprules(mx::Mxpr{:UpValues}) = listupvalues(margs(mx,1))

#### Example

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

apprules(mx::Mxpr{:ReplaceRepeated}) = doreplacerepeated(mx,mx[1],mx[2])
doreplacerepeated(mx,expr,r::Mxpr{:Rule}) = replacerepeated(expr,Rule_to_PRule(r))
doreplacerepeated(mx,a,b) = mx


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

# TODO: Make sure Rationals are simplified
dopower(mx::Mxpr{:Power},b::Number,e::Number) = mpow(b,e)
dopower(mx::Mxpr{:Power},b::Symbolic,n::Integer) = n == 1 ? b : n == 0 ? one(n) : mx
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp::Integer) = mpow(base(b), mmul(exp,expt(b)))
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp::Real) = mpow(base(b), mmul(exp,expt(b)))
dopower(mx::Mxpr{:Power},b::Mxpr{:Power},exp) = is_Number(expt(b)) ? mpow(base(b), mmul(expt(b),exp)) : mx
dopower(mx,b,e) = mx

@sjdoc Abs "
Abs(z) represents the absolute value of z.
"

function apprules(mx::Mxpr{:Abs})
    doabs(mx,mx[1])
end

doabs(mx,n::Number) = mabs(n)

# Abs(x^n) --> Abs(x)^n  for Real n
function doabs(mx,pow::Mxpr{:Power})
    doabs_pow(mx,base(pow),expt(pow))
end
doabs_pow(mx,b,e) = mx
doabs_pow(mx,b,e::Real) = mxpr(:Power,mxpr(:Abs,b),e)


doabs(mx,prod::Mxpr{:Times}) = doabs(mx,prod,prod[1])

#doabs(mx,prod,s::Symbol)

function doabs(mx,prod,f::Number)
    f >=0 && return mx
    if f == -1
        return doabsmone(mx,prod,f)
    end
    args = copy(margs(prod))
    args[1] = -args[1]
    return mxpr(:Abs,mxpr(:Times,args))
end

function doabsmone(mx,prod,f::Integer)
    args = copy(margs(prod))
    shift!(args)
    if length(args) == 1
        return mxpr(:Abs,args)
    else
        return mxpr(:Abs,mxpr(:Times,args))
    end
end

# TODO Fix canonical routines so that 1.0 * a is not simplifed to a
function doabsmone(mx,prod,f::Real)
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
"
@sjseealso_group(BI,BF)
@sjdoc BF "
BF(n) converts the number, or string n to a BigFloat. SJulia currently neither
detects overflow, nor automatically promotes types from fixed to arbitrary precision.
"
apprules(mx::Mxpr{:BI}) = dobigint(mx,mx[1])
dobigint(mx,x) = mx
dobigint{T<:Number}(mx,x::T) = BigInt(x)
dobigint{T<:AbstractString}(mx,x::T) = BigInt(x)
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

@sjdoc HAge "
HAge(s) returns the timestamp for the expression or symbol s.
Using this timestamp to avoid unnecessary evaluation is a partially
implemented feature.
"
@sjseealso_group(HAge,Age,Fixed,Syms,DirtyQ,Unfix)
# Get the last-altered timestamp of an expression or symbol
apprules(mx::Mxpr{:HAge}) = hdo_getage(mx,mx[1])
hdo_getage(mx,s::Symbol) = int(symage(s))
hdo_getage(mx,s::Mxpr) = int(getage(s))
#do_getage(mx,s::Mxpr) = do_getage(mx,meval(s))
hdo_getage(mx,x) = mx

apprules(mx::Mxpr{:Age}) = do_getage(mx,mx[1])
do_getage(mx,s::Symbol) = int(symage(s))
do_getage(mx,s::Mxpr) = int(getage(s))
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

@sjdoc Help "
Help(sym) prints documentation for the symbol sym. Eg: Help(Expand)
"

function apprules(mx::Mxpr{:Help})
    print_doc(margs(mx)...)
end


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
        removesym(v)
    end
    return res
end

@sjdoc Println "
Println(expr1,expr2,...) prints the expressions and a newline.
"
apprules(mx::Mxpr{:Println}) = println(margs(mx)...)

## Expand, only a bit is implemented

@sjdoc Expand "
Expand(expr) expands products in expr. This is only partially implemented,
mostly to test the efficiency of evaluation and evaluation control.
"
apprules(mx::Mxpr{:Expand}) = _doexpand(mx[1])

@sjdoc Range "
Range(n) returns the List of integers from 1 through n.
Range(n1,n2) returns the List of numbers from n1 through n2.
Range(n1,n2,di) returns the List of numbers from n1 through n2 in steps of di
di may be negative. Floats and some symbolic arguments are supported.
You can get also get SJulia lists like using Unpack(:([1.0:10^5])).
This uses emebedded Julia to create a typed Array and then unpacks it to a List.
"

function apprules(mx::Mxpr{:Range})
    iter = make_sjitera(margs(mx))
    args = do_range(iter)
    r = mxpr(:List,args)
    setfixed(r)
    setcanon(r)
    mergesyms(r,:nothing)
    return r
end

function do_range(iter::SJIterA1)  # iter is parameterized, so we hope type of n is inferred.
    n = iter.num_iters
    args = newargs(n);
    j = one(iter.imax)
    @inbounds for i in 1:n
        args[i] = j
        j += 1
    end
    return args    
end

# Fails for rationals. nd counting is wrong
function do_range{T<:Real,V<:Real}(iter::SJIterA2{T,V})
    nd = mplus(iter.imax,-iter.imin) + 1
    if nd > 1
#        args = newargs(abs(nd))
        args = newargs(iter.num_iters)
        for i in zero(iter.imin):nd-1
            args[i+1] = mplus(i,iter.imin)
        end
    else  # Mma does not allow this second branch: eg Range(5,1) implies di = -1
        nd = -nd + 2
        args = newargs(iters.num_iters)
        for i in zero(iter.imin):(nd - 1)
            args[i+1] = mplus(iter.imin, -i)
        end
    end
    return args
end

# symbolic types
# This is about as fast as Mma 3 (running on a somewhat slower cpu)
function do_range(iter::SJIterA2)
    args = newargs(iter.num_iters)
    imin = iter.imin
    args[1] = imin
    s = imin
    if is_Mxpr(imin,:Plus)
        if is_Number(imin[1])  # number is always first in canon order.
            b = imin[1]  # extract number
            r = imin[2:end]  # the rest of the sum
            for i in 2:iter.num_iters
                args[i] = mxpr(:Plus,b+i-1,r...) # only a little slower than Mma if disable gc
                setfixed(args[i])
            end
        else  # imin is a sum with no numbers, so we put a number in front
            sargs = margs(s)
            for i in 2:iter.num_iters
                args[i] = mxpr(:Plus,i-1,sargs...)
                setfixed(args[i])
            end
        end
    else
        for i in 2:iter.num_iters
            args[i] = mxpr(:Plus,i,s)
            setfixed(args[i])
        end
    end
    #  we don't handle counting down case.
    return args
end    

# seems to be little penalty for mplus instead of +
function do_range{T<:Real,V<:Real,W<:Real}(iter::SJIterA3{T,V,W})
    n = iter.num_iters
    args = newargs(n)
    j = iter.imin
    for i in 1:n
        args[i] = j
        j = mplus(j,iter.di)
    end
    return args
end

function do_range(iter::SJIterA3)
    args = newargs(iter.num_iters)
    imin = iter.imin
    args[1] = imin
    s = imin
    if is_Number(iter.di)
        if true
            if is_Mxpr(imin,:Plus)
                if is_Number(imin[1])  # number is always first in canon order.
                    b = imin[1]        # extract number
                    r = imin[2:end]    # the rest of the sum
                    for i in 2:iter.num_iters
                        b = b + iter.di
                        if b == 0  # more efficient to move this branch out
                            if length(r) == 1
                                args[i] = r[1]
                            else
                                args[i] = mxpr(:Plus,r...)
                            end
                        else
                            args[i] = mxpr(:Plus,b,r...)
                        end
                        setfixed(args[i])
                    end
                else  # imin is a sum with no numbers, so we put a number in front
                    sargs = margs(s)
                    j = zero(iter.di)
                    for i in 2:iter.num_iters
                        j += iter.di
                        args[i] = mxpr(:Plus,j,sargs...)
                        setfixed(args[i])
                    end
                end
            else # imin is not a sum, so just create one
                j = zero(iter.di)
                for i in 2:iter.num_iters
                    j += iter.di
                    args[i] = mxpr(:Plus,j,s)
                    setfixed(args[i])
                end
            end
        else  #  iter.di < 0
            error("unimplemented")
        end
    else # di is not a number
        error("unimplemented")
    end
    return args
end    

function apprules(mx::Mxpr{:OldRange})
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

# At commit 3478f83ee403238704ad6af81b42e1c7f4d07cc8
# Table(a(i),[i,10000]) takes bout 0.45--0.49 s, makelist reports 0.503.
# 3rd party Maxima table( z(i), [i, 1, 100000]) takes 0.1
# Mma 3, 0.05 s 

function apprules(mx::Mxpr{:Table})
    expr = mx[1]
    iter = mx[2]
    isym = get_localized_symbol(iter[1])
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

# These tests concern not Table per se, but the efficiency
# of evaluation, in particular assigning values to symbols.
# commit 3478f83ee403238704ad6af81b42e1c7f4d07cc8
# Testing Table(a(i),[i,10^5])
# changes                   Time
# normal                    0.49
# disable setsymval         0.35
# 
# commit 0d2332b17eb7e5c518a86b1b3044b691fddb5b87
# setsymval is now much faster because we changed data structure in SSJSym
# Testing Table(a(i),[i,10^5])
# normal                    0.36
# *sigh* Cannot reproduce the performance in the previous line. Even if
# I return to that commit. Time is closer to 0.5 than 0.35
# setsymval still seems to be an expensive operation

function do_table(imax::Int,isym,ex)
    args = newargs(imax)
    sisym = getssym(isym)
    @inbounds for i in 1:imax
        setsymval(sisym,i)
        v = meval(ex)
        args[i] = v
        setfixed(args[i])  # no difference ?
    end
    return args
end

# The speed penalty for val::Any instead of val::Int, is factor of 10^4.
# Setting isym.val is by far the slowest step in Table(i,[i,10^6])
# Much slower than Mma and SBCL
# If we us val::Array{Any,1}, the speed penalty is better by a couple
# orders of magnitude
type NSYM
#    val::Int
    val::Array{Any,1}
end

function testset()
    ss = NSYM(Any[0])
    sum0 = 0
    for i in 1:10^6
        ss.val[1] = i
        sum0 += ss.val[1]
    end
    sum0
end
