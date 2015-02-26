##  Some 'apprules' definitions.
# These evaluate expressions with builtin (protected) heads.
# They are called from meval.

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

#### Increment

@sjdoc Increment "
Increment(n) increments the value of n by 1 and returns the old value.
"

apprules(mx::Mxpr{:Increment}) = do_Increment(mx,margs(mx)...)
do_Increment(mx,x::SJSym) = do_Increment1(mx,x,symval(x))
function do_Increment1(mx,x,xval::Number)
    setsymval(x,mplus(xval,1))  # maybe + is ok here.
    return xval
end
function do_Increment1(mx,x,val)
    setsymval(x,doeval(mxpr(:Plus,val,1)))
    return val
end
do_Increment(mx,args...) = mx

#### TimesBy

@sjdoc TimesBy "
TimesBy(a,b), or a *= b, sets a to a * b and returns the new value. This is currently
faster than a = a * b for numbers.
"

apprules(mx::Mxpr{:TimesBy}) = do_TimesBy(mx,margs(mx)...)
do_TimesBy(mx,x::SJSym,val) = do_TimesBy1(mx,x,symval(x),val)
function do_TimesBy1(mx,x,xval::Number, val::Number)
    r = mmul(xval,val)
    setsymval(x,r)
    return r
end
function do_TimesBy1(mx,x,xval,val)
    setsymval(x,doeval(mxpr(:Set,x, mxpr(:Times,xval,val))))
    return symval(x)
end
do_TimesBy(mx,args...) = mx

#### AddTo

@sjdoc AddTo "
AddTo(a,b), or a += b, sets a to a + b and returns the new value. This is currently
faster than a = a + b for numbers.
"

apprules(mx::Mxpr{:AddTo}) = do_AddTo(mx,margs(mx)...)
do_AddTo(mx,x::SJSym,val) = do_AddTo1(mx,x,symval(x),val)
function do_AddTo1(mx,x,xval::Number, val::Number)
    r = mplus(xval,val)
    setsymval(x,r)
    return r
end
function do_AddTo1(mx,x,xval,val)
    setsymval(x,doeval(mxpr(:Set,x, mxpr(:Plus,xval,val))))
    return symval(x)
end
do_AddTo(mx,args...) = mx

#### UpSet

@sjdoc UpSet "
UpSet(a(g(x_)),b), or a(g(x_)) ^= b  associates the transformation rule with g.
"

function apprules(mx::Mxpr{:UpSet})
    upset(mx,margs(mx,1),margs(mx,2))
end

function upset(mx,lhs::Mxpr, rhs)
    rule = mxpr(:RuleDelayed,mxpr(:HoldPattern,lhs),rhs)
#    println("upset $mx, $lhs, $rhs")
    for i in 1:length(lhs)
        m = lhs[i]
#        println("  upset $m")
        if is_Mxpr(m) && warncheckprotect(m)
            push_upvalue(mhead(m),rule)
        elseif is_SJSym(m) && warncheckprotect(m)
            push_upvalue(m,rule)
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

#### Keys

@sjdoc Keys "
Keys(d) returns a list of the keys in Dict d
"
apprules(mx::Mxpr{:Keys}) = do_keys(mx,mx[1])
do_keys(mx,d::Dict) = mxpr(:List,collect(keys(d))...)
do_keys(mx,x) = (warn("Can't return keys of $x"); mx)

#### Values

@sjdoc Values "
Values(d) returns a list of the values in Dict d
"

# mkapprule("Values") ... broken
apprules(mx::Mxpr{:Values}) = do_values(mx,mx[1])
do_values(mx,d::Dict) = mxpr(:List,collect(values(d))...)
do_values(mx,x) = (warn("Can't return values of $mx"); mx)

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
A more accurate name is NodeCount.
"
apprules(mx::Mxpr{:LeafCount}) = leaf_count(mx[1])


@sjdoc ByteCount "
ByteCount(expr) gives number of bytes in expr.
"
apprules(mx::Mxpr{:ByteCount}) = byte_count(mx[1])

@sjdoc Depth "
Depth(expr) gives the maximum number of indicies required to specify
any part of expr, plus 1.
"
apprules(mx::Mxpr{:Depth}) = depth(mx[1])

## Part

@sjdoc Part "
Part(expr,n) or expr[n], returns the nth element of expression expr.
Part(expr,n1,n2,...) or expr[n1,n2,...] returns a nested part.
The same can be acheived less efficiently with expr[n1][n2]...
expr[n] = val sets the nth part of expr to val. n and val are evaluated
normally. expr is evaluated once.
expr[n] also returns the nth element of instances of several
Julia types such as Array, or the element with key 'n' for Dict's.
"

# Get part of expression. Julia :ref is mapped to :Part
# a[i] parses to Part(a,i), and a[i][j] to Part(Part(a,i),j)
# a[i,j] parses to Part(a,i,j).
function apprules(mx::Mxpr{:Part})
    do_Part(mx,margs(mx)...)
end

function do_Part(mx::Mxpr{:Part},texpr,tinds...)
    for j in 1:length(tinds)
        texpr = get_part_one_ind(texpr,tinds[j])
    end
    return texpr
end

function get_part_one_ind(texpr::Union(Mxpr,Array),ind::Integer)
#    ind::Int = tind
    ind = ind < 0 ? length(texpr)+ind+1 : ind
    texpr = ind == 0 ? mhead(texpr) : texpr[ind]
    return texpr
end

function get_part_one_ind(texpr::Dict,tind)
    return texpr[tind]  # Part 0 can't return "Dict" because it could be a key.
end

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

@sjdoc Span "
Span(a,b) or a:b represents elements a through b.
Span(a,b,c) or a:b:c represents elements a through b in steps of c.
expr(a:b) returns elements a through b of expr, with the same head as expr.
"

@sjdoc Head "
Head(expr) returns the head of expr, which may be an SJulia expression or object of any
Julia type. The head of a Julia expression is Expr, eg.
Head( :( :( a = 1) )) returns Expr. Note we have to quote twice, because one level of
a quoted Julia expression is evaluated so that we can embed Julia code.
"

apprules(mx::Mxpr{:Head}) = gethead(margs(mx,1))
gethead(mx::Mxpr) = mhead(mx)
gethead(s::SJSym) = getsym(:Symbol)
gethead(ex) = typeof(ex)


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
ReplaceAll(expr,List(rule1,rule2,...)) replaces parts at all levels in expr according to the
list or rules. If given explicitly, the rules must be given as List(...) rather than
[...] because of a parsing error.
"

apprules(mx::Mxpr{:ReplaceAll}) = doreplaceall(mx,mx[1],mx[2])
doreplaceall(mx,expr,r::Mxpr{:Rule}) = replaceall(expr,Rule_to_PRule(r))
function doreplaceall(mx,expr,rs::Mxpr{:List})
    rsa = Array(PRule,0)
    for i in 1:length(rs)
        push!(rsa, Rule_to_PRule(rs[i]))
    end
    replaceall(expr,rsa)
end
doreplaceall(mx,a,b) = mx

@sjexamp( ReplaceAll,
         ("ClearAll(zz,b,c)",""),
         ("zz = 10 * b^2 * (c+d)","zz = 10 * b^2 * (c+d)"),
         ("ReplaceAll(zz, List(c => 3,d => 2) )", "50*b^2"))

apprules(mx::Mxpr{:ReplaceRepeated}) = doreplacerepeated(mx,mx[1],mx[2])
# Doing the meval stuff below is a workaround for a bug.
# All the integers should be gone in the following:
# m1 = ExpandA((a+b)^3)  --> a^3 + 3*(a^2)*b + 3*a*(b^2) + b^3
# m2 = ReplaceRepeated(m1, x_Integer => 1)  --> a + 2*a*b + b
# Mma 3 does this correctly. We insert an expensive workaround.
function doreplacerepeated(mx,expr,r::Mxpr{:Rule})
    ex1 = replacerepeated(expr,Rule_to_PRule(r))
    if is_Mxpr(ex1)
        ex1 = meval_arguments(ex1)        # 
        ex1 = meval_apply_all_rules(ex1)    
        #    ex1 = meval(ex1)
        ex1 = replacerepeated(ex1,Rule_to_PRule(r))
    end
    return ex1
end
doreplacerepeated(mx,a,b) = mx


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

# for operator form of MatchQ
function do_GenHead(mx,head::Mxpr{:MatchQ})
    mxpr(mhead(head),copy(margs(mx))...,margs(head)...)
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
    do_Comparison(mx,margs(mx)...)
#    do_Comparison(mx)
end

function do_Comparison(mx::Mxpr{:Comparison},a::Number,comp::SJSym,b::Number)
    _do_Comparison(a,comp,b)
end

# Mma does this a == a != b  --->  a == a && a != b
# and  a == a  -->  True
# So, I think we should have a == a != b  --->  a != b,
# or something else consistent. For now,
# we follow Mma instead of weeding out comparisons known to
# be true.
function do_Comparison(mx::Mxpr{:Comparison},args...)
    len = length(args)
    for i in 2:2:len
        a = args[i-1]
        cmp = args[i]
        b = args[i+1]
        res = _do_Comparison(a,cmp,b)
        res == false && return res
        res != true && return mx
    end
    return true
end


function _do_Comparison(a::Number, comp::Symbol, b::Number)
    if comp == :<    # Test For loop shows this is much faster than evaling Expr
        return a < b
    elseif comp == :>
        return a > b
    elseif comp == :(==)
        return a == b
    elseif comp == :(>=)
        return a >= b
    elseif comp == :(<=)
        return a <= b
    elseif comp == :(!=)
        return a != b
    elseif comp == :(===)
        return a === b        
    end
    eval(Expr(:comparison,a,comp,b)) # This will be slow.    
end

function _do_Comparison{T<:Union(Mxpr,Symbol,String,DataType)}(a::T,comp::SJSym,b::T)
    if comp == :(==)
        res = a == b
        res && return res
    elseif comp == :(!=)
        res = a == b
        res && return false
    elseif comp == :(===)
        return a === b
    end
    return nothing
end

# TODO: Try to find why the Unions don't work and condense these methods
function _do_Comparison(a::Mxpr,comp::Symbol,b::Mxpr)
    if comp == :(==)
        res = a == b
        res && return res
    elseif comp == :(!=)
        res = a == b
        res && return false
    elseif comp == :(===)
        return a === b
    end
    return nothing
end

function _do_Comparison(a::Mxpr,comp::Symbol,b::Symbol)
    if comp == :(==)
        res = a == b
        res && return res
    elseif comp == :(!=)
        res = a == b
        res && return false
    elseif comp == :(===)
        return a === b
    end
    return nothing
end

function _do_Comparison(a, comp::SJSym, b::Bool)
#    println("In any cmp  bool $a $comp $b")    
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return false
end

function _do_Comparison(a, comp::SJSym, b::Number)
#    println("In any cmp  bool $a $comp $b")    
    comp == :(==) && return false
    comp == :(!=) && return true
    comp == :(===) && return false
    return false
end

function _do_Comparison(a::Bool, comp::SJSym, b::Bool)
#    println("In bool cmp  $a $comp $b")
    comp == :(==) && return a == b
    comp == :(!=) && return a != b
    comp == :(===) && return a == b    
    return false  # I guess this is good
end


## A few Number rules

# These may not all be necessary.

apprules(mx::Mxpr{://}) = makerat(mx,mx[1],mx[2])
makerat{T<:Number}(mx::Mxpr{://},n::T,d::T) = n//d
makerat(mx,n,d) = mx
apprules(mx::Mxpr{:complex}) = makecomplex(mx,mx[1],mx[2])
makecomplex(mx::Mxpr{:complex},n::Real,d::Real) = complex(n,d)
makecomplex(mx,n,d) = mx

#### Power

# Probably faster to handle this in
# canonicalization code
function apprules(mx::Mxpr{:Power})
    do_Power(mx,mx[1],mx[2])
end

# TODO: Make sure Rationals are simplified
do_Power(mx::Mxpr{:Power},b::Number,e::Number) = mpow(b,e)
do_Power(mx::Mxpr{:Power},b::Symbolic,n::Integer) = n == 1 ? b : n == 0 ? one(n) : mx
do_Power(mx::Mxpr{:Power},b::Mxpr{:Power},exp::Integer) = mpow(base(b), mmul(exp,expt(b)))
do_Power(mx::Mxpr{:Power},b::Mxpr{:Power},exp::Real) = mpow(base(b), mmul(exp,expt(b)))
do_Power(mx::Mxpr{:Power},b::Mxpr{:Power},exp) = is_Number(expt(b)) ? mpow(base(b), mmul(expt(b),exp)) : mx

do_Power(mx::Mxpr{:Power},b::SJSym,expt::FloatingPoint) = b == :E ? exp(expt) : mx
do_Power{T<:FloatingPoint}(mx::Mxpr{:Power},b::SJSym,expt::Complex{T}) = b == :E ? exp(expt) : mx

#
# Check if the exact answer is an integer.
function do_Power{T<:Integer}(mx::Mxpr{:Power},b::T,e::Rational)
    res = b^e
    ires = round(T,res)
    nrat = Rational(den(e),num(e))
    return ires^nrat == b ? ires : mx
end

do_Power(mx,b,e) = mx

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
@sjseealso_group(BI,BF,Big)
@sjdoc BF "
BF(n) converts the number, or string n to a BigFloat. SJulia currently neither
detects overflow, nor automatically promotes types from fixed to arbitrary precision.
"

@sjdoc Big "
Convert a number to a maximum precision representation (typically
'BigInt' or 'BigFloat')
"

apprules(mx::Mxpr{:BI}) = dobigint(mx,mx[1])
dobigint(mx,x) = mx
dobigint{T<:Number}(mx,x::T) = BigInt(x)
dobigint{T<:AbstractString}(mx,x::T) = BigInt(x)

apprules(mx::Mxpr{:BF}) = dobigfloat(mx,mx[1])
dobigfloat(mx,x) = mx
dobigfloat{T<:Number}(mx,x::T) = BigFloat(x)
dobigfloat{T<:AbstractString}(mx,x::T) = BigFloat(x)

apprules(mx::Mxpr{:Big}) = do_Big(mx,mx[1])
do_Big(mx,x) = mx
do_Big(mx,x::Number) = big(x)

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

@sjseealso_group(Timing,Allocated,TimeOn,TimeOff,TrDownOn,TrDownOff,TrUpOn,TrUpOff)
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

apprules(mx::Mxpr{:TimeOn}) = (set_timing() ; nothing)
apprules(mx::Mxpr{:TimeOff}) = (unset_timing(); nothing)

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
Help(sym) or Help(\"sym\") prints documentation for the symbol sym. Eg: Help(Expand).
Help() lists all documented symbols. Due to parsing restrictions at the repl, for some
topics, the input must be a string. The same help can be accessed with
h\"topic\", which is implemented as a Julia special string literal.
Help(regex) prints a list of topics whose documentation text matches the
regular expression regex.
Help(All -> true) prints all of the documentation.
"

apprules(mx::Mxpr{:Help}) = do_Help(mx,margs(mx)...)

# function apprules(mx::Mxpr{:Help})
#     if length(mx) > 0 && mx[1] == mxpr(:RuleDelayed, :All,true)
#         print_all_docs()
#     else
#         print_doc(margs(mx)...)
#     end
# end

function do_Help(mx,args...)
    if length(mx) > 0 && mx[1] == mxpr(:RuleDelayed, :All,true)
        print_all_docs()
    else
        if length(margs(mx)) == 0
            print_doc("Help")
        end
        print_doc(margs(mx)...)
    end
end

function do_Help(mx,r::Regex)
    print_matching_topics(r)
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

@sjdoc ToString "
ToStringLength(expr) returns the string of the printed form or expr.
"
apprules(mx::Mxpr{:ToString}) = string(mx[1])


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

## ExpandA, only a bit is implemented

@sjdoc ExpandA "
ExpandA(expr) expands products in expr. This is only partially implemented,
mostly to test the efficiency of evaluation and evaluation control. The
sympy version Expand() is more capable, but slower.
"
apprules(mx::Mxpr{:ExpandA}) = _doexpand(mx[1])
