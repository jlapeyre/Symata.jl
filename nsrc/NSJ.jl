for v in ( "Set", "Pattern", "SetJ" )
    @eval begin
        set_attribute(symbol($v),:HoldFirst)
        set_attribute(symbol($v),:Protected)        
    end
end

for v in ("Clear", "SetDelayed", "HoldPattern", "Hold", "DumpHold")
    @eval begin
        set_attribute(symbol($v),:HoldAll)
        set_attribute(symbol($v),:Protected)
    end
end

for v in ("Attributes",)
    @eval begin
        set_attribute(symbol($v),:HoldAll)
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:Listable)        
    end
end

for v in (:(:Dump), :(:Cos), :(:Length),:(:Plus),:(:Times), :(:Blank),
          :(:JVar))
    @eval begin
        set_attribute($v,:Protected)        
    end
end

for v in ("Pi","E")
    @eval begin
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:ReadProtected)
        set_attribute(symbol($v),:Constant)        
    end
end

## Symbol correspondence/translation between Julia and SJulia

const JTOMSYM  =
 Dict(
      :(=) => :Set,
      :(:=) => :SetDelayed,
      :+ => :Plus,
      :* => :Times,
      :^ => :Power,
      :(=>) => :Rule, # Mma uses ->  (hmmm)
      :vcat => :List,
      :ref => :Part,
      :cell1d => :List,   # curly brackets, but deprecated by julia
      :comparison => :Comparison,
      )

const MTOJSYM = Dict{Symbol,Symbol}()
for (k,v) in JTOMSYM  MTOJSYM[v] = k end

function jtomsym(x::Symbol)
    if haskey(JTOMSYM,x)
        return JTOMSYM[x]
    end
    return x
end

mtojsym(s::SJSym) = mtojsym(s.name)
function mtojsym(x::Symbol)
    if haskey(MTOJSYM,x)
        return MTOJSYM[x]
    end
    return x
end

const OPTYPE  = Dict{Symbol,Symbol}()

for op in (:(=), :(:=), :(=>), :Rule )
    OPTYPE[op] = :binary
end

getoptype(s::SJSym) = getoptype(s.name)

function getoptype(x::Symbol)
    if haskey(OPTYPE,x)
        return OPTYPE[x]
    end
    return :prefix
end

## SJSym functions

Base.show(io::IO, s::SJSym) = Base.show_unquoted(io,s.name)
sjeval(s::SJSym) = s.val
sjset(s::SJSym,val) = s.val = val
==(a::SJSym,b::SJSym) = a.name == b.name

getindex(x::Mxpr,k::Int) = return k == 0 ? x.head : x.args[k]
function mxpr(s::SJSym,iargs...)
    args = Array(Any,0)
    for x in iargs push!(args,x) end
    Mxpr{s.name}(s,args)
end
mxpr(s::Symbol,args...) = mxpr(getsym(s),args...)

function get_attributes(sj::SJSym)
    ks = sort!(collect(keys(sj.attr)))
    sjs = [getsym(x) for x in ks]
    mxpr(:List,sjs...)
end

function ==(ax::Mxpr, bx::Mxpr)
    ax.head != bx.head  && return false
    a = ax.args
    b = bx.args
    (na,nb) = (length(a),length(b))
#    println("length $na $nb")
    na != nb && return false
    for i in 1:na
        a[i] != b[i] && return false
    end
    true
end

getindex(mx::Mxpr, k::Int) = return k == 0 ? mx.head : mx.args[k]
Base.length(mx::Mxpr) = length(mx.args)

## Mxpr Display

# Mathematica syntax
# const FUNCL = '['
# const FUNCR = ']'
# const LISTL = '{'
# const LISTR = '}'

# Julia-like syntax
const FUNCL = '('
const FUNCR = ')'
const LISTL = '['
const LISTR = ']'

function Base.show(io::IO, s::Mxpr)
    if getoptype(s.head) == :binary
        return show_binary(io,s)
    end
    show_prefix_function(io,s)
end

function show_prefix_function(io::IO, mx::Mxpr)
    mx.head == getsym(:List) ? nothing : show(io,mx.head)
    args = mx.args
    print(io,mx.head == getsym(:List) ? LISTL : FUNCL)
    for i in 1:length(args)-1
        show(io,args[i])
        print(io,",")
    end
    isempty(args) || show(io,args[end])
    print(io,mx.head == getsym(:List) ? LISTR : FUNCR)    
end

function show_binary(io::IO, mx::Mxpr)
    if length(mx) != 2
        show_prefix_function(io,mx)
    else
        show(io,mx.args[1])
        print(io, "  ", mtojsym(mx.head), " ")
        show(io,mx.args[2])
    end
end

## Translate Expr to Mxpr

extomx(x) = x
function extomx(s::Symbol)
    ss = string(s)
    if contains(ss,"_")
        return parseblank(ss)
    else
        return getsym(jtomsym(s))
    end
end

function parseblank(s::String)
    a = split(s,['_'], keep=true)
    length(a) > 3 && error("parseblank: Illegal Pattern expression '$s'")
    if length(a) == 2
        blanktype = :Blank
        (blankhead,blankname) = (a[1],a[2])
    else
        a[2] != "" && error("parseblank: Illegal Pattern expression '$s'")
        blanktype = :BlankSequence
        (blankhead,blankname) = (a[1],a[3])
    end
    if length(blankname) == 0
        blank = mxpr(blanktype)
    else
        blank = mxpr(blanktype,getsym(blankname))
    end
    length(blankhead) == 0 && return blank
    mxpr(:Pattern,getsym(blankhead),blank)
end

function extomxarr(ain,aout)
    for x in ain
        push!(aout,extomx(x))
    end
end

function extomx(ex::Expr)
    newa = Array(Any,0)
    local head::Symbol
    a = ex.args    
    if ex.head == :call
        head = jtomsym(a[1])
        for i in 2:length(a) push!(newa,extomx(a[i])) end
    elseif ex.head == :block
        mx = extomx(a[2])
        return mx
    elseif haskey(JTOMSYM,ex.head)
        head = JTOMSYM[ex.head]
        extomxarr(a,newa)        
    else        
        dump(ex)
        error("extomx: No translation for Expr head '$(ex.head)' in $ex")
    end
    mxpr(head,newa...)
end

## Macro for translation and evaluation, at repl or from file

global NEVAL = 0

macro ex(ex)
    res = extomx(ex)
    NEVAL = 1
    mx = meval(res)
    local mx1
    while true
        mx1 = meval(mx)
        if mx1 == mx
            break
        end
        if NEVAL > 1000
            error("Too many evaluations.")
        end
        mx = mx1
    end
    NEVAL = 0
    :(($(esc(mx1))))
end


## Evaluation of Mxpr

meval(x) = x
meval(s::SJSym) = s.val == s.name ? s : s.val

function meval(mx::Mxpr)
    nhead = meval(mx.head)
    nargs = Array(Any,0)
    start = 1
    if get_attribute(mx.head.name,:HoldFirst)
        start = 2
        push!(nargs,mx.args[1])
    end
    if get_attribute(mx.head.name,:HoldAll)
        nargs = mx.args
    else
        for i in start:length(mx.args)
            push!(nargs,meval(mx.args[i]))
        end
    end
    nmx = mxpr(nhead,nargs...)
    apprules(nmx)
end

apprules(x) = x

function checkprotect(s::SJSym)
    get_attribute(s.name,:Protected) &&
    error("Symbol '",s.name, "' is protected.")
end

checkprotect(mx::Mxpr) = checkprotect(mx.head)

# Set SJSym value.
# Set has HoldFirst, SetDelayed has HoldAll.
function apprules(mx::Union(Mxpr{:Set},Mxpr{:SetDelayed}))
    set_and_setdelayed(mx,mx.args[1],mx.args[2])
end

function set_and_setdelayed(mx,lhs::SJSym, rhs)
    checkprotect(lhs)
    sjset(lhs,rhs)
    rhs
end

function apprules(mx::Mxpr{:SetJ})
    lhs = mx.args[1]
    rhs = mx.args[2]
    eval(Expr(:(=),lhs.name,rhs))
end 

# No, not this way
#function set_and_setdelayed(mx,lhs::Mxpr{:JVar}, rhs)
#    eval(Expr(:(=),lhs.args[1].name,rhs))
#end

set_and_setdelayed(mx,y,z) = mx

# 'Clear' a value. ie. set symbol's value to its name
function apprules(mx::Mxpr{:Clear})
    for a in mx.args
        checkprotect(a)
        a.val = a.name
    end
end

apprules(mx::Union(Mxpr{:Dump},Mxpr{:DumpHold})) = for a in mx.args dump(a) end

apprules(mx::Mxpr{:Length}) = symjlength(mx.args[1])
symjlength(mx::Mxpr) = length(mx.args)
symjlength(x) = length(x)

# This won't work for setting a part
function apprules(mx::Mxpr{:Part})
    a = mx.args
    (a[1])[a[2]]
end

apprules(mx::Mxpr{:Head}) = gethead(mx.args[1])
gethead(mx::Mxpr) = mx.head
gethead(ex) = typeof(ex)

apprules(mx::Mxpr{:JVar}) = eval(mx.args[1].name)

apprules(mx::Mxpr{:Attributes}) = get_attributes(mx.args[1])

apprules(mx::Mxpr{://}) = makerat(mx,mx.args[1],mx.args[2])
makerat{T<:Number}(mx::Mxpr{://},n::T,d::T) = n//d
makerat(mx,n,d) = mx
apprules(mx::Mxpr{:complex}) = makecomplex(mx,mx.args[1],mx.args[2])
makecomplex(mx::Mxpr{:complex},n::Real,d::Real) = complex(n,d)
makecomplex(mx,n,d) = mx
