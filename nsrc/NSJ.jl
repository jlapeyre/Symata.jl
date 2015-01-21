for v in ( :(:Set), )
    @eval begin
        set_attribute($v,:HoldFirst)
        set_attribute($v,:Protected)        
    end
end

for v in ( :(:Clear), :(:SetDelayed) )
    @eval begin
        set_attribute($v,:HoldAll)
        set_attribute($v,:Protected)        
    end
end

## Symbol correspondence between Julia and SJulia

const JTOMSYM  =
 Dict(
      :(=) => :Set,
      :(:=) => :SetDelayed,
      :+ => :Plus,
      :* => :Times,
      :(=>) => :Rule, # Mma uses ->  (hmmm)
      :vcat => :List,
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
    s.head == getsym(:List) ? nothing : show(io,s.head)
    args = s.args
    print(io,s.head == getsym(:List) ? LISTL : FUNCL)
    for i in 1:length(args)-1
        show(io,args[i])
        print(io,",")
    end
    show(io,args[end])
    print(io,s.head == getsym(:List) ? LISTR : FUNCR)
end

function Base.show(io::IO, s::Mxpr{:SetDelayed})
    show(io,s[1])
    print(" := ")
    show(io,s[2])
end

function show_binary(io::IO, mx::Mxpr)
    show(io,mx.args[1])
    print(io, "  ", mtojsym(mx.head), " ")
    show(io,mx.args[2])
end

## Translate Expr to Mxpr

extomx(x) = x
extomx(s::Symbol) = getsym(jtomsym(s))

function extomxarr(ain,aout)
    for x in ain
        push!(aout,extomx(x))
    end
end

function extomx(ex::Expr)
    newa = Array(Any,0)
    local head::Symbol
    a = ex.args    
    if ex.head == :call || ex.head == :ref
        head = jtomsym(a[1])
        for i in 2:length(a) push!(newa,extomx(a[i])) end
    elseif haskey(JTOMSYM,ex.head)
        head = JTOMSYM[ex.head]
        extomxarr(a,newa)        
    else        
        dump(ex)
        error("extomx: No translation for Expr head '$(ex.head)'")
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

# Set SJSym value.
# Set has HoldFirst, SetDelayed has HoldAll.
function apprules(mx::Union(Mxpr{:Set},Mxpr{:SetDelayed}))
    checkprotect(mx.args[1])
    sjset(mx.args[1],mx.args[2])
    mx.args[2]
end

# 'Clear' a value. ie. set symbol's value to its name
function apprules(mx::Mxpr{:Clear})
    for a in mx.args
        checkprotect(a)
        a.val = a.name
    end
end
