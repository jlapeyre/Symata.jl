module SJuliaIO

import SJulia: Mxpr, SJSym, SSJSym, is_Mxpr, is_Number, is_SJSym, getsym,
       symname, mhead, margs, is_type, getoptype,
       mtojsym

# A space, or maybe not.
const opspc = " "

# Julia-like syntax
const FUNCL = '('
const FUNCR = ')'
const LISTL = '['
const LISTR = ']'

function fullform(io::IO, mx::Mxpr)
    print(io,mhead(mx))
    print("(")
    if length(mx) > 0 fullform(io,mx[1]) end
    for i in 2:length(mx)
        print(io,",")        
        fullform(io,mx[i])
    end
    print(")")
end
fullform(io::IO,x) = show(io,x)
fullform(x) = fullform(STDOUT,x)

needsparen(x::Mxpr) = length(x) > 1
needsparen(x::Rational) = true
needsparen(x::Complex) = true
needsparen(x) = false

function de_gensym(str::String)
    if str[1] == '#' && str[2] == '#'  # De-gensym local variables for display
        return split(str,['#'],keep=false)[1]
    else
        return str
    end
end

# We need a way to force using this. In the REPL, I guess
# Hmm. Wrap all output in a type made just for that purpose.... no that won't work
# sjshow(io::IO,x) = Base.show(io,x)

function Base.show(io::IO, s::SJSym)
    if symname(s) == :Pi
        Base.show_unquoted(io,:π)
    elseif symname(s) == :EulerGamma
        Base.show_unquoted(io,:γ)
    elseif symname(s) == :Infinity
        Base.show_unquoted(io,:∞)
    else
        ss = string(symname(s))
        ss = de_gensym(ss) # remove gensym characters
        Base.show_unquoted(io,symbol(ss))
    end
end

# This may break. It will only work if the value of s
# is the symbol name in the symbol table that is associated
# with s; ie it is a 'free' symbol. SSJSym does not carry symbol name information.
# This is only stored as the key to the symbol table.
Base.show(io::IO, s::SSJSym) = Base.show(symname(s))
    
# We display real part if it is 0.0
function Base.show{T<:Real}(io::IO, z::Complex{T})
    show(io,real(z))
    print(io," + ")
    show(io,imag(z))
    print(io,"I")    
end

# Do not display real part if it is 0
function Base.show{T<:Integer}(io::IO, z::Complex{T})
    if real(z) != 0
        show(io,real(z))
        print(io," + ")
    end
    if imag(z) == 1
        print(io,"I")
    else
        show(io,imag(z))
        print(io,"I")
    end
end

function Base.show{T<:Integer}(io::IO, z::Complex{Rational{T}})
    if real(z) != 0
        show(io,real(z))
        print(io," + ")
    end
    if imag(z) == 1
        print(io,"I")
    else
        show(io,imag(z))
        print(io,"I")
    end
end

Base.show{T<:BigFloat}(io::IO,x::T) = Base.showcompact(io,x)

# Not sure this is a good idea, confusing symbols with boolean values
function Base.show(io::IO, v::Bool)
    v ? Base.show_unquoted(io,:True) : Base.show_unquoted(io,:False)
end

Base.show(io::IO, mx::Mxpr{:FullForm}) = fullform(io,mx[1])

# Arguments are not evaluated, like Hold.
# But Holdform is not printed
function Base.show(io::IO, s::Mxpr{:HoldForm})
    Base.show(io,s[1])
end

function Base.show(io::IO, s::Mxpr)
    if getoptype(mhead(s)) == :binary  
        return show_binary(io,s)
    elseif getoptype(mhead(s)) == :infix
        return show_infix(io,s)
    end
    show_prefix_function(io,s)
end

function Base.show(io::IO, mx::Mxpr{:Comparison})
    args = mx.args    
    for i in 1:length(args)-1
        show(io,args[i])
        print(io," ")
    end
    isempty(args) || show(io,args[end])
end

function show_prefix_function(io::IO, mx::Mxpr)
    is_Mxpr(mx,:List) ? nothing : print(io,mtojsym(mhead(mx)))
    args = mx.args
    print(io,mhead(mx) == getsym(:List) ? LISTL : FUNCL)
    wantparens = true
    if mhead(mx) == :List wantparens = false end    
    for i in 1:length(args)-1
        if needsparen(args[i]) && wantparens
            print(io,"(")
        end
        show(io,args[i])
        if needsparen(args[i]) && wantparens
            print(io,")")
        end        
        print(io,",")
    end
    isempty(args) || show(io,args[end])
    print(io,mx.head == getsym(:List) ? LISTR : FUNCR)    
end

function show_binary(io::IO, mx::Mxpr)
    if length(mx) != 2
        show_prefix_function(io,mx)
    else
        lop = mx.args[1]
        if needsparen(lop)
            print(io,"(")
            show(io,lop)
            print(io,")")
        else
            show(io,lop)
        end        
        print(io, opspc, mtojsym(mx.head), opspc)
        rop = mx.args[2]
        if  needsparen(rop)
            print(io,"(")
            show(io,rop)
            print(io,")")
        else
            show(io,rop)
        end
    end
end

function Base.show(io::IO, mx::Mxpr{:Part})
    args = margs(mx)
    show(io,args[1])
    print(io,"[")
    show(io,args[2])
    for i in 3:length(args)
        print(io,",")
        show(io,args[i])
    end
    print(io,"]")    
end

# unary minus
function Base.show(io::IO, mx::Mxpr{:Minus})
    arg = mx.args[1]
    if is_Number(arg) || is_SJSym(arg)        
        print(io,"-")
        show(io,arg)
    else
        print(io,"-(")
        show(io,arg)
        print(io,")")
    end
end

function Base.show(io::IO, mx::Mxpr{:Plus})
    args = mx.args
    if length(args) < 1
        error("show: can't show Plus with no args.")
    end
    show(io,args[1])    
    for i in 2:length(args)
        if is_type(args[i],Mxpr{:Minus})
            print(io, " - ")            
            show(io,(args[i]).args[1])
        else
            print(io, " + ")
            show(io,args[i])
        end
    end
end

function show_infix(io::IO, mx::Mxpr)
    args = margs(mx)
    np = false
    sepsym = mtojsym(mhead(mx))
    # uncomment following to print space for multiplication. But, I want "InputForm" for now,
    # so we can copy output to input.
#    if mhead(mx) == :Times sepsym = " " end # not a sym. Maybe we should make them all strings
    for i in 1:length(args)-1
        if needsparen(args[i]) # && wantparens
            np = true
            print(io,"(")
        else
            np = false
        end
        show(io,args[i])
        if np
            print(io,")")
        end
        print(io, opspc, sepsym, opspc)
    end
    if ! isempty(args)
        if needsparen(args[end]) #  && wantparens
            np = true
            print(io,"(")
        else
            np = false
        end   
        show(io, args[end])
        if np
            print(io,")")
        end
    end
end

function Base.show(io::IO, mx::Mxpr{:Blank})
    print(io,"_")    
    if length(mx) > 0
        show(io,mx[1])
    end
end

function Base.show(io::IO, mx::Mxpr{:BlankSequence})
    print(io,"__")    
    if length(mx) > 0
        show(io,mx[1])
    end
end

function Base.show(io::IO, mx::Mxpr{:BlankNullSequence})
    print(io,"__")    
    if length(mx) > 0
        show(io,mx[1])
    end
end

function Base.show(io::IO, mx::Mxpr{:Pattern})
    show(io,mx[1])
    show(io,mx[2])    
end

end # module SJuliaIO
