module SJuliaIO

import Main: Mxpr, SJSym, SSJSym, is_Mxpr, getsym,
       symname, mhead, margs, is_type, getoptype,
       mtojsym

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

function Base.show(io::IO, s::SJSym)
    ss = string(symname(s))
    if ss[1] == '#' && ss[2] == '#'  # De-gensym local variables for display
        ss = split(ss,['#'],keep=false)[1]
    end
    Base.show_unquoted(io,symbol(ss))
end

function Base.show(io::IO, s::SSJSym)
    ss = string(symname(s))
    if ss[1] == '#' && ss[2] == '#'  # De-gensym local variables for display
        ss = split(ss,['#'],keep=false)[1]
    end
    Base.show_unquoted(io,symbol(ss))
end

Base.show(io::IO, mx::Mxpr{:FullForm}) = fullform(io,mx[1])

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
        print(io, "", mtojsym(mx.head), "")
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
        print(io, sepsym)
    end
    if ! isempty(args)
        if needsparen(args[end]) #  && wantparens
            np = true
            print(io,"(")
        else
            np = false
        end   
        show(io,args[end])
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
