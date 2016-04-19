# These SJulia functions (symbols) translate between SJulia and Julia data and expressions

#### JVar

@sjdoc JVar "
JVar(x) returns the Julia value of the Symbol that x evaluates to. For example,
if a = 1 in Julia and b = a in SJulia, then JVar(b) evaluates to 1.
"

@sjseealso_group(Jxpr,JVar)
apprules(mx::Mxpr{:JVar}) = eval(symname(mx[1]))

#### SetJ

@sjdoc SetJ "
SetJ(x,val) sets the Julia symbol x to val. Variables and functions in SJulia
are separate from those in Julia, ie, their table of bindings to symbols are separate.
"

# This can also be done with a Jxpr
# Bind a Julia symbol to the rhs
function apprules(mx::Mxpr{:SetJ})
    lhs = mx[1]
    rhs = mx[2]
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
         ("m = :( collect(1:3) )",
          "3-element Array{Int64,1}:\n 1\n 2\n 3"))

@sjexamp( Jxpr,
         "Call a Julia function",
         ("tf = :( time )",""),
         ("tf()","1.424287593897437e9"))

# quote, i.e. :( expr ) is parsed as a Julia expression and is wrapped as
# Mxpr with head Jxpr. It is evaluated here.
# Eg.  m = :( [1:10] )  creates a Julia array and assigns to SJulia symbol m
function apprules(mx::Mxpr{:Jxpr})
    do_jxpr(mx,mx[1])
end

function do_jxpr{T<:Union{Expr,Symbol}}(mx::Mxpr{:Jxpr}, ex::T)
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
#    args = do_unpack(obj)
    #    mx = mxpr(:List,args)
    mx = unpack_to_List(obj)
    setfixed(mx)
    setcanon(mx)
    return mx
end


function unpack_to_List(obj)
    args = do_unpack(obj)
    return mxpr(:List,args)
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


# We really need to be able to Translate from SJulia back into Julia
# There are conversion tables in
# math_functions.jl
# parse.jl
# mxpr_top.jl
# These should be organized better--- for translating in both directions.


#### JFunc

# function apprules(mxpr::Mxpr{:JFunc})
#     jname = mx[1]
#     paramlist = mx[2]
#     body = mx[3]
#     jxpr = 
# end
