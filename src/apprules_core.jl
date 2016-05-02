# mkapprule defines a builtin "function" corresponding to a Head. Eg, Cos, While, Table, ...
# @mkapprule Headname  actually defines builtin evaluation rules for expressions with head Headname
#
# You can supply information about the number of args etc. by passing a dict to @mkapprules like this:
#   @mkapprules Headname  :key1 => val1  :key2 => val2 ....
# 
# @mkapprules writes a default rule that returns the input expression.
# Eg. if you don't write a function (described below) to handle an expression with head Headname(a,b,c)
# Headname(a,b,c) is returned
#
# :nargs => n              The expression requires exactly n arguments.
# :nargs =  ur::UnitRange  The number of arguments must lie within the range.
# :options = Dict( :opt1 => default1, ... )  The expression takes keyword arguments, "Options". These are translated
#                                            to rules, which is how we implement keyword argumentss.
# TODO :nargs =>  one or more, etc.
# TODO use this syntax :options = (:opt1 => default1, ...) 

# get_arg_dict constructs a Dict from the macro aguments.
function get_arg_dict(args)
    d = Dict{Symbol,Any}()
    for p in args
        pe = eval(p)
        d[pe[1]] = pe[2]
    end
    d
end

macro mkapprule(args...)
    n = length(args)
    head = args[1]
    headstr = string(head)
    fns = symbol("do_" * headstr)
    mxarg = parse("mx::Mxpr{:" * headstr * "}") # something easier worked too, but I did not realize it
    local apprulecall
    apprulecall = :(apprules($mxarg) = $fns(mx, margs(mx)...))    
    defmx = :( mx )
    specs = Dict()
    if n > 1
        rargs = args[2:end]
        specs = get_arg_dict(rargs)
    end
    if haskey(specs, :options)
        optd = get_arg_dict(specs[:options])
        if haskey(specs, :nargs)
            nargcode = checkargscode(:newargs,head,specs[:nargs])
        else
            nargcode = :(nothing)
        end
        apprulecall = :(function apprules($mxarg)
                        kws = $optd
                        newargs = separate_known_rules(mx,kws)
                        $nargcode
                        if length(kws) == 0
                        $fns(mx,margs(mx)...)
                        else
                        $fns(mx,newargs...; kws...)
                        end
                        end )
    elseif haskey(specs, :nargs)
        nargcode = checkargscode(:mx,head,specs[:nargs])
        if isa(specs[:nargs],Int) || isa(specs[:nargs],UnitRange)
            nargcode = checkargscode(:mx,head,specs[:nargs])
            apprulecall = :(function apprules($mxarg)
                            $nargcode
                            $fns(mx,margs(mx)...)
                            end)
        end
    else
        nothing
    end
    if haskey(specs, :nodefault)
        defaultmethod = ""
    else
        defaultmethod = :($fns($mxarg, args...) = $defmx)
    end
    esc(quote
        set_pattributes([$headstr],[:Protected])
        $apprulecall
        $defaultmethod
        end)
end

# The macro mkapprule writes this function:  apprules(mx::Mxpr{:Headname}) = do_Headname(mx,margs(mx)...)
# We have to write functions do_Headname that handle various argument numbers and types via Julia's multiple dispatch.
# doap macro writes a do_Headname rule (a function)
# For example:
#    do_Headname(mx::Mxpr{:Headname},x,y)
# handles expressions Headname(x,y)
# To write this rule, you call doap like this
# @doap function Headname(x,y)
#                ...
#       end
# or
# @doap  function Headname(x,y) = ...
#
# or
# @doap function Headname{T<:SomeType}(x,y::T) = ...
# etc.

function quotesymbol(sym)
    QuoteNode(sym)
end

macro doap(func)
    prototype = func.args[1].args                      # eg.  Headname{T,V}(x::T,y::V) or Headname(x,y::Int), etc.
    sj_func_name0 = prototype[1]                       #      Headname{T,V}, or Headname
    if isa(sj_func_name0, Expr)                        # got  Headname{T,V}
        if ( sj_func_name0.head == :curly )
            sj_func_name = sj_func_name0.args[1]       #   get Headname
            new_func_name = symbol("do_",sj_func_name) #  Headname --> do_Headname
            sj_func_name0.args[1] = new_func_name      #  replace Headname{T,V} -> do_Headname{T,V}      
        else
            error("Can't interpret ", sj_func_name)
        end
    else                                                       # got Headname(x,y)
        sj_func_name = sj_func_name0                 
        new_func_name = symbol("do_",sj_func_name)             # do_Headname
        prototype[1] = new_func_name                           # replace Headname --> do_Headname
    end
    local quote_sj_func_name
    if isa(sj_func_name,Symbol)
        quote_sj_func_name = quotesymbol(sj_func_name)         # :Headname from prototype like Headname(args...)
    elseif isa(sj_func_name,Expr)
        quote_sj_func_name = quotesymbol(sj_func_name.args[1]) # :Headname from prototype like Headname{T,V}(args...)
    else
        error("doap: Can't interpret ", sj_func_name)
    end
    mxarg =  :( mx::Mxpr{$quote_sj_func_name}  )             # mx::Mxpr{:Headname}
    insert!(prototype,2, mxarg)                              # insert mx::Mxpr{:Headname} as the first argument in prototype
    :(($(esc(func))))                                        # return the rewritten function
end

#### Default apprule
#  This is for a head that is (almost always) not a "system" symbol.
#  Usually, it is a user defined symbol

apprules(x) = x
