## Translate Expr to Mxpr

# Input lines are parsed by the Julia parser into Julia Expr.
# Here, we reinterpret the Expr into a Mxpr

extomx(x) = x
function extomx(s::Symbol)
    s == :I && return complex(0,1)
    s == :π && return :Pi
    s == :True && return true
    s == :False && return false
    ss = string(s)
    if contains(ss,"_")  # Blanks used in patterns
        return parseblank(ss)
    else
        return getsym(jtomsym(s)) # Maybe translate the symbol
    end
end

# Underscore is not allowed in symbols. Instead,
# they signify part of a pattern. This follows Mma,
# and we don't consume and Julia syntax to signify patterns.
# We don't yet parse three blanks in a row.
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
        blank = mxpr(blanktype,symbol(blankname))
    end
    length(blankhead) == 0 && return blank
    mxpr(:Pattern,symbol(blankhead),blank)
end

function extomxarr(ain,aout)
    for x in ain
        push!(aout,extomx(x))
    end
end

## Main translation routine
# We use Julia for lexing/parsing. But we change the semantics:
# sometimes a little, sometimes a lot.
function extomx(ex::Expr)
    newa = newargs()
    local head::Any
    ex = rewrite_expr(ex)
    is_type(ex,Expr) || return ex  # may be a rational
    a = ex.args
    # We usually set the head and args in the conditional and construct Mxpr at the end
    if ex.head == :call
        head = jtomsym(a[1])
        @inbounds for i in 2:length(a) push!(newa,extomx(a[i])) end
    elseif ex.head == :block
        mx = extomx(a[2]) # Can't remember, I think this is Expr with head :call
        return mx
    elseif haskey(JTOMSYM,ex.head)
        head = JTOMSYM[ex.head]
        extomxarr(a,newa)
    elseif ex.head == :kw  # Interpret keword as Set, but Expr is different than when ex.head == :(=)
        head = :Set
        extomxarr(a,newa)        
    elseif ex.head == :(:) # Eg the colon here: g(x_Integer:?(EvenQ)) := x
        if length(a) == 2
            if is_type(a[1], Symbol) && is_type(a[2], Expr) &&
                (a[2].args)[1] == :(?)
                ptargs = a[2].args
                length(ptargs) != 2 && error("extomx: too many args to PatternTest")
                is_type(ptargs[2],Symbol) || error("extomx: argument to PatternTest must be a Symbol")
                head = :PatternTest
                push!(newa,extomx(a[1]),getsym(ptargs[2]))
            else
                # something here later
                head = :Span
                extomxarr(a,newa)
                # error("extomx: No translation for $ex, can't use colon this way")
            end
        else
            head = :Span
            extomxarr(a,newa)
           # error("extomx: No translation for $ex can't use colon this way")
        end
    elseif ex.head == :quote   # Quotes are wrapped in Jxpr which is evaluated by Julia eval()
        head = :Jxpr           # This allows running Julia code from within SJulia.
        push!(newa,ex.args[1]) # We evaluate the expression only whenever the Jxpr is evaled
                               # But, this is the same effect as evaling ex
    elseif ex.head == :macrocall
        return eval(ex )
    elseif ex.head == :string
        head = :StringInterpolation
        extomxarr(a,newa)
    else        
        dump(ex)
        error("extomx: No translation for Expr head '$(ex.head)' in $ex")
    end
    mx = mxpr(head,newa)  # Create the Mxpr
    return mx
end

is_call(ex::Expr) = ex.head == :call
# is ex a call with operator op ?
is_call(ex::Expr, op::Symbol) = ex.head == :call && ex.args[1] == op
# is ex a call with operator op and len args (including the op) ?
is_call(ex::Expr, op::Symbol, len::Int) = ex.head == :call && ex.args[1] == op && length(ex.args) == len
# is ex a call with len args (including the op) ?
is_call(ex::Expr, len::Int) = is_call(ex) && length(ex.args) == len

# We check for :call repeatedly. We can optimize this later.
is_binary_minus(ex::Expr) = is_call(ex, :-, 3)
is_unary_minus(ex::Expr) = is_call(ex, :-, 2)
# number of args != 3 will pass through. But probably can't be used
is_division(ex::Expr) = is_call(ex, :/,3)  
is_power(ex::Expr) = is_call(ex, :^)

is_sqrt(ex::Expr) = is_call(ex,:Sqrt)

# save time, don't make SJulia meval convert //(a,b) to rational
# we should also detect sums/products of like numbers, etc. and
# combine them.
function is_rational(ex::Expr)
    is_call(ex, :(//), 3) && is_Number(ex.args[2]) &&
        is_Number(ex.args[3])
end

# In extomx, we first rewrite some Math to canonical forms
# a - b  -->  a + (-b)
rewrite_binary_minus(ex::Expr) = Expr(:call, :+, ex.args[2], Expr(:call,:(-),ex.args[3]))
#  - b  -->  -1 * b
rewrite_unary_minus(ex::Expr) = Expr(:call, :*, -1 , ex.args[2])
# a / b -->  a * b^(-1)
rewrite_division(ex::Expr) = Expr(:call, :*, ex.args[2], Expr(:call,:^,ex.args[3],-1))

# Not used
#rewrite_binary_minus(mx::Mxpr) = mxpr(:+, mx[1], mxpr(:(-),mx[2]))
#rewrite_division(mx::Mxpr) = mxpr(:+, mx[1], mxpr(:^,mx[2],-1))

# There is no binary minus, no division, and no sqrt in Mxpr's.
# Concrete example: a - b --> a + -b.
# We definitely need to dispatch on a hash query, or types somehow
# Other rewrites needed, but not done.
function rewrite_expr(ex::Expr)
    for i in 1:length(ex.args)
        x = ex.args[i]
        if is_type(x,Expr) && x.head == :macrocall
            ex.args[i] = eval(x)
        end
    end
    if is_unary_minus(ex)    #  - b --> -1 * b
        ex = rewrite_unary_minus(ex)
    elseif is_binary_minus(ex)  #  a - b --> a + -b.
        ex = rewrite_binary_minus(ex)
    elseif is_division(ex) # a / b --> a + b^(-1)
        ex = rewrite_division(ex)
    elseif is_call(ex, :Exp, 2)  # Exp(x) --> E^x
        ex = Expr(:call, :^, :E, ex.args[2])
    elseif is_call(ex,:Sqrt,2) # This should happen at Mxpr level, and be optimized
        ex = Expr(:call, :^, ex.args[2], Expr(:call,:(//), 1,2))
    elseif is_rational(ex)
        return eval(ex)
    end
    return ex
end
