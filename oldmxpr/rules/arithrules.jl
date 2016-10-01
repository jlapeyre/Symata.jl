arithrules =
  [
   @rule( 1 * _ => _ ),
   @rule( _ * 1 => _ ),
   @rule( _ * 0 => 0 ),
   @rule( 0 * _ => 0 ),
   @rule( x_ / 1 => x_ ),
   @rule( (x_ * y_) / y_ => x_ ),
   @rule(  x_^0 => 1),
   @rule(  _  + _ => 2 * _),
   @rule(  x_  + n_ * x_ => (n_+1) * x_),
   @rule(  n_ * x_ + x_ => (n_+1) * x_),
   @rule(  n_::Number  * (m_::Number * x_) => (n_*m_) * x_),
   @rule( n_::Number * x_ - m_::Number * x_ => (n_-m_) * x_),
   @rule(  +(x_,x_,x_) => 3 * x_),
   @rule(  x_ - x_  => 0),
   @rule(  0 + x_  => x_),
   @rule(  1/(1/x_) => x_),
   @rule(  x_ / x_ => 1),
   @rule(  x_ + -x_  => 0),
   @rule(  x_^n1_ * x_^n2_ => x_^(n1_+n2_)),
   @rule(  x_  * x_^n_ => x_^(1+n_)),
   @rule(  Log(x_^n_) => n_ * Log(x_)),
   @rule(  Log(x_ * y_) => Log(x_) + Log(y_)),
   @rule(  Log(Exp(x_)) => x_),
   @rule(  Log(1) => 0)
 ]


for f in (:exp, :log, :cos, :sin, :tan, :lambertw)
    s = string(f)
    f2 = symbol(string(uppercase(s[1])) * s[2:end])
    @eval begin
        ($f2)(x::Float64) = ($f)(x)
    end
end

function chkratden(x::Rational)
    println("checking den of $x")
    return x.den == 1 ? x.num : x
end

# Print expression without quotes :( )
Base.show(io::IO, ex::Expr) = Base.show_unquoted(io, ex)
Base.show(io::IO, ex::Symbol) = Base.show_unquoted(io, ex)

# Evalute expression depth first.
# If eval fails, return input
function jseval(ex)
    if isexpr(ex)
        ex = Expr(ex.head,ex.args[1], map(evalorex,ex.args[2:end])...)
    end
    evalorex(ex)
end

# Do our js eval and then quote the result to prevent Julia
# from evaluating it.
macro js(ex)
    ex = jseval(ex)

    ex = replacerepeated(ex, arithrules)
    ex = jseval(ex)  # probably need a loop
    Expr(:quote, ex)
end

# Stop Julia from approximating integer division
function /(a::Int, b::Int)
    ar = a//1
    br = b//1
    res = ar/br
    return res.den == 1 ? res.num : res
end

# Convert Rational to int if it is rational
# hmm. or handle with a rule ?
function *(a::Int, b::Rational)
    res = (a * b.num) // b.den
    return res.den == 1 ? res.num : res
end

const _jslexorder = Dict{DataType,Int}()

function mklexorder()
    i = 1
    for typ in (Float64,Int,Rational,Symbol,Expr)
        _jslexorder[typ] = i
        i += 1
    end
end

mklexorder()

_jslexless(x,y) = lexless(x,y)

function _jslexless(x::Expr,y::Expr)
    x.head !=  y.head && return x.head < y.head
    lx = length(x.args)
    ly = length(y.args)
    for i in 1:min(lx,ly)
        _jslexless(x.args[i],y.args[i]) && return true
    end
    lx < ly && return true
    return false
end

function jslexless(x,y)
    tx = typeof(x)
    ty = typeof(y)
    if tx != ty
        return _jslexorder[tx] < _jslexorder[ty]
    end
    return _jslexless(x,y)
end

function orderexpr!(x::Expr)
    op = shift!(x.args)
    sort!(x.args,lt=jslexless)
    unshift!(x.args,op)
    x
end

# sum numbers at beginning of + or * call
for (fop,name) in  ((:+,:compactplus!),(:*,:compactmul!))
    @eval begin
        function ($name)(x::Expr)
            a = x.args
            op = a[1]
            shift!(a)
            length(a) < 2 && return x
            typeof(a[2]) <: Number || return x
            sum0 = a[1]
            while length(a) > 1
                shift!(a)
                typeof(a[1]) <: Number || break
                sum0 = ($fop)(sum0,a[1])
            end
            length(a) == 0 && return sum0
            unshift!(a,sum0)
            unshift!(a,op)
            return x
        end
    end
end
