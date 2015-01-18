isexpr(x) = (typeof(x) == Expr)
iscall(x) = isexpr(x) && x.head == :call

function iscomplex(ex)
    typeof(ex) <: Complex ||
    (iscall(ex) && ex.args[1] == :complex)
end
