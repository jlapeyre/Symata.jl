macro mdebug(level, a...)
    if level <= MXDEBUGLEVEL
        :((println($(a...));println()))
    else
        nothing
    end
end

using Base.Test
using SJulia

# For use in ../test/
macro testex(expr)
    mx = Expr(:macrocall, Symbol("@ex"), expr)
    result = eval(mx)
    retresult::Bool = true
    if typeof(result) <: Bool
        retresult = result
    else
        retresult = false
    end
    Expr(:macrocall,Symbol("@test"),retresult)
end

## For compatibility with older code.
## Usage: @ma(mx,k) = val  --> margs(mx)[k] = val
# Use this to set an element of an Mxpr without canonicalizing.  Eg,
# setindex!, i.e.  mx[k] = va; will reorder every time it is called on
# type Orderless.
macro ma(mx,k)
    :((($(esc(mx))).args[$(esc(k))]))
end

capitalize_first_character(s::AbstractString) = uppercase(string(s[1])) * s[2:end]

