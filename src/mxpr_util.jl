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
    mx = Expr(:macrocall, symbol("@ex"), expr)
    Expr(:macrocall,symbol("@test"),eval(mx))
end

## For compatibility with older code.
## Usage: @ma(mx,k) = val  --> margs(mx)[k] = val
# Use this to set an element of an Mxpr without canonicalizing.  Eg,
# setindex!, i.e.  mx[k] = va; will reorder every time it is called on
# type Orderless.
macro ma(mx,k)
#    :((margs($(esc(mx)))[$(esc(k))]))
    :((($(esc(mx))).args[$(esc(k))]))
end

capitalize_first_character(s::String) = uppercase(string(s[1])) * s[2:end]

