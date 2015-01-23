macro mdebug(level, a...)
    if level <= MXDEBUGLEVEL
        :((println($(a...));println()))
    else
        nothing
    end
end

is_type(x,t::DataType) = typeof(x) == t
is_type_less(x,t::DataType) = typeof(x) <: t
is_type(x,t::UnionType) = typeof(x) == t
is_type_less(x,t::UnionType) = typeof(x) <: t


## For compatibility with older code.
## Usage: @ma(mx,k) = val  --> margs(mx)[k] = val
# Use this to set an element of an Mxpr without canonicalizing.  Eg,
# setindex!, i.e.  mx[k] = va; will reorder every time it is called on
# type Orderless.
macro ma(mx,k)
#    :((margs($(esc(mx)))[$(esc(k))]))
    :((($(esc(mx))).args[$(esc(k))]))
end
