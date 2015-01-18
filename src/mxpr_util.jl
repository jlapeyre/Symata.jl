# nice, but we are not using it now.
# stringtosymbols(str::String) = [ symbol(s)  for s in split(str) ]

macro mdebug(level, a...)
    if level <= MXDEBUGLEVEL
        :((println($(a...));println()))
    else
        nothing
    end
end

## Usage: @ma(mx,k) = val  --> margs(mx)[k] = val
# Use this to set an element of an Mxpr without canonicalizing.  Eg,
# setindex!, i.e.  mx[k] = va; will reorder every time it is called on
# type Orderless.
macro ma(mx,k)
    :((margs($(esc(mx)))[$(esc(k))]))
end
