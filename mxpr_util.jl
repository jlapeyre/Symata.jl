# nice, but we are not using it now.
# stringtosymbols(str::String) = [ symbol(s)  for s in split(str) ]

macro mdebug(level, a...)
    if level <= MXDEBUGLEVEL
        :(println($(a...)))
    else
        nothing
    end
end
