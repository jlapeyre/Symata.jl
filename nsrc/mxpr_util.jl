macro mdebug(level, a...)
    if level <= MXDEBUGLEVEL
        :((println($(a...));println()))
    else
        nothing
    end
end
