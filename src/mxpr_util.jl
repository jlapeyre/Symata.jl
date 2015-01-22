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
