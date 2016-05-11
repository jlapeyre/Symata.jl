# Cases::level: Level specification {2, {3}} is not of the form n, {n}, or {m, n}.

immutable LevelSpecToDepth
    level::Int
end

immutable LevelSpecAtDepth
    level::Int
end

immutable LevelSpecRange
    start::Int
    stop::Int
end

immutable LevelSpecAll
end

# Form n
function make_level_specification(n::Integer)
    LevelSpecToDepth(n)
end

# Form [n] or [m,n]
function make_level_specification(spec::Mxpr{:List})
    _make_level_specification(spec,margs(spec)...)
end

# Form [n]
function _make_level_specification(spec::Mxpr{:List}, n::Integer)
    LevelSpecAtDepth(n)
end

# Form [m,n]
function _make_level_specification(spec::Mxpr{:List}, start::Integer, stop::Integer)
    LevelSpecRange(start, stop)
end

# # Form [m,n]
# function inner_make_level_specification(spec::Mxpr{:List}, specargs::Mxpr{:List}, start::Integer, stop::Integer)
#     LevelSpecRange(start, stop)
# end

# function inner_make_level_specification(spec,specargs,specargs1...)
#     error("Level specification $spec is not of the form n, {n}, or {m, n}.")
# end

function _make_level_specification(spec::Mxpr{:List},args...)
    error("Level specification $spec is not of the form n, {n}, or {m, n}.")
end

function make_level_specification(x)
    error("Level specification $x is not of the form n, {n}, or {m, n}.")
end
