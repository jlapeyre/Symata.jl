#### LeveSpec types for various types of level specification

abstract LevelSpec

immutable LevelSpecToDepth  <: LevelSpec
    level::Int
end

immutable LevelSpecAtDepth  <: LevelSpec
    level::Int
end

immutable LevelSpecRange  <: LevelSpec
    start::Int
    stop::Int
end

immutable LevelSpecAll  <: LevelSpec
end

#### Create LevelSpec instances from Mxpr expressions

# Form n
function make_level_specification(expr,n::Integer)
    if n < 0 n += depth(expr) end
    LevelSpecToDepth(n)
end

# Form [n] or [m,n]
function make_level_specification(expr,spec::Mxpr{:List})
    _make_level_specification(expr,spec,margs(spec)...)
end

# Form Infinity or else error.
function make_level_specification(expr,x)
    if x == Infinity     # could use singlton type
        return LevelSpecAll()
    end
    symerror("Level specification $x is not of the form n, {n}, or {m, n}.")
end

#########

# Form [n]
function _make_level_specification(expr,spec::Mxpr{:List}, n::Integer)
    if n < 0 n += depth(expr) end
    LevelSpecAtDepth(n)
end

# Form [m,n]
function _make_level_specification(expr, spec::Mxpr{:List}, start::Integer, stop::Integer)
    if start < 0 start += depth(expr) end
    if stop < 0 stop += depth(expr) end
    LevelSpecRange(start, stop)
end

function _make_level_specification(expr, spec::Mxpr{:List},args...)
    symerror("Level specification $spec is not of the form n, {n}, or {m, n}.")
end

#### Structure for performing action at levels

type LevelAction
    data::Any
    doaction::Function
    levelind::Int
    subind::Int
    parent::Any
    levelbreak::Bool
end

LevelAction(data,doaction) = LevelAction(data,doaction,0,0,Null,false)

has_level_zero(spec::LevelSpecAtDepth) = spec.level == 0
has_level_zero(spec::LevelSpecToDepth) = spec.level == 0
has_level_zero(spec::LevelSpecRange) = spec.start == 0
has_level_zero(spec::LevelSpecAll) = true

# This is length for mapping purposes
function length_for_level(x)
    return 0
end

function length_for_level(mx::Mxpr)
    return length(mx)
end

macro travcode()
    return quote
        elen = length_for_level(expr)
        if elen > 0
            action.parent = expr
            for i in 1:elen
                action.levelind += 1
                action.subind = i
                traverse_levels!(action,spec,expr[i])
                action.levelbreak && return
                action.levelind -= 1
            end
        end
    end
end

function traverse_levels!(action::LevelAction, spec::LevelSpecAtDepth, expr)
    if action.levelind == spec.level
        action.doaction(action.data, expr)
        action.levelbreak && return
    elseif action.levelind < spec.level
        @travcode
    end
end

function traverse_levels!(action::LevelAction, spec::LevelSpecToDepth, expr)
    if action.levelind <= spec.level
        action.doaction(action.data, expr)
        action.levelbreak && return
    end
    if action.levelind < spec.level
        @travcode
    end
end

function traverse_levels!(action::LevelAction, spec::LevelSpecRange, expr)
    if action.levelind <= spec.stop && action.levelind >= spec.start
        action.doaction(action.data, expr)
        action.levelbreak && return
    end
    if action.levelind < spec.stop
        @travcode
    end
end

function traverse_levels!(action::LevelAction, spec::LevelSpecAll, expr)
    action.doaction(action.data, expr)
    action.levelbreak && return
    @travcode
end
