#### LeveSpec types for various types of level specification

## TODO: Heads => True

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
    includezero::Bool
end

LevelSpecAll() = LevelSpecAll(false)

#### Create LevelSpec instances from Mxpr expressions

## Interpretation of negative index is different here than from Mma
## In Mma, a negative index counts up from each leaf.
## TODO: or FIXME
## 1. Implement both depth-first and breadth-first traversal. Only
## breadth-first is implmented now.
## 2. For negative indicies. Traverse the whole tree and build a flat
## Array of of the depth of each leaf. Then count negative levels from
## each leaf.

# Form n.  levels 1 through n
function make_level_specification(expr,n::Integer)
    if n < 0 n += depth(expr) end
    LevelSpecToDepth(n)
end

# Form [n] or [m,n]  level n only, or levels m through n
function make_level_specification(expr,spec::Mxpr{:List})
    _make_level_specification(expr,spec,margs(spec)...)
end

# Form Infinity or else error. levels 1 through Infinity
function make_level_specification(expr,x)
    if x == Infinity     # could use singlton type
        return LevelSpecRange(1,depth(expr))
#        return LevelSpecAll(false)
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

function _make_level_specification(expr, spec::Mxpr{:List}, start::Integer, stop)
    stop != Infinity && symerror("Level specification $spec is not of the form n, [n], or [m, n].")
    d = depth(expr)
    if start < 0 start += d end
    LevelSpecRange(start,d)
end

function _make_level_specification(expr, spec::Mxpr{:List},args...)
    symerror("Level specification $spec is not of the form n, [n], or [m, n].")
end

#### Structure for performing action at levels

type LevelAction
    data::Any            # application-specific data
    doaction::Function   # function takes arguments (data,expr)
    levelind::Int
    subind::Int
    parent::Any
    levelbreak::Bool
end

LevelAction(data,doaction) = LevelAction(data,doaction,0,0,Null,false)

has_level_zero(spec::LevelSpecAtDepth) = spec.level == 0
has_level_zero(spec::LevelSpecToDepth) = spec.level == 0
has_level_zero(spec::LevelSpecRange) = spec.start == 0
has_level_zero(spec::LevelSpecAll) = spec.includezero

# This is length for mapping purposes
function length_for_level(x)
    return 0
end

function length_for_level(mx::Mxpr)
    return length(mx)
end

function checkbreak(action)
    if action.levelbreak
        true
    else
        false
    end
end

## breadth first traverse
macro travcode()
    return quote
        elen = length_for_level(expr)
        if elen > 0
            action.parent = expr
            for i in 1:elen
                action.parent = expr
                action.levelind += 1
                action.subind = i

                traverse_levels!(action,spec,expr[i])
                if  checkbreak(action)
                    return
                end
                action.levelind -= 1
            end
#            action.parent = Null
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
    if action.levelind <= spec.level && action.levelind > 0
        action.doaction(action.data, expr)
        checkbreak(action) && return
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
