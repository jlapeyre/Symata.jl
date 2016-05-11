# Cases::level: Level specification {2, {3}} is not of the form n, {n}, or {m, n}.

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
    error("Level specification $x is not of the form n, {n}, or {m, n}.")
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

function _make_level_specification(spec::Mxpr{:List},args...)
    error("Level specification $spec is not of the form n, {n}, or {m, n}.")
end

#### Structure for performing action at levels

type LevelAction
    data::Any    
    doaction::Function
    levelind::Int
    subexprind::Int
    parent::Any
end

LevelAction(data,doaction) = LevelAction(data,doaction,0,0,Null)

function traverse_levels!(action::LevelAction, spec::LevelSpecAtDepth, expr)
    if action.levelind == spec.level
        action.doaction(action.data, expr)
    elseif action.levelind < spec.level
        action.parent = expr
        for i in 1:length(expr)
            action.levelind += 1
            action.subexprind = i
            traverse_levels!(action,spec,expr[i])
            action.levelind -= 1            
        end
    end
end

function traverse_levels!(action::LevelAction, spec::LevelSpecToDepth, expr)
    if action.levelind <= spec.level
        action.doaction(action.data, expr)
    end
    if action.levelind < spec.level
        for i in 1:length(expr)
            action.levelind += 1
            traverse_levels!(action,spec,expr[i])
            action.levelind -= 1            
        end
    end
end

function traverse_levels!(action::LevelAction, spec::LevelSpecRange, expr)
    if action.levelind <= spec.stop && action.levelind >= spec.start
        action.doaction(action.data, expr)
    end
    if action.levelind < spec.stop
        for i in 1:length(expr)
            action.levelind += 1
            traverse_levels!(action,spec,expr[i])
            action.levelind -= 1            
        end
    end
end

function traverse_levels!(action::LevelAction, spec::LevelSpecAll, expr)
    action.doaction(action.data, expr)
    if isa(expr,Mxpr)
        for i in 1:length(expr)
            action.levelind += 1
            traverse_levels!(action,spec,expr[i])
            action.levelind -= 1            
        end
    end
end
