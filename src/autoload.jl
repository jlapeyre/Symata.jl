type SymataAutoload
    triggers::Dict
    isloaded::Bool
end

const autoload = SymataAutoload(Dict{Symbol,Bool}(),false)

addtriggers(args...) = foreach( t -> (autoload.triggers[t] = true; protect(t)), args)

check_autoload(x) = nothing

"""
    check_autoload(s::Symbol)

if `s` is in a list of triggers check if the Symata language code has
been loaded. If it has not been, load it. If it has been, do nothing.

`check_autoload` is called in: in `AST_translation.jl`, `AST_translation_tables.jl`,
and `doc.jl`. It is called in most instances of reading a symbol when reading Symata
code.
"""
function check_autoload(s::Symbol)
    global autoload
    if (! autoload.isloaded) && haskey(autoload.triggers, s)
        autoload.isloaded = true        
        load_symata_code_now()
    end
end

function load_symata_code_now()
    fqpath = joinpath(dirname(@__FILE__), "../symsrc/autoloaded.sj")
    read_Symata_file(fqpath)
end

####

addtriggers(:Array, :ExpToTrig, :Subdivide, :TakeDrop, :ArrayDepth, :TensorRank, :Divide, :ListCorrelate, :ListConvolve,
            :Accumulate, :Equal, :Unequal)

# addtriggers(:Array, :ExpToTrig, :Subdivide, :TakeDrop, :ArrayDepth, :TensorRank, :Divide, :ListCorrelate, :ListConvolve,
#              :Accumulate, :SameQ, :UnsameQ, :Equal, :Unequal)
