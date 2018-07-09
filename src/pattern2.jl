## Pattern matching needs a new implementation. The ad hoc approach can't go further.
## This is a start ...

### Types for patterns

abstract type AbstractBlanks end

abstract type ABlank  <: AbstractBlanks end
abstract type ABlankSequence <: AbstractBlanks end
abstract type ABlankNullSequence <: AbstractBlanks end

mutable struct BlankNoHead <: ABlank end
mutable struct BlankWithHead{T} <: ABlank
    head::T
end

mutable struct BlankSequenceNoHead <: ABlankSequence end
mutable struct BlankSequenceWithHead{T} <: ABlankSequence
    head::T
end

mutable struct BlankNullSequenceNoHead <: ABlankNullSequence end
mutable struct BlankNullSequenceWithHead{T} <: ABlankNullSequence
    head::T
end

const BlanksNoHead = Union{BlankNoHead,BlankSequenceNoHead,BlankNullSequenceNoHead}
const BlanksWithHead = Union{BlankWithHead,BlankSequenceWithHead,BlankNullSequenceWithHead}

topattern(mx::Mxpr) = mxpra(topattern(mhead(mx)), mapmargs(x -> topattern(x), margs(mx)))
topattern(x) = x

## replace this later with BlankT. after removing the old BlankT
topattern(b::Mxpr{:Blank}) = isempty(b) ? BlankNoHead() : BlankWithHead(_make_blank_head(b,margs(b)...))
topattern(b::Mxpr{:BlankSequence}) = isempty(b) ? BlankSequenceNoHead() : BlankSequenceWithHead(_make_blank_head(b,margs(b)...))
topattern(b::Mxpr{:BlankNullSequence}) = isempty(b) ? BlankNullSequenceNoHead() :
          BlankNullSequenceWithHead(_make_blank_head(b,margs(b)...))

_make_blank_head(b,head,args...) = error("More than one argument in Blank.")
_make_blank_head(b,head::Symbol) = @isdefined(head) ? eval(head) : head
_make_blank_head(b,head) = head


#### Matching1

pmatch(b::BlanksNoHead,ex) = true
pmatch(b::BlankWithHead{T},ex) where {T<:DataType} = isa(ex,b.head)
pmatch(b::BlankSequenceWithHead{T},ex) where {T<:DataType} = isa(ex,b.head)
pmatch(b::BlankNullSequenceWithHead{T},ex) where {T<:DataType} = isa(ex,b.head)
pmatch(b::BlanksWithHead,ex) = isa(ex,Mxpr{b.head})
pmatch(b::AbstractBlanks,ex) = symerror("match: Can't match Blank of type ", typeof(b))

#### Matching

mutable struct Capt
    c::Dict{SJSym,Any}
end

Capt() = Capt(Dict{SJSym,Any}())

abstract type  AbstractMatchData end

mutable struct SearchRange
    start::Int
    stop::Int
end

SearchRange(r::UnitRange) = SearchRange(r.start, r.stop)

mutable struct MatchDataIn
    _expr::Mxpr
    _indr::SearchRange
    _pat ## pattern in general sense
    _capt::Capt
end

## Neither flat nor orderless
mutable struct MatchData <: AbstractMatchData
    d::MatchDataIn
end

## flat
mutable struct MatchDataF <: AbstractMatchData
    d::MatchDataIn
end

## orderless
mutable struct MatchDataO <: AbstractMatchData
    d::MatchDataIn
end

## flat and orderless
mutable struct MatchDataFO <: AbstractMatchData
    d::MatchDataIn
end

getexpr(d::AbstractMatchData) = d.d._expr
getcap(d::AbstractMatchData) = d.d._capt
getstartind(d::AbstractMatchData) = d.d._indr.start
getstopind(d::AbstractMatchData) = d.d._indr.stop
setind(d::AbstractMatchData,i::Integer) = d.d._indr.start = i
incrind(d::AbstractMatchData) = d.d._indr.start += 1
getpat(d::AbstractMatchData) = d.d._pat

function setsearchrange(d::AbstractMatchData, r::UnitRange)
    d.d._indr.start = r.start
    d.d._indr.stop = r.stop
end


startsearchind(d::AbstractMatchData) = getstartind(d) + 1
stopsearchind(d::AbstractMatchData) = getstopind(d) + 1
firstel(d::AbstractMatchData) = d[startsearchind(d)]

Base.getindex(d::AbstractMatchData,i::Integer) =  getexpr(d)[i]
Base.length(d::AbstractMatchData) = length(getexpr(d))

matchdata(expr::Mxpr, pat) = matchdata(expr,pat,Capt())

function matchdata(expr::Mxpr, pat, capt::Capt)
    if isFlat(expr)
        if isOrderless(expr)
            MatchDataFO(expr,pat,capt)
        else
            MatchDataF(expr,pat,capt)
        end
    elseif isOrderless(expr)
        MatchDataO(expr,pat,capt)
    else
        MatchData(expr,pat,capt)
    end
end

_matchdatain(expr,pat,capt) = MatchDataIn(expr,SearchRange(0:0),pat,capt)

MatchData(expr,pat,capt) = MatchData(_matchdatain(expr,pat,capt))
MatchDataF(expr,pat,capt) = MatchDataF(_matchdatain(expr,pat,capt))
MatchDataO(expr,pat,capt) = MatchDataO(_matchdatain(expr,pat,capt))
MatchDataFO(expr,pat,capt) = MatchDataFO(_matchdatain(expr,pat,capt))

####

abstract type AbstractMatchIndex end

#abstract MatchIndexCompound <: AbstractMatchIndex

mutable struct MatchIndexSingle <: AbstractMatchIndex
    i::Int
end
    
mutable struct MatchIndexAbstractArray{T<:AbstractArray} <: AbstractMatchIndex
    a::T
end

MatchIndex(a::AbstractArray) = MatchIndexAbstractArray(a)
MatchIndex(a::Int) = MatchIndexSingle(a)

getstart(mi::MatchIndexAbstractArray) = start(mi.a)
getstop(mi::MatchIndexAbstractArray) = lastindex(mi.a)

getstart(mi::MatchIndexSingle) = mi.i
getstop(mi::MatchIndexSingle) = mi.i

####

abstract type AbstractMatchType end

mutable struct MatchOne{T<:AbstractMatchIndex} <: AbstractMatchType
    a::T
end

MatchOne(a) = MatchOne(MatchIndex(a))

mutable struct MatchNone <: AbstractMatchType
end

mutable struct MatchFail <: AbstractMatchType
end

struct MatchSequence{T<:AbstractMatchIndex} <: AbstractMatchType
    a::T
end

MatchSequence(a) = MatchSequence(MatchIndex(a))

struct MatchNullSequence{T<:AbstractMatchIndex} <: AbstractMatchType
    a::T
end

MatchNullSequence(a) = MatchNullSequence(MatchIndex(a))

getstart(mr::AbstractMatchType) = getstart(mr.a)
getstop(mr::AbstractMatchType) = getstop(mr.a)

function setsearchrange(d::AbstractMatchData, mr::AbstractMatchType)
    d.d._indr.start = getstart(mr)
    d.d._indr.stop = getstop(mr)     
end

function setsearchstop(d::AbstractMatchData, mr::AbstractMatchType)
    d.d._indr.stop = getstop(mr)
end

#### Matching 2

function matchrange(data::MatchData, pat::ABlank)
#    pmatch(pat,data[startsearchind(data)]) ?
    #       MatchOne(startsearchind(data)) : MatchFail()
    (j1,j) = _matchrange_blank(data,pat)
    return j1 == 0 ? MatchFail() : MatchOne(j1:j)
end

function _matchrange_blank(data,pat)
    local j1
    matchedone::Bool = false
    for i in startsearchind(data):stopsearchind(data)
        j1 = i
        if pmatch(pat,data[i])
            matchedone = true
            break
        end
    end
    matchedone || return (0,0)
    j = j1
    #    for i in j1+1:length(data)
    maxi = min(length(data),stopsearchind(data))
#    for i in j1+1:stopsearchind(data)
    for i in j1+1:maxi
        pmatch(pat,data[i]) || break
        j = i
    end
    return (j1,j)
end


function matchrange(data::MatchData, pat::ABlankSequence)
    (j1,j) = _matchrange_blanksequence(data,pat)
    return j1 == 0 ? MatchFail() : MatchSequence(j1:j)
end

function matchrange(data::MatchData, pat::ABlankNullSequence)
    (j1,j) = _matchrange_blanksequence(data,pat)
    return j1 == 0 ? MatchNone() : MatchNullSequence(j1:j)    
end

function _matchrange_blanksequence(data,pat)
    local j1
    matchedone::Bool = false
    for i in startsearchind(data):stopsearchind(data)
        j1 = i
        if pmatch(pat,data[i])
            matchedone = true
            break
        end
    end
    matchedone || return (0,0)
    j = j1
    for i in j1+1:length(data)
#    for i in j1+1:stopsearchind(data)
        pmatch(pat,data[i]) || break
        j = i
    end
    return (j1,j)
end

function advanceind(d::AbstractMatchData, mr::AbstractMatchType)
    nothing
end

function advanceind(d::AbstractMatchData, mr::MatchOne)
    setsearchrange(d,mr)
end

function advanceind(d::AbstractMatchData, mr::MatchSequence)
    setsearchrange(d,mr)
end

function advanceind(d::AbstractMatchData, mr::MatchNullSequence)
    setsearchstop(d,mr)
end

function matchranges(expr::Mxpr, pat)
    pat = topattern(pat)
    matchranges(matchdata(expr,pat))
end

function matchranges(d::AbstractMatchData)
    pat = getpat(d)
    a = Any[]
    for p in pat
        mr = matchrange(d,p)
        advanceind(d,mr)
        push!(a,mr)
    end
    a
end
