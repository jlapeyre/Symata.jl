@sjdoc Position "
Position(expr,x) returns a list of part specifications of positions in
expr at which x occurs. Only literal values for x are supported, not
patterns
"

@mkapprule Position

function do_Position(mx::Mxpr{:Position},expr,subx)
    ps = find_positions(expr,subx)
    nargs = newargs(ps)
    for i in 1:length(ps)
        nargs[i] = mxpr(:List,ps[i]...) # maybe not fastest way.
    end
    mxpr(:List,nargs)
end

# return a list of part specs at which subx occurs in ex
function find_positions(ex,subx)
    posns = Array(Array{Int,1},0)
    lev = Array(Int,100)
    clev::Int = 1
    lev[clev] = 0
    _find_positions(ex,subx,lev,posns,clev)
    return posns
end

function _find_positions(ex::Mxpr,subx,lev,posns,clev)
    if clev > length(lev) push!(lev,0) end
    args = margs(ex)
    @inbounds for i in 1:length(args)
        lev[clev] = i
        _find_positions(args[i],subx,lev,posns,clev+1)
        end
    if mhead(ex) == subx
        lev[clev] = 0
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev))
    end
    if ex == subx
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev-1))
    end
end

function _find_positions(ex,subx,lev,posns,clev)
    if ex == subx
        nlev = copy(lev)
        push!(posns,view(nlev,1:clev-1))
    end
end
