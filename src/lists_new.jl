## Newer Table.
# Instead of localizing the iterating var iter in expr, we
# find and record the positions in expr where iter occurs
# and set all occurence in a loop each time before evaluating.
# Testing with the type NSYM, we see that a simpler structure
# can be copied much more quickly.
# It runs TableNew(a(i),[i,10^5]) twice as fast as the usual
# Table, but it is still slow. Probably creating new mxprs
# is expensive. But, older code had free sym lists and was
# faster. Still don't understand.

# This is also rather limited
#set_attribute(:TableNew, :HoldAll)
function apprules(mx::Mxpr{:Table})
    expr = mx[1]
    iter = mx[2]
    exprpos = expression_positions(expr,iter[1])
    imax = meval(iter[2])
    #    ex = replsym(deepcopy(expr),iter[1],isym) # takes no time, for simple expression
    ex = deepcopy(expr)
    args = do_table_new(imax,iter[1],ex,exprpos) # creating a symbol is pretty slow
    mx1 = mxpr(:List,args) # takes no time
#    mergesyms(mx1,:nothing) # not correct, but stops the merging
    setcanon(mx1)
    setfixed(mx1)
    return mx1
end

# Making this a kernel is not only useful, but faster.
# Set part in expr given by spec to val
function set_part_spec(expr,spec,val)
    p = expr
    @inbounds for k in 2:(length(spec)-1)
        p = p[spec[k]]
    end
    @inbounds p[spec[end]] = val
end

function set_all_part_specs(expr,specs,val)
    @inbounds for j in 1:length(specs)
        set_part_spec(expr,specs[j],val)
    end    
end


function do_table_new{T<:Integer}(imax::T,isym,exin::Mxpr,exprpos)
    args = newargs(imax)
    clearsyms(exin) # Clear the iterator variable
    ex = exin
    @inbounds for i in 1:imax
        #        ex = copy(exin)
        set_all_part_specs(ex,exprpos,i)
        unsetfixed(ex)   # force re-evaluation
        args[i] = doeval(ex)
#        args[i] = meval1(ex)
#        args[i] = meval(ex)
        setfixed(args[i])
        setcanon(args[i])
    end
    return args
end

# For symbols, either the iterator, or not.
function do_table_new{T<:Integer}(imax::T,isym,ex::SJSym,exprpos)
    args = newargs(imax)
    if isym == ex
        @inbounds for i in 1:imax
            args[i] = doeval(i)
            setfixed(args[i])
            setcanon(args[i])
        end
    else
        @inbounds for i in 1:imax
            args[i] = doeval(ex)
            setfixed(args[i])
            setcanon(args[i])
        end
    end
    return args
end

# ex is anything other than Mxpr or Symbol
function do_table_new{T<:Integer}(imax::T,isym,ex,exprpos)
    args = newargs(imax)
    @inbounds for i in 1:imax
        args[i] = ex
    end
    return args
end

## Return positions in ex at which subx is a subexpression
# Returns an array of arrays representing part specifications
# ie each array returned is a list of positions at levels.
# The first index refers to the top level and is always zero.
# It essentially means nothing unless the postion spec is
# just [0], in which case, it means the head matchs.
# We actually only know that this works in a few cases used
# by Table

function expression_positions(ex,subx)
    posns = Array(Array{Int,1},0)
    lev = Array(Int,10)
    clev::Int = 1
    lev[clev] = 0
    _expr_positions(ex,subx,lev,posns,clev)
    return posns
end


function _expr_positions(ex,subx,lev,posns,clev)
    if is_Mxpr(ex)
        args = margs(ex)
        @inbounds for i in 1:length(args)
            lev[clev+1] = i
            _expr_positions(args[i],subx,lev,posns,clev+1)
        end
        if mhead(ex) == subx  # this is only found in toplevel case
            lev[clev] = 0     # so, move it out of here.
            nlev = copy(lev)
            push!(posns,slice(nlev,1:clev))
        end
    end
    if ex == subx
        nlev = copy(lev)
        push!(posns,slice(nlev,1:clev))
    else
        nothing
    end
end