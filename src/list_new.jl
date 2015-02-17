## This Table is not to be loaded, but is for experimenting.
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
function apprules(mx::Mxpr{:TableNew})
    expr = mx[1]
    iter = mx[2]
    isym = get_localized_symbol(iter[1])
    exprpos = expression_positions(expr,iter[1])
    imax = meval(iter[2])
#    issym = createssym(isym,Int)  ## Trying out Typed variable
    ex = replsym(deepcopy(expr),iter[1],isym) # takes no time, for simple expression
    args = do_table_new(imax,isym,ex,exprpos) # creating a symbol is pretty slow
    removesym(isym)
    mx1 = mxpr(:List,args) # takes no time
#    mergesyms(mx1,:nothing) # not correct, but stops the merging
    setcanon(mx1)
    setfixed(mx1)
    return mx1
end

function do_table_new{T<:Integer}(imax::T,isym,exin,exprpos)
    args = newargs(imax)
    sisym = getssym(isym)
    #    setsymval(sisym,1)  # the old way to do it.
    ex = exin
    clearsyms(ex) # Clear the iterator variable
#    ss = NSYM(Any[1:10])  # the arg nonsense here.
    @inbounds for i in 1:imax
#        ss1 = copy(ss)
        for j in 1:length(exprpos)  # exprpos is a list of positions at which the itvar occurs in ex
            ispec = exprpos[j]
            p = ex
            for k in 2:length(ispec)
                p = ex[ispec[k]]
            end
            ex[ispec[end]] = i
        end
        unsetfixed(ex)   # force re-evaluation
        args[i] = doeval(ex)
        setfixed(args[i])
        setcanon(args[i])
    end
    return args
end

# Test code.
# The speed penalty for val::Any instead of val::Int, is factor of 10^4.
# Setting isym.val is by far the slowest step in Table(i,[i,10^6])
# Much slower than Mma and SBCL
# If we us val::Array{Any,1}, the speed penalty is better by a couple
# orders of magnitude

type NSYM
#    val::Int
    val::Array{Any,1}
end

Base.copy(x::NSYM) = NSYM(copy(x.val))

# function testset()
#     ss = NSYM(Any[0])
#     sum0 = 0
#     for i in 1:10^6
#         ss.val[1] = i
#         sum0 += ss.val[1]
#     end
#     sum0
# end

## Return positions in ex at which subx is a subexpression
# Returns an array of arrays representing part specifications
# ie each array returned is a list of positions at levels.
# The first index refers to the top level and is always zero.
# It essentially means nothing unless the postion spec is
# just [0], in which case, it means the head matchs.

function expression_positions(ex,subx)
    posns = Array(Any,0)
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
