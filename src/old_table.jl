## This is the OLD version of Table. We are using the version in lists_new.jl

# We only do Table(expr,[i,imax])
# Our Table is rather slow. Slower than Maxima makelist.
# Table(a(i),[i,10000]) is 2 to 3 x slower than makelist( i, i, 1, 10000)
# If we cheat and only do a single evaluation,
# and setfixed() then the speed is the same.
# But, the 3rd party 'table' for Maxima is 4 times faster than makelist
# in this example.
# Mma 3.0  Timing[mm = Table[a[i],{i,10^5}];]  {0.05 Second, Null}
# 3rd pary maxima: table(a(i),[i,10^5]);  0.14 s
# At commit 3478f83ee403238704ad6af81b42e1c7f4d07cc8
# Table(a(i),[i,10000]) takes bout 0.45--0.49 s, makelist reports 0.503.

set_attribute(:TableOld, :HoldAll)

function apprules(mx::Mxpr{:TableOld})
    expr = mx[1]
    iter = mx[2]
    isym = get_localized_symbol(iter[1])
    imax = meval(iter[2])
#    issym = createssym(isym,Int)  ## Trying out Typed variable
    ex = replsym(deepcopy(expr),iter[1],isym) # takes no time, for simple expression
    args = do_table(imax,isym,ex) # creating a symbol is pretty slow
    removesym(isym)
    mx1 = mxpr(:List,args) # takes no time
    # This mergesyms no longer makes this much faster, because overall Table has slowed by more than 5x
    # syms are being merged alot somewhere else
    mergesyms(mx1,:nothing) # not correct, but stops the merging
    setcanon(mx1)
    setfixed(mx1)
    return mx1
end

# These tests concern not Table per se, but the efficiency
# of evaluation, in particular assigning values to symbols.
# commit 3478f83ee403238704ad6af81b42e1c7f4d07cc8
# Testing Table(a(i),[i,10^5])
# changes                   Time
# normal                    0.49
# disable setsymval         0.35
# 
# commit 0d2332b17eb7e5c518a86b1b3044b691fddb5b87
# setsymval is now much faster because we changed data structure in SSJSym
# Testing Table(a(i),[i,10^5])
# normal                    0.36
# *sigh* Cannot reproduce the performance in the previous line. Even if
# I return to that commit. Time is closer to 0.5 than 0.35
# setsymval still seems to be an expensive operation
#
# 4f7d9f6dff207ae2c95736ab058627f503fbad26
# With this commit, Time is 1.83 s. For same code as above.  Table is
# becoming slower. We try using a stripped-down meval1 from
# alteval.jl, time is .43 with no gc.

function do_table{T<:Integer}(imax::T,isym,ex)
    args = newargs(imax)
    sisym = getssym(isym)
#    setsymval(sisym,1)    
    @inbounds for i in 1:imax
        setsymval(sisym,i)
#        args[i] = meval(ex)
        args[i] = doeval(ex)
#        args[i] = meval1(ex)
        setfixed(args[i])  # no difference ?
    end
    return args
end

# The speed penalty for val::Any instead of val::Int, is factor of 10^4.
# Setting isym.val is by far the slowest step in Table(i,[i,10^6])
# Much slower than Mma and SBCL
# If we us val::Array{Any,1}, the speed penalty is better by a couple
# orders of magnitude

# type NSYM
# #    val::Int
#     val::Array{Any,1}
# end

# function testset()
#     ss = NSYM(Any[0])
#     sum0 = 0
#     for i in 1:10^6
#         ss.val[1] = i
#         sum0 += ss.val[1]
#     end
#     sum0
# end
