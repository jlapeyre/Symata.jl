# Test time-efficiency of creating Mxpr's

# Main result of this test is that creating a free symbol Dict with
# each expression slows creation by about 40%. This is for an
# expression with no, or few arguements. This will be important when
# creating many small expressions so that creation time dominates
# efficiency.

# With maximum intelligence, we can create a lot of small expressions.
# in 2 or 3 times the time of Mma 3. In some simple situations, we are already
# much faster. I don't know why Mma can't detect this. Eg.
# Table(0.0,[i,n]) is 4 times faster in SJulia than Mma 3. But we
# create no subexpression here. This suggests that Mma 3 takes no
# time to create expressions. Maybe it maintains a pool. We could
# try maintaining a pool, but our Mxpr is subtyped for each Head.
# So we can't produce and use generic Mxprs.

# Yet another test:
# Careful, sometimes reloading alteval.jl
# is not enough after changes. We need to restart julia.

# Ok, best to copy output.
# Using meval1 in alteval.jl with only meval1 of subexpressions:
# a = Table(s*b,[i,10^4]);  time: 0.014919181 seconds
# Using meval1 with up/down values/ merge_args_if_empty --  0.032s
# doing flatcanon! as well -- 0.06s.
# Using full meval, once  -- 0.08s
# using full doeval:
# Table(s*b,[i,10^4]); time: 0.179446132 
# Timing[a=Table[s(b),{i,10^4}];]  --> {0.011 Second, Null}

# Tests done after commit:
# commit 3645cad341f248968c384636f2a5b724f7a25b31
# Date:   Thu Feb 19 13:21:25 2015 +0100

# We make a new type TMxpr that is the same as Mxpr and then modify
# it to test efficiency.
# Removing the field syms::FreeSyms makes creation run in 72% of the
# time as without it.

# Creates a Julia array of n Mxpr's  s(1,2,3) with no evaluation
# disable gc first
# n = 10^5  0.06s
# Mma 3 does this with Table in 0.03s on a slower processor
# for n=10^4 -- 10^5, time is roughly linear for both Mma and Julia
# Creating array of expressions s() instad of s(1,2,3) is slightly faster in Julia
# Mma does s[] and s[1,2,3] at same speed.

# Creating an array of s() with Table is 23 times slower in SJulia.
# I guess with some basic very basic optimization, we could increase improve this
# by a factor of 5. Eg. we can use the expensive symbol lists to see that
# s() will not evaluate to anything. There still may be an unforseen problem
# with this in the future.

# Table[s[i],{i,10^4}] makes Mma run at about the same speed as the arraytest
# below with mxpr(:s,i). No surprise, we do no evaluation.
# However, even if there is a downrule for one value of i in s[i], in Mma 3,
# say s[100] := 1, Table runs no slower. I think the downrule is very efficient.
# In SJulia,  s(100) := 1, causes a downrule to be invoked for each i and the Table
# runs almost twice as slow.

function arraytest(n)
    arr = Array(Any,0)
    for i in 1:n
        push!(arr,mxpr(:s,i))
    end
    arr
end

function tarraytest(n)
    arr = Array(Any,0)
    for i in 1:n
        push!(arr,tmxpr(:s,i))
    end
    arr
end

## Here we copy Mxpr type and creation and modify it

# Test mxpr
type TMxpr{T}
    head::Any          # making this Any instead of SJSym slows things a bit
    args::MxprArgs
    fixed::Bool
    canon::Bool
#    syms::FreeSyms
    age::UInt64
    key::UInt64
    typ::DataType
end

typealias TFreeSyms Dict{Symbol,Bool}
tnewsymsdict() = TFreeSyms() # Dict{Symbol,Bool}()  # create dict for field syms of Mxpr

@inline setage(tmx::TMxpr) = tmx.age = increvalage()

@inline function tmxpr(s::SJSym,iargs...)
    args = newargs()
    for x in iargs push!(args,x) end
#    mx = TMxpr{symname(s)}(s,args,false,false,tnewsymsdict(),0,0,Any)
    mx = TMxpr{symname(s)}(s,args,false,false,0,0,Any)    
    setage(mx)
    mx
end
