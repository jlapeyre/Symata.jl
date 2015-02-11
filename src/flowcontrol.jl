# Not super-fast.

@sjdoc For "
For(start,test,incr,body) is a for loop. Eg. For(i=1,i<=3, i = i + 1 , Println(i))
"

function apprules(mx::Mxpr{:For})
    (start,test,incr)= (mx[1],mx[2],mx[3])
    body = nothing
    if isdefined(margs(mx),4)
        body = mx[4]
    end
    doeval(start)
    while doeval(test)
        doeval(body)
        doeval(incr)
    end
end

@sjdoc If "
If(test,tbranch,fbranch) evaluates test and if the result is true, evaluates tbranch, otherwise fbranch
"

function apprules(mx::Mxpr{:If})
    (test,tbranch)= (mx[1],mx[2])
    fbranch = false
    if length(mx) == 3
        fbranch = mx[3]
    end
    tres = doeval(test) == true
    tres ? doeval(tbranch) : doeval(fbranch)
end

@sjdoc While "
While(test,body) evaluates test then body in a loop until test does not return true.
"

function apprules(mx::Mxpr{:While})
    (test,body)= (mx[1],mx[2])
    while doeval(test) == true
        doeval(body)
    end
end

function apprules(mx::Mxpr{:Do})
    expr = mx[1]
    iter = make_sjiter(mx[2])
    do_doloop(expr,iter)
end

function do_doloop(expr,iter::SJIter1)
    for i in 1:iter.imax
        doeval(expr)
    end
end

function do_doloop2(expr,incr)
    incr = one(incr)
end

#    isym = gensym(string(iter.imax))
#    ex = replsym(deepcopy(expr),iter.imax,isym)
