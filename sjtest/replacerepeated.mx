# This SJulia code is for testing the algorithm
# for ReplaceRepeated.

replacerepeated(ex_, rules_) :=
    Module([res,res1,i,maxits = 100],
           (res = ReplaceAll(ex,rules);
            For(i=1, i<= maxits, i += 1,  (res1 =  ReplaceAll(res,rules);
                                           res = res1;
                                           If(res1 == res, Break())));
            If(i == maxits, Warn(StringJoin("maxits reached ",ToString(i))));
            Return(res)))
