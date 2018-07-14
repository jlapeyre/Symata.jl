import Symata: mxpra, MxprArgs

let mx = mxpra(:List, Any[1,2,3])
    @test  mhead(mx) == :List
    @test  margs(mx) == Any[1,2,3]
    @test  typeof(margs(mx)) == MxprArgs
end

