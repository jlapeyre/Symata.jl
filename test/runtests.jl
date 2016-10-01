using SJulia
using Base.Test

import Base.Test: @test

@ex TimeOff()   # don't print hundreds of diagnostic lines

function runtests()
    eval(parse("@ex Tests()"))
end

@test (runtests() ; true)
