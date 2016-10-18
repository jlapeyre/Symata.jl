using Symata
using Base.Test

import Base.Test: @test

@Symata.ex TimeOff()   # don't print hundreds of diagnostic lines

# For debugging
#@Symata.ex VersionInfo()

function runtests()
    eval(parse("@ex Tests()"))
end

@test (runtests() ; true)
