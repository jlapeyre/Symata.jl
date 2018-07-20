include("./src/version.jl")

vers = string("v", SYMATA_VERSION)
com = `git tag $vers`
run(com)
