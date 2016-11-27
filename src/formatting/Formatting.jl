module Formatting

    import Base.show

    export
        FormatSpec, FormatExpr,
        printfmt, printfmtln, fmt, format,
        sprintf1, generate_formatter

    using Compat

    include("cformat.jl" )
    include("fmtspec.jl")
    include("fmtcore.jl")
    include("formatexpr.jl")

end # module
