## These are the 'builtin' symbols
# They can by listed by giving `BuiltIn()'.
# They all have the attribute `Protected'.

# This file is loaded last because we are polluting Julia symbol table
# with symbols in order to get completion at the repl.  Each of these
# symbols will be bound in Julia to true, unless already bound.  We
# want to allow that they be bound to functions, instead, in files
# loaded before this one.

function set_pattributes{T<:AbstractString}(syms::Array{T,1},attrs::Array{Symbol,1})
    for s in syms
        for a in attrs
            set_attribute(symbol(s),a)
        end
        set_attribute(symbol(s),:Protected)  # They all are Protected
    end
end

set_pattributes(["Pattern", "SetJ", "SetAttributes"],
               [:HoldFirst])


# Age, Fixed and others are not Mma
for v in ("Module","LModule","Clear", "ClearAll", "HoldPattern", "HoldForm", "Hold", "DumpHold",
          "DownValues", "HAge", "Table", "For", "If", "While", "Do", "Jxpr", "Unprotect") 
    @eval begin
        set_attribute(symbol($v),:HoldAll)
        set_attribute(symbol($v),:Protected)
    end
end

for v in ("Attributes",)
    @eval begin
        set_attribute(symbol($v),:HoldAll)
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:Listable)        
    end
end

for v in ("Rule",)
    @eval begin
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:SequenceHold)        
    end
end

for v in ("Set",)
    @eval begin
        set_attribute(symbol($v),:HoldFirst)        
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:SequenceHold)        
    end
end

for v in ("RuleDelayed","PatternTest")
    @eval begin
        set_attribute(symbol($v),:HoldRest)
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:SequenceHold)        
    end
end

for v in ("Timing","Allocated","SetDelayed")
    @eval begin
        set_attribute(symbol($v),:HoldAll)
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:SequenceHold)        
    end
end

for v in ("Pi","E")
    @eval begin
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:ReadProtected)
        set_attribute(symbol($v),:Constant)
    end
end

for v in ("I")
    @eval begin
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:ReadProtected)
        set_attribute(symbol($v),:Locked)
    end
end

for v in ("Pi","E")
    @eval begin
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:ReadProtected)  # In Mma E is not ReadProtected
        set_attribute(symbol($v),:Constant)
    end
end

for v in ("CompoundExpression","Sum")
    @eval begin
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:ReadProtected)
        set_attribute(symbol($v),:HoldAll) 
    end
end

for v in ("Part",)
    @eval begin
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:ReadProtected)        
    end
end

for v in ("Cos", "ACos", "Sin","Tan","Cosh","Sinh","Log","Minus","Abs")  # etc.
    @eval begin    
        set_attribute(symbol($v),:Listable)
        set_attribute(symbol($v),:NumericFunction)
        set_attribute(symbol($v),:Protected)        
    end
end

for v in ("Plus", "Times")
    @eval begin
        set_attribute(symbol($v),:Flat)
        set_attribute(symbol($v),:Listable)
        set_attribute(symbol($v),:NumericFunction)
        set_attribute(symbol($v),:OneIdentity)
        set_attribute(symbol($v),:Orderless)
        set_attribute(symbol($v),:Protected)
    end
end


for v in ("Power",)
    @eval begin
        set_attribute(symbol($v),:Listable)
        set_attribute(symbol($v),:NumericFunction)
        set_attribute(symbol($v),:OneIdentity)
        set_attribute(symbol($v),:Protected)
    end
end

for v in ("EvenQ","OddQ","Range")
    @eval begin
        set_attribute(symbol($v),:Listable)
        set_attribute(symbol($v),:Protected)        
    end
end

# Of course, these need to be organized
for v in ("Age","Apply","Dump", "Length","Blank","BlankSequence","BlankNullSequence",
          "JVar", "MatchQ", "AtomQ", "Println","Keys",
          "Replace", "ReplaceAll","TraceOn","TraceOff","FullForm", "Expand",
          "BI", "BF", "BuiltIns", "Symbol", "Pack", "Unpack","Example","Fixed",
          "UserSyms", "List","Syms",
          "Comparison", "DirtyQ", "Flat", "Listable", "Head", "Integer",
          "Orderless","NumericFunction","OneIdentity", "!=", "//", ">","==",
          "<", "<=","nothing","N",
          "String", "StringLength","Protected", "TimeOn", "TimeOff"
          )
    set_attribute(symbol(v),:Protected)
end

@sjdoc I "
I represents the imaginary unit
"

@sjdoc E "
E is the base of the natural logarithm
"

@sjdoc Pi "
Pi is the trigonometric constant π.
"
