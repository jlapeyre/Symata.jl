## These are the 'builtin' symbols
# They can by listed by giving `BuiltIn()'.
# They all have the attribute `Protected'.

for v in ("Pattern", "SetJ", "SetAttributes") #  "HPart", "HSetPart")
    @eval begin
        set_attribute(symbol($v),:HoldFirst)
        set_attribute(symbol($v),:Protected)        
    end
end

# Age, Fixed and others are not Mma
for v in ("Module","LModule","Clear", "ClearAll", "HoldPattern", "HoldForm", "Hold", "DumpHold",
          "DownValues", "HAge", "Table", "For", "If", "While", "Do", "Jxpr") 
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

for v in ("Cos", "ACos", "Sin","Tan","Cosh","Sinh","Log","Minus")  # etc.
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

for v in ("Age","Apply","Dump", "Length","Blank","BlankSequence","BlankNullSequence",  # "SetPart"
          "JVar", "MatchQ", "AtomQ", "Println",
          "Replace", "ReplaceAll","TraceOn","TraceOff","FullForm", "Expand",
          "BI", "BF", "BuiltIns", "Symbol", "Pack", "Unpack","Example","Fixed",
          "UserSyms", "List","Syms",
          "Comparison", "DirtyQ", "Flat", "Listable", "Head", "Integer",
          "Orderless","NumericFunction","OneIdentity", "!=", "//", ">","==",
          "<", "<=","nothing",
          "String", "StringLength","Protected", "TimeOn", "TimeOff"
    )
    @eval begin
        set_attribute(symbol($v),:Protected)        
    end
end
