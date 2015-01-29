for v in ( "Set", "Pattern", "SetJ" )
    @eval begin
        set_attribute(symbol($v),:HoldFirst)
        set_attribute(symbol($v),:Protected)        
    end
end

for v in ("Module","Clear", "ClearAll", "SetDelayed", "HoldPattern", "Hold", "DumpHold",
          "DownValues")
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

for v in ("RuleDelayed","PatternTest")
    @eval begin
        set_attribute(symbol($v),:HoldRest)
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:SequenceHold)        
    end
end

for v in ("Timing","Allocated")
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

for v in ("CompoundExpression",)
    @eval begin
        set_attribute(symbol($v),:Protected)
        set_attribute(symbol($v),:ReadProtected)
        set_attribute(symbol($v),:HoldAll) 
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

for v in ("Apply","Dump", "Cos", "Length","Blank",
          "JVar", "MatchQ", "EvenQ", "OddQ", "Println",
          "Replace", "ReplaceAll","TraceOn","TraceOff","FullForm",
          "BI", "BF", "BuiltIns", "Symbol")
    @eval begin
        set_attribute(symbol($v),:Protected)        
    end
end
