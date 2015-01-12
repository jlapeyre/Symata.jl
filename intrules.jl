rules = Array(PRule,0)

push!(rules, @rule Int(1/x_, x_::Symbol) => Log(x_))
push!(rules, @rule Int(x_^m_::((x)->(x+1>0)),
                       x_::Symbol) => x^(m_+1) / (m_+1))

