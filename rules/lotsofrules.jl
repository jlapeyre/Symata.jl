r1 = @rule  Log(x_^n_) => n_ * Log(x_)
r2 = @rule  Log(x_ * y_) => Log(x_) + Log(y_)
r2a = @rule Log(1) => 0
r3 = @rule  _  + _ => 2 * _
r4 = @rule  x_  + n_ * x_ => (n_+1) * x_
r5 = @rule  n_ * x_ + x_ => (n_+1) * x_
r6 = @rule  n_::Number  * (m_::Number * x_) => (n_*m_) * x_
r6a = @rule n_::Number * x_ - m_::Number * x_ => (n_-m_) * x_
r7 = @rule  +(x_,x_,x_) => 3 * x_
r8 = @rule  x_ - x_  => 0
r9 = @rule  0 + x_  => x_
r10 = @rule  1/(1/x_) => x_
r10a = @rule  x_ / x_ => 1
r11 = @rule  x_ + -x_  => 0
r12 = @rule  x_^n1_ * x_^n2_ => x_^(n1_+n2_)
r12a = @rule  x_  * x_^n_ => x_^(1+n_)
r13 = @rule  x_^0 => 1
r14 = @rule  0 * x_ => 0
r15 = @rule  x_ * 0  => 0
r16 = @rule  1 * _ => _
r16a = @rule  _ * 1  => _



lamrules = [
            @rule( LambertW(x_) * exp(LambertW(x_)) => x_),
            @rule(  LambertW(x_*exp(x_)) => x_)
            ]
#            @rule  Log(x_/LambertW(x_))  => LambertW(x_)

rules = [r1,r2,r2a,r3,r4,r5,r6,r6a,r7,r8,r9,r10,r10a,r11,r12,r12a,
         r13,r14,r15,r16,r16a,lamrules]
