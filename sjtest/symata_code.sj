cosfixedpoint(x0_AbstractFloat) :=
       Module( [x=x0, i=0],
               begin
               While( Chop(Cos(x) -x ) != 0,
                      (x = Cos(x); Increment(i); If(i>1000, (Println("Too many iterations"); Break()))   ))
               [x,i]
               end)

[Chop(cosfixedpoint(1.0)[1] - 0.7390851332151662), Head(cosfixedpoint(0))]
