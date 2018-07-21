# cosfixedpoint(x0_AbstractFloat) :=
#        Module( [x=x0, i=0],
#                begin
#                While( Chop(Cos(x) -x ) != 0,
#                       (x = Cos(x); Increment(i); If(i>1000, (Println("Too many iterations"); Break()))   ))
#                [x,i]
#                end)

# NOTE: The version above works in Julia 0.5 and nightly.
# It fails in Julia 0.4. The [x,i] in the compound statement is not read correctly
# from a file. Cutting and pasting, to the REPL still works.
# It may not have been the switch from (x,x) to begin x  x end that broke this test.
cosfixedpoint(x0_AbstractFloat) :=
       Module( [x=x0, i=0],
               (
               While( Chop(Cos(x) -x ) != 0,
                      (x = Cos(x); Increment(i); If(i>1000, (Println("Too many iterations"); Break()))   )),
               [x,i]
               ))

[Chop(cosfixedpoint(1.0)[1] - 0.7390851332151662), Head(cosfixedpoint(0))]
