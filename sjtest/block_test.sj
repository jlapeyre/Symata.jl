ex = begin
    1;
    2
end

T ex == 2

ex = begin
    1,
    2
end

T ex == 2

ClearAll(ex)

# The follwing do work at command line
# ex1 = [
# 1
# 2
# 3
# ]
# ex = begin
#      1
#      2
#  end
