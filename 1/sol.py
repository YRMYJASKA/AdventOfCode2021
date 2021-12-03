import numpy as np
from functools import reduce

# More general solution for part 2
# Pre-condition: len(inputs) >= 3
def general_sol(inputs, n):
    shifted = np.roll(inputs, -n)
    result = 0
    return sum(map(lambda x : (x > 0), (shifted - inputs)[:-n]))
#   for k in (shifted - inputs)[:-n]:
#       if k > 0:
#           result += 1
#   return result

# read file and start the solution
inputs = np.array([])
with open("input.txt", "r") as f:
    for lines in f:
        num = int(lines)
        inputs = np.append(inputs, num)

print("Part 1:", general_sol(inputs, 1))
print("Part 2:", general_sol(inputs, 3))
