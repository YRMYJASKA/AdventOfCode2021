from functools import reduce
inputs = []
with open("input.txt", "r") as f:
    for l in f:
        ll = [9]
        for i in l.strip():
            ll.append(int(i))
        ll.append(9)
        inputs.append(ll)
    inputs.append([9]*len(inputs[0]))
    inputs = [[9]*len(inputs[0])] + inputs

def getLowPoints(data):
    l = len(data)
    w = len(data[0])
    general = [(y, x)*(data[y][x] < data[y+1][x] and data[y][x] < data[y][x-1]
               and data[y][x] < data[y][x+1] and data[y][x] < data[y-1][x])
               for x in range(1, w-1) for y in range(1, l-1)]
    lowPoints = filter(lambda x: x != (), general)
    return lowPoints
        
def part1(data):
    lowPoints = getLowPoints(data)
    return sum(map(lambda x: data[x[0]][x[1]] + 1, lowPoints))

def calcBasin(data, pt, pts):
    if data[pt[0]][pt[1]] == 9 or pt in pts:
        return pts
    neighbours = [tuple(map(sum, zip(pt, n)))
                  for n in [(0,1), (0,-1), (1,0), (-1,0)]]
    pts.add(pt)
    pts1 = calcBasin(data, neighbours[0], pts)
    pts2 = calcBasin(data, neighbours[1], pts1)
    pts3 = calcBasin(data, neighbours[2], pts2)
    pts4 = calcBasin(data, neighbours[3], pts3)
    sets = [pts, pts2, pts3, pts4]
    return reduce(lambda a, b: a.union(b), sets)


def part2(data):
    lowPoints = getLowPoints(data)
    basins = map(lambda x: calcBasin(data, x, set()), lowPoints)
    sizes = list(map(len, basins))
    sizes.sort()
    return sizes[-1]*sizes[-2]*sizes[-3]


# print(inputs)

if __name__ == "__main__":
    print(part1(inputs))
    print(part2(inputs))
            
