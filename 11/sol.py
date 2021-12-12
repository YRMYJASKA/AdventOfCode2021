import numpy as np
from functools import reduce
from pprint import pprint

def part1(grid, steps):
    dim = len(grid)
    total_flashes = 0
    def flash_at(_grid, iy, ix, flashes):
        if iy >= dim or iy < 0 or ix <0 or ix >=dim:
            return _grid, flashes
        val_at = _grid[iy][ix]
        if val_at == False:
            return _grid, flashes
        elif val_at > 9:
            _grid[iy][ix] = False
            flashes += 1
            # recurse
            nextPts = [(iy+y, ix+x) for x in [-1,0,1] for y in [-1,0,1]]
            newGrid = _grid
            for ny, nx in nextPts:
                newGrid, flashes = flash_at(newGrid, ny, nx, flashes)
            return newGrid, flashes
        else:
            _grid[iy][ix] += 1
            if val_at + 1 > 9:
                return flash_at(_grid, iy, ix, flashes)
            return _grid, flashes
            
    flash_counts = []
    # Actual execution
    for _ in range(steps):
        flashes_at_step = 0
        for iy, ix in np.ndindex((dim, dim)):
            grid[iy][ix] += 1
        # Compute flash
        for iy, ix in np.ndindex((dim, dim)):
            if grid[iy][ix] > 9:
                grid,flashes_at_step= flash_at(grid, iy, ix, flashes_at_step)
                 
        for iy, ix in np.ndindex((dim, dim)):
            if grid[iy][ix] == False:
                grid[iy][ix] = 0
        flash_counts.append(flashes_at_step)
        #print("step", s+1)
        #pprint(grid)
    return flash_counts

def part2(grid):
    dim = len(grid)
    total_flashes = 0
    def flash_at(_grid, iy, ix, flashes):
        if iy >= dim or iy < 0 or ix <0 or ix >=dim:
            return _grid, flashes
        val_at = _grid[iy][ix]
        if val_at == False:
            return _grid, flashes
        elif val_at > 9:
            _grid[iy][ix] = False
            flashes += 1
            # recurse
            nextPts = [(iy+y, ix+x) for x in [-1,0,1] for y in [-1,0,1]]
            newGrid = _grid
            for ny, nx in nextPts:
                newGrid, flashes = flash_at(newGrid, ny, nx, flashes)
            return newGrid, flashes
        else:
            _grid[iy][ix] += 1
            if val_at + 1 > 9:
                return flash_at(_grid, iy, ix, flashes)
            return _grid, flashes
            
    # Actual execution
    s = 0
    while True:
        s += 1
        flashes_at_step = 0
        for iy, ix in np.ndindex((dim, dim)):
            grid[iy][ix] += 1
        # Compute flash
        for iy, ix in np.ndindex((dim, dim)):
            if grid[iy][ix] > 9:
                grid,flashes_at_step= flash_at(grid, iy, ix, flashes_at_step)
                 
        for iy, ix in np.ndindex((dim, dim)):
            if grid[iy][ix] == False:
                grid[iy][ix] = 0
        if flashes_at_step == dim*dim:

            return s

if __name__ == "__main__":
    
    file_input = []
    with open("input.txt","r") as f:
        for l in f:
            file_input.append(list(map(int, l.rstrip())))
    print(sum(part1(file_input, 100)))
    print(part2(file_input) + 100)
