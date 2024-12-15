

input = "/Users/marti/Downloads/day12.2024.input.txt"
grid = []
rows = 0
cols = 0
with open(input) as fh:
    for line in fh:
        grid.append( list(line.strip()) )
        rows += 1
        cols = len(line) - 1
print([rows,cols])

def getsquare(y,x):
    try:
        return grid[y][x]
    except:
        return -1

def fill(c,y,x,id):
    def helper(y,x):
        if getsquare(y,x) == c:
            grid[y][x] = id
            fill(c,y,x,id)
    helper(y+1,x)
    helper(y-1,x)
    helper(y,x+1)
    helper(y,x-1)

id = 1
for y in range(rows):
    for x in range(cols):
        c = grid[y][x]
        if type(c) == type('A'):
            fill(c,y,x,id)
            id += 1

print(grid)
