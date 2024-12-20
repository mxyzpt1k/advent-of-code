# Advent of Code
# December 6, 2025

# This was originally created in a Jupyter notebook, so some of the
# lonely variables are the output of cells.

# For part 1, I decided to rotate the map instead of keeping track
# of direction when the guard turned right. That took more time than
# I thought to get right.

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#   6   01:25:57  12173      0   01:48:07   6770      0


f = open("day6.2024.input.txt")
#f = open("day6.test")
lines = readlines(f)
close(f)
Original = reverse(rotr90(stack(lines)), dims=2)  # bleah
A = copy(Original);

rows,cols = size(A)
start = undef
for r =1:rows
    for c = 1:cols
        if A[r,c] == '^'
            start = (r,c)
        end
    end
end
start   # cell output

## part 1

function mark(A,row,col)
    if A[row,col] == '.'
        A[row,col] = '@'
    end
end

function solve(A,start)
    finished = false
    rows,cols=size(A)
    p = start
    k = 0
    while k < 100000  # arbitrary limit, but it seemed like enough
        k += 1
        row, col = p
        f = A[row-1,col]
        if f == '%'  # a border
            mark(A,row,col)
            finished = true
            break
        elseif f == '#'  # an obstacle
            A = rotl90(A)
            p = (rows-col+1, row)
        else
            mark(A,row,col)
            p = (row-1, col)
        end
    end
    (A, finished)
end

A, finished = solve(A, start)
n = 0
rows,cols = size(A)
for row = 1:rows
    for col = 1:cols
        if A[row,col] == '@'
            n += 1
        end
    end
end
n+1  # cell output and the answer to part 1

## part 2  (simple brute force)

rows, cols = size(A)
n = 0
for row = 2:rows-1
    for col = 2:cols-1
        if Original[row,col] == '.'
            A = copy(Original)
            A[row,col] = '#'
            A, finished = solve(A,start)
            if !finished
                n += 1
            end
        end
    end
end
n  # cell output and the answer to part 2


