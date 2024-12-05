#          --------Part 1--------   --------Part 2--------
#    Day       Time   Rank  Score       Time   Rank  Score
#       4   01:03:10  10952      0   01:26:58  10381      0

#f = open("day4.test.input.txt")
f = open("day4.2024.input.txt")
dat = readlines(f)
close(f)
A = stack(dat);

count = 0

# Part 1

## let's try generating indexes for each direction

function search_horz(fun, A)
    rows, cols = size(A)
    for r in 1:rows
        for c in 1:cols - 3
            fun(A[r,c:c+3])
        end
    end
end

function search_diag(fun,A)
    rows,cols = size(A)
    for r in 1 : rows - 3
        for c in 1 : cols - 3
            v = [A[r,c],A[r+1,c+1],A[r+2,c+2],A[r+3,c+3]]
            fun(v)
        end
    end
end

function xsearch(v)
    global count
    if v == ['X','M','A','S']
        count += 1
    elseif v == ['S','A','M','X']
        count += 1
    end
end

count = 0
function search_xmas(A)
    global count
    count = 0
    search_horz(xsearch, A)
    search_horz(xsearch, rotl90(A))
    search_diag(xsearch, A)
    search_diag(xsearch, rotl90(A))
    count
end

search_xmas(A)


# Part 2

function search_mas(fun, A)
    rows, cols = size(A)
    for r in 1:rows-2
        for c in 1:cols-2
            ary = A[r:r+2,c:c+2]
            fun(ary)
        en
    end
end

function corners(A, a, b, c, d)
    global count
    if A[1,1] == a && A[3,1] == b && A[1,3] == c && A[3,3] == d
        count += 1
    end
end

function is_xmas(A)
    if A[2,2] == 'A'
        corners(A, 'M', 'S', 'M', 'S')
        #corners(A, 'S', 'M', 'M', 'S')  # invalid
        corners(A, 'S', 'M', 'S', 'M')
        #corners(A, 'M', 'S', 'S', 'M')  # invalid
        corners(A, 'M', 'M', 'S', 'S')
        corners(A, 'S', 'S', 'M', 'M')
    end
end

count = 0
search_mas(is_xmas, A)
count
