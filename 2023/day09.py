# Day 9: Mirage
# Advent of Code 2023
# Saturday, December 09, 2023

# usage: python3 day09.py < day9.2023.input.txt

import sys

def extrapolate(lst):
    if lst.count(0) == len(lst):
        return 0
    diffs = [lst[i-1] - lst[i] for i in range(1, len(lst))]
    return lst[0] + extrapolate(diffs)

total = 0
for line in sys.stdin:
    nums = [int(x) for x in line.split()]
    nums.reverse()              # part 1, comment this line out for part 2
    total += extrapolate(nums)
print(total)
