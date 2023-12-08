#!/usr/bin/awk -f
# Advent of Code 2023
# Friday, December 08, 2023

# just playing around to see what an awk solution looks like

BEGIN {
    FS = "[^A-Z0-9]+"
}

NR == 1 {
    split($0, lr, "")
}
    
NR > 2 {
    left[$1] = $2
    right[$1] = $3
}

END {
    n = 0
    location = "AAA"
    done = 0
    while (! done) {
	for (i=1; i<=length(lr); i++) {
	    n = n + 1
	    if ("L" == lr[i])
		location = left[location]
	    else
		location = right[location]
	    if (location == "ZZZ") {
		done = 1
		break
	    }
	}
    }
    print n
}
    
