# day15.jl - advent of code 2015
# Wednesday, November 22, 2023
# warming up for 2023

function day15()
  # cookie input translated
  A = [ 2 0 -2 0 ;
        0 5 -3 0 ;
        0 0 5 -1 ;
        0 -1 0 5 ]'

  cals = [3 3 8 8]

  best = 0

for i in 0 : 100
    for j in 0 : (100-i)
      for k in 0 : (100 - (i+j))
        m = 100 - (i+j+k)
        if (i + j + k + m) == 100
          x = [i ; j ; k ; m]
          if sum(cals * x) == 500
      	    temp = reduce((y,z)->y*z, map(x -> max(x,0), A*x))
            best = max(best,temp)
          end
        end
      end
    end
  end

  return best
end

day15()

