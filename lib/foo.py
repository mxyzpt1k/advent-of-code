
def isleap(year):
  return year % 4 == 0 and (year % 400 == 0 or year % 100 != 0)

def test(year):
  print( "%d %d" % (year, isleap(year))) 

test( 1900 )
test( 1992 )
test( 2000 )
test( 2001 )
test( 2023 )
test( 2024 )
test( 2100 )

