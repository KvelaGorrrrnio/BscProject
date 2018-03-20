~ initialisation
init: entry
  n ^= 30
  w ^= 1
goto loop
~ the loop body
loop: fi (v = 0) init test
  v += w
  swap v w
  n -= 1
goto test
~ test if loop is done
test: from loop
  skip
if (n = 0 || v > w) end loop
~ when loop is done, exit
end:  from test
  skip
exit

