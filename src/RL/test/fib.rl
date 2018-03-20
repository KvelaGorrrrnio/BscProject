init: entry ~ entry point
  n ^= 30   ~ find the 30th fib number
  w ^= 1
goto loop ~ start the loop
loop: fi (v = 0) init test
  v += w
  swap v w
  n -= 1
goto test ~ do the test
test: from loop
if (n = 0 || v > w) end loop ~ if not satisfied, continue loop
end:  from test
exit      ~ exit

