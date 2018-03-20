~ Fibonacci program
~ Written in RL
init: entry ~ Entry
  n ^= 5 ~ 30
  w ^= 1
goto loop

loop: fi (v = 0) init test
  v += w
  swap v w
  n -= 1
goto test
test: from loop
if (n = 0 || v > w) end loop
end: from test
exit



