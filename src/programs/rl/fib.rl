~ Fibonacci program
~ Written in RL
init: entry ~ Entry
  n ^= 30 ~ 5
  w ^= 1
goto loop
loop: fi (v = 0) init test ~ Loop body
  v += w
  swap v w
  n -= 1
goto test
test: from loop ~ Condition
if (n = 0 || v > w) end loop
end: from test ~ Exit
exit

