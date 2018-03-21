~ Fibonacci program
~ Written in RL

~ Initialisation
init: entry
  n ^= 16
  w ^= 1
goto loop
~ The loop body
loop: fi not (v != 0) init test
  v += w
  swap v w
  n -= 1
goto test
~ Test if loop should end
test: from loop
if (n = 0 || v > w) end loop
~ End
end: from test ~ Exit
exit

