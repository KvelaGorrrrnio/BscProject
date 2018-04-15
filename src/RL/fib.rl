~ Fibonacci program
~ Written in RL

~ Initialisation
init: entry
  n ^= 16
  w ^= 1
goto loop
~ The loop body
loop: fi (v = 0) init loop
  v += w
  swap v w
  n -= 1
if (n) loop end
~ End
end: from loop ~ Exit
  .
exit

