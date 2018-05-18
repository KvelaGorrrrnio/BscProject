/*
  Fibonacci program
  Written in RL
*/
int n
int v
int w

// Initialisation
init: entry
  n ^= 16
  w ^= 1
goto loop

// The loop body
loop: fi (v = 0) init loop
  v += w
  swap v w
  n -= 1
if n = 0 end loop

// End
end: from loop
exit

// End of program
