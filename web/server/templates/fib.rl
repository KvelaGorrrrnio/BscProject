// Compute the n'th fibonacci pair

int n
int v
int w

start: entry
  n ^= 16
  w ^= 1
goto loop

loop: fi (v = 0) start loop
  v += w
  swap v w
  n -= 1
if n = 0 end loop

end: from loop
exit
