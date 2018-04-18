end: entry
  skip
goto loop

loop: fi (n) loop end
  n += 1
  swap v w
  v -= w
if (v = 0) init loop

init: from loop
  w ^= 1
  n ^= 16
exit
