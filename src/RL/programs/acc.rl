init: entry
  n ^= 10
goto loop

loop: fi (v = 0) init loop
  v += n
  n -= 1
if (n) loop end

end: from loop
  .
exit
