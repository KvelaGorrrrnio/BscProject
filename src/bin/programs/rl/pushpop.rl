~ Computing the n first squares

~ Initialisation
init: entry
  n ^= 20
goto loop
~ Loop
loop: fi (n = 20) init loop
  w += n * n
  push w q
  n -= 1
if (n = 0) end loop
~ End of loop
end: from loop
exit
