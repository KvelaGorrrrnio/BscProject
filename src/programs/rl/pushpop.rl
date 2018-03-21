~ Computing the n first squares

~ Initialisation
init: entry
  n ^= 20
  goto loop
~ Loop
loop: fi (c = 0) init loop
  ~ compute the square
  w += c * c
  ~ push it
  push w q
  ~ increment
  c += 1
  if (c = n) end loop
~ End of loop
end: from loop
exit
