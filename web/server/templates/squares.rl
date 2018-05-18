// Computing the n first squares

int n int w
list int q

// Initialisation
start: entry
  n ^= 10
goto loop

// Loop
loop: fi (n = 10) start loop
  w += n * n
  push w q
  n -= 1
if (n = 0) end loop

// End of loop
end: from loop
  .
exit
