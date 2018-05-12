int n
list int q
list int p
int v

init: entry
  n += 1
  push n q
  push n q
  n += 1
  push n q
  n += 1
  push n q
  n += 1
  push n q
  push n q
  n += 1
  push n q
  push n q
  n += 1
  push n q
  n += 1
  push n q
  push n q
  n += 1
  push n q
goto loop

loop: fi (v = 0) init loop
  pop n q
  v += (2 * n) ** (# p)
  push n p
if (? q) end loop

end: from loop
  .
exit
