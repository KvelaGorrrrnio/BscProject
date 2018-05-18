int n int i
list int q

start: entry
  n += 1
  push n q
  push n q
  n += 1
  push n q
  n += 1
  push n q
  push n q
  push n q
  n += 1
  push n q
goto loop

loop: fi (i = 0) start loop
  n += (2 * q[#q - i - 1]) ** i
  i += 1
if (i = #q) end loop

end: from loop
  i -= #q
exit
