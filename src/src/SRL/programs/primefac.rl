init: entry
  v += 2
  n += 315
goto loop1

loop1: fi (empty (p) && v = 2) init loop1
  skip
if (n % v = 0) then2 else2

then2: from loop1
  n /= v
  push v p
  v ^= top (p)
goto contif2

else2: from loop1
  v += 1
goto contif2

contif2: fi (not (empty (p)) && top (p) = v) then2 else2
  skip
if (n = 1) contloop1 loop1

contloop1: from contif2
  v -= top (p)
  n -= 1
exit
