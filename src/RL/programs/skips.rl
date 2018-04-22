init: entry
  skip
  skip
  skip
  skip
  n ^= 0
  skip
  skip
  skip
if (not (not (not (- (- n) != ~ (~ 8))))) then1 else1

then1: from init
  skip
  skip
  skip
goto contif1

else1: from init
  v += 1
goto contif1

contif1: fi (not (n != 0) && a) then1 else1
  skip
if (0) then2 else2

then2: from contif1
  v += 1
goto contif2

else2: from contif1
  skip
  skip
goto contif2

contif2: fi (0) then2 else2
  n += (- (- (~ (~ (~ n)))))
  skip
  skip
  swap v n
  skip
  swap n n
  skip
  skip
goto loop3

loop3: fi (not (not (a = c) && not (0)) || (1 && 0)) contif2 loop3
  n += - (- (~ a))
  n += - (~ a + - (- c))
  n -= (- 8)
  n += 4 * (1 * 0)
  n += - 0
  n += 0 - 93
  n += 93 + 0
  n -= - ((5 + 2) / 1 - - (a - c))
  n += (5 + 2) ** 0
  n += (5 + 2) ** 1
  n += (5 + 2) ** 2
  n += not (a && (1 && 0))
if (# b = 0 || 1) contloop3 loop3

contloop3: from loop3
  push o l
  push l p
  push p q
  push q b
  o += 1
exit
