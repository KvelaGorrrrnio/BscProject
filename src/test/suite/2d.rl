int n
list int q
list list int p

init : entry
  n += 1
  push n q
  push q p
  n += 1
  push n q
  push q p
  n += ^(^p)
exit
