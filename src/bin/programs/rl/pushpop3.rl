init: entry
  x ^= 3
  push x s
  x ^= 5
  push x s
  x ^= 9
  push x s
  x ^= 2
  push x s
  q ^= top s
  pop tmp s
  p ^= top s
exit
