init: entry
  c ^= 20
  goto test
test: fi c = 20 init loop_body
  skip
  if c = 0 end loop_body
loop_body: from test
  a += 3
  c -= 1
  goto test
end: from test
  swap a c
exit
