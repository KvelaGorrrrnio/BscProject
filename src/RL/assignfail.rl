init: entry
  skip
  goto test
test: fi (c = 0) init loop_body
  skip
  if (c = 10 * (((2 + 1)))) end loop_body
loop_body: from test
  c += 1
  a += (1 + a) * 2
  goto test
end: from test
  swap a c
exit
