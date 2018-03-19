init: entry
  skip
  goto test
test: fi (c = 0) init loop_body
  skip
  if (c = 10 * (2 + 1)) end loop_body
loop_body: from test
  a += (1 + 2) * 2
  c += 1
  goto test
end: from test
  swap a c
  exit