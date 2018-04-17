~ Initialisation
init: entry
  w ^= 0
  push w x
  w ^= 8
  push w x
  w ^= 4
  push w x
  w ^= 12
  push w x
  w ^= 2
  push w x
  w ^= 10
  push w x
  w ^= 6
  push w x
  w ^= 14
  push w x
  w ^= 1
  push w x
  w ^= 9
  push w x
  w ^= 5
  push w x
  w ^= 13
  push w x
  w ^= 3
  push w x
  w ^= 11
  push w x
  w ^= 7
  push w x
  w ^= 15
  push w x
  push w s
  pop  w s
goto test
test: fi (baggage = 0) init end
  baggage += 1
if (empty x) end outerloop
outerloop: from test
if ((not (empty s)) && (((top s) > (top x)) || ((top s) = (top x)))) poploop outerloop_cont
poploop: from outerloop
  pop  q s
  push q tmp
goto outerloop
outerloop_cont: from outerloop
if (empty s) then else
then: from outerloop_cont
  tmpv -= 1    ~ no preceding smaller value
  push tmpv res
goto end
else: from outerloop_cont
  tmpv += top s
  push tmpv res
goto end
end: fi ((top res) < 0) then else
  pop tmpv x
  push tmpv s
goto test
