     list int lst
list list int lst'
int n int v

init: entry
goto outloop

outloop: fi empty lst' init next
goto loop

loop: fi v=0 outloop loop
  n += v
  v += 2
  push n lst
if v < 10 loop next

next: from loop
  v -= 10
  push lst lst'
if size lst' = 5 end outloop

end: from next
  swap lst'[0][3] lst'[2][0]
  pop lst lst'
  lst'[2][0] += 5 * top lst'[2]
exit
