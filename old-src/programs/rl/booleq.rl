init: entry
  skip
if ((a=a) = (b=B)) then else
then: from init
  a += 9
goto end
else: from init
  b += 9
goto end
end: fi (a>0) then else
exit
