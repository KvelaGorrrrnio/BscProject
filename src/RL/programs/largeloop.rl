/*
  This program is for benchmarking
  the language when using large
  loops. At this time, it is very
  slow (1.76s @ n=100000).
*/

init : entry
goto loop

loop : fi (n = 0) init loop
  n += 1
if (n = 100000) end loop

end : from loop
exit
