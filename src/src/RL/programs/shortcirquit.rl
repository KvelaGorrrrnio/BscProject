/*
  This program should throw an error if short
  cirquiting is not implemented correctrly
*/

init: entry
goto test

test: fi (?q) init count
if (?q || (^q != 1)) count end

count: from test
  n += 10 - #q
  push n q
goto test

end: from test
exit
