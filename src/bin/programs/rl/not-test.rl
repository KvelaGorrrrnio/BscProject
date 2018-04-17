init: entry
  if (c != 0) init endBefore
endBefore: from init
  a += 1
  if not (c = 0) endBefore end
end: from init
  b += 2
  swap a b
  exit
