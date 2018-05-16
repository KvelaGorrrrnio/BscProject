init: entry
goto dup

dup: from init
goto end

dup: from init
goto end

end: from dup
exit
