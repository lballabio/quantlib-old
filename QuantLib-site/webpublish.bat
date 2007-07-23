
if not %1.==. goto NOTNANDO
set USERNAME=nando
goto DONE

:NOTNANDO
set USERNAME=%1
:DONE


scp -C ../QL.tar.bz2 %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs

pause
