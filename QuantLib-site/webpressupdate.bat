
if not %1.==. goto NOTNANDO
set USERNAME=nando
goto DONE

:NOTNANDO
set USERNAME=%1
:DONE

scp -C -r docs.shtml %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs
scp -C -r press/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/press

pause
