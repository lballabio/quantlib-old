
if not %1.==. goto NOTNANDO
set USERNAME=nando
goto DONE

:NOTNANDO
set USERNAME=%1
:DONE


scp -p -C -r quep.html %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs
scp -p -C -r quep/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep
rem scp -p -C -r quep/quep3/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep3

pause
