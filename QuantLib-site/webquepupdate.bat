
if not %1.==. goto NOTNANDO
set USERNAME=nando
goto DONE

:NOTNANDO
set USERNAME=%1
:DONE


scp -C -r quep.shtml %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs
scp -C -r quep/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep
rem scp -C -r quep/quep3/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep3

pause
