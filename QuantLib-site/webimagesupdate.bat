
if not %1.==. goto NOTNANDO
set USERNAME=nando
goto DONE

:NOTNANDO
set USERNAME=%1
:DONE


scp -C -r images/*.gif %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/images
scp -C -r images/*.jpg %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/images
scp -C -r images/*.png %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/images

pause
