
if not %1.==. goto NOTNANDO
set USERNAME=nando
goto DONE

:NOTNANDO
set USERNAME=%1
:DONE


scp QuantLib-0.3.1-docs-html-online.tar.gz nando@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/

pause
