
if not %1.==. goto NOTNANDO
set USERNAME=nando
goto DONE

:NOTNANDO
set USERNAME=%1
:DONE

set RNAME=0.3.1
set TARGETDIR=D:\Data\QL031

scp "%TARGETDIR%/QuantLib-docs/QuantLib-%RNAME%-docs-html-online.tar.gz" nando@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/

pause
