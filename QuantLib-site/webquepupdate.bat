
if not %1.==. goto NOTNANDO
set USERNAME=nando
goto DONE

:NOTNANDO
set USERNAME=%1
:DONE


C:\cygwin\bin\scp -C -r quep.shtml %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs
C:\cygwin\bin\scp -C -r quep/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep
C:\cygwin\bin\scp -C -r quep/quep001/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep001
C:\cygwin\bin\scp -C -r quep/quep002/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep002
C:\cygwin\bin\scp -C -r quep/quep002/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep003
C:\cygwin\bin\scp -C -r quep/quep005/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep005
C:\cygwin\bin\scp -C -r quep/quep006/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep006
C:\cygwin\bin\scp -C -r quep/quep007/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep007
C:\cygwin\bin\scp -C -r quep/quep011/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep011
C:\cygwin\bin\scp -C -r quep/quep012/*.* %USERNAME%@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs/quep/quep012

pause
